package scala_book {
  import scala_book.Prop.Passed
  import scala_book.Prop.Falsified

  //
  // Prop
  //
  case class Prop(run: (Prop.TestCases, RNG) => Prop.Result) {
    def &&(p: Prop): Prop = {
      Prop((testCases, rng) => {
        this.run(testCases, rng) match {
          case Passed => p.run(testCases, rng)
          case x      => x
        }
      })
    }

    def ||(p: Prop): Prop = {
      Prop((testCases, rng) => {
        this.run(testCases, rng) match {
          case Passed =>
            p.run(testCases, rng) match {
              case Passed                        => Passed
              case Falsified(failure, successes) => Passed
            }
          case Falsified(failure, successes) =>
            p.tag(failure).run(testCases, rng)
        }
      })
    }

    def tag(msg: String) =
      Prop((n, rng) =>
        run(n, rng) match {
          case Falsified(e, sc) => Falsified(msg + ", " + e, sc)
          case x                => x
        }
      )
  }

  object Prop {
    type TestCases = Int
    type FailedCase = String
    type SuccessCount = Int

    sealed trait Result {
      def isFalsified: Boolean
    }
    case object Passed extends Result {
      def isFalsified = false
    }

    case class Falsified(failure: FailedCase, successes: SuccessCount)
        extends Result { def isFalsified = true }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
      Prop((n, rng) =>
        randomStream(as)(rng)
          .zip(Stream.from(0))
          .take(n)
          .map {
            case (a, i) =>
              try {
                if (f(a)) Passed else Falsified(a.toString, i)
              } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
          }
          .find(_.isFalsified)
          .getOrElse(Passed)
      )

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
  }

  //
  // SGen (Sized generator)
  //

  case class SGen[A](forSize: Int => Gen[A]) {
    def flatMap[B](f: A => Gen[B]): SGen[B] = {
      SGen(i => this.forSize(i).flatMap(f))
    }

    def map[B](f: A => B): SGen[B] = {
      SGen(i => this.forSize(i).map(f))
    }

    def apply(n: Int): Gen[A] = forSize(n)
  }

  //
  // Gen
  //
  case class Gen[A](sample: State[RNG, A]) {

    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(State(s => {
        val (a2, s2) = sample.run(s)
        f(a2).sample.run(s2)
      }))
    }

    def map[B](f: A => B): Gen[B] = {
      Gen(State(s => {
        val (a2, s2) = sample.run(s)
        (f(a2), s2)
      }))
    }

    def listOfN(size: Gen[Int]): Gen[MyList[A]] = {
      size.flatMap(a => Gen.listOfN(a, this))
    }

    def unsized: SGen[A] = SGen(_ => this)
  }

  object Gen {
    def choose(start: Int, stopExclusive: Int): Gen[Int] = {
      Gen(
        State((s: RNG) => MyRandom.nonNegativeInt(s)).map(n =>
          start + n % (stopExclusive - start)
        )
      )
    }

    def unit[A](a: => A): Gen[A] = Gen(State(rng => (a, rng)))

    def boolean: Gen[Boolean] =
      Gen(State((s: RNG) => MyRandom.nonNegativeInt(s)).map(n => n % 2 == 0))

    def listOfN[A](n: Int, g: Gen[A]): Gen[MyList[A]] = {
      def build(n: Int, l: MyList[Gen[A]]): MyList[Gen[A]] = {
        if (n == 0) return l
        build(n - 1, g :: l)
      }
      val gens = build(n, MyList.empty())
      return Gen(
        State((s: RNG) =>
          gens.foldRight((MyList.empty(): MyList[A], s))((a, b) => {
            val (currentList, s1) = b
            val (newValue, s2) = a.sample.run(s1)
            (newValue :: currentList, s2)
          })
        )
      )
    }

    def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
      Gen(
        State((s: RNG) => {
          val (a, s2) = MyRandom.boolean(s)
          if (a) g1.sample.run(s2) else g2.sample.run(s2)
        })
      )
    }

    def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
      Gen(
        State((s: RNG) => {
          val (g1Gen, g1Weight) = g1
          val (g2Gen, g2Weight) = g2
          val totalWeight = g1Weight + g2Weight
          val (a, s2) = MyRandom.double(s)
          val mod = a % totalWeight
          if (a <= g1Weight) g1Gen.sample.run(s2) else g2Gen.sample.run(s2)
        })
      )
    }
  }
}
