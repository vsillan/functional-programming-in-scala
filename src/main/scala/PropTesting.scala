package scala_book {
  import scala_book.Prop.Passed
  import scala_book.Prop.Proved
  import scala_book.Prop.Falsified
  import java.util.concurrent.{Executors}

  //
  // Prop
  //
  case class Prop(run: (Prop.Max, Prop.TestCases, RNG) => Prop.Result) {
    def &&(p: Prop): Prop = {
      Prop((max, testCases, rng) => {
        this.run(max, testCases, rng) match {
          case Passed => p.run(max, testCases, rng)
          case Proved => p.run(max, testCases, rng)
          case x      => x
        }
      })
    }

    def ||(p: Prop): Prop = {
      Prop((max, testCases, rng) => {
        this.run(max, testCases, rng) match {
          case Passed =>
            p.run(max, testCases, rng) match {
              case Passed                        => Passed
              case Proved                        => Passed
              case Falsified(failure, successes) => Passed
            }
          case Falsified(failure, successes) =>
            p.tag(failure).run(max, testCases, rng)
        }
      })
    }

    def tag(msg: String) =
      Prop((max, n, rng) =>
        run(max, n, rng) match {
          case Falsified(e, sc) => Falsified(msg + ", " + e, sc)
          case x                => x
        }
      )
  }

  object Prop {
    type Max = Int
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

    case object Proved extends Result {
      def isFalsified = false
    }

    def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

    def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop =
      Prop { (max, n, rng) =>
        val casesPerSize = (n + (max - 1)) / max
        val props: Stream[Prop] =
          Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

        val prop = props
          .map(p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
            }
          )
          .toList
          .reduce(_ && _)

        prop.run(max, n, rng)
      }

    def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
      Prop((max, n, rng) =>
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

    def check(p: => Boolean): Prop =
      Prop { (_, _, _) => if (p) Proved else Falsified("()", 0) }

    val executorGen = Gen.weighted(
      Gen.choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
      Gen.unit(Executors.newCachedThreadPool) -> .25
    )

    def forAllPar[A](g: Gen[A])(f: A => Par.Par[Boolean]): Prop =
      forAll(executorGen ** g) { case (s, a) => f(a)(s).get }

    def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
      Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

    def buildMsg[A](s: A, e: Exception): String =
      s"test case: $s\n" +
        s"generated an exception: ${e.getMessage}\n" + s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

    def run(
        p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis)
    ) =
      p.run(maxSize, testCases, rng) match {
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed =>
          println(s"+ OK, passed $testCases tests.")
        case Proved =>
          println(s"+ OK, proved property.")
      }

    def runRaw(
        p: Prop,
        maxSize: Int = 100,
        testCases: Int = 100,
        rng: RNG = SimpleRNG(System.currentTimeMillis)
    ) =
      p.run(maxSize, testCases, rng)
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

  object SGen {
    def listOf[A](g: Gen[A]): SGen[MyList[A]] = {
      SGen(i => Gen.listOfN(i, g))
    }
  }

  //
  // Gen
  //
  case class Gen[A](sample: State[RNG, A]) {
    def **[B](g: Gen[B]): Gen[(A, B)] = (this map2 g)((_, _))

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

    def map2[B, C](g: Gen[B])(f: (A, B) => C): Gen[C] =
      Gen(sample.map2(g.sample)(f))

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

    def listOf1[A](g: Gen[A]): Gen[MyList[A]] = {
      Gen.listOfN(1, g)
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
