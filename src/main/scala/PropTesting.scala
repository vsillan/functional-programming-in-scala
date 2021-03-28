package scala_book {
  case class Gen[A](sample: State[RNG, A]) {
    def flatMap[B](f: A => Gen[B]): Gen[B] = {
      Gen(State(s => {
        val (a2, s2) = sample.run(s)
        f(a2).sample.run(s2)
      }))
    }

    def listOfN(size: Gen[Int]): Gen[MyList[A]] = {
      size.flatMap(a => Gen.listOfN(a, this))
    }
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
