package scala_book {
  case class Gen[A](sample: State[RNG, A]) {}

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
  }
}
