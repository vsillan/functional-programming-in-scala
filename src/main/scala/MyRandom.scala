package scala_book {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5deece66dL + 0xbL) & 0xffffffffffffL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object MyRandom {
    type Rand[+A] = State[RNG, A]

    def unit[A](x: A): Rand[A] =
      State(rng => {
        (x, rng)
      })

    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (n, rng2) = rng.nextInt
      if (n == Int.MinValue) {
        (Math.abs(n + 1), rng2)
      } else {
        (Math.abs(n), rng2)
      }
    }

    def double(rng: RNG): (Double, RNG) = {
      val (n, rng2) = nonNegativeInt(rng)
      ((n / (Int.MaxValue.toDouble + 1)), rng2)
    }

    def intDouble(rng: RNG): ((Int, Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (MyList[Int], RNG) = {
      def buildList(
          list: MyList[Int],
          count: Int,
          rng: RNG
      ): (MyList[Int], RNG) = {
        if (count > 0) {
          val (a, rng2) = rng.nextInt
          buildList(a :: list, count - 1, rng2)
        } else {
          (MyList.reverse(list), rng)
        }
      }

      buildList(MyList.empty: MyList[Int], count, rng)
    }

    def intsWithSequence(count: Int)(rng: RNG): (MyList[Int], RNG) = {
      val state = sequence(
        MyList.fill(count)(State((rng2: RNG) => rng2.nextInt))
      )
      state.run(rng)
    }

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
      State((rng: RNG) => {
        val (a, rng2) = s.run(rng)
        (f(a), rng2)
      })

    def doubleWithMap(rng: RNG): Rand[Double] = {
      map(State(nonNegativeInt))(_ / (Int.MaxValue.toDouble + 1))
    }

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      State(rng => {
        val (a, rng2) = ra.run(rng)
        val (b, rng3) = rb.run(rng2)
        (f(a, b), rng3)
      })

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      State(rng => {
        val (a, rng2) = f.run(rng)
        g(a).run(rng2)
      })

    def mapWithFlatmap[A, B](s: Rand[A])(f: A => B): Rand[B] = {
      flatMap(s)(a => unit(f(a)))
    }

    def map2WithFlatmap[A, B, C](ra: Rand[A], rb: Rand[B])(
        f: (A, B) => C
    ): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))

    def sequence[A](fs: MyList[Rand[A]]): Rand[MyList[A]] = {
      fs.foldRight(unit(MyList[A]()))((f, acc) => map2(f, acc)(_ :: _))
    }

    def nonNegativeLessThan(n: Int): Rand[Int] = {
      flatMap(State(nonNegativeInt)) { i =>
        val mod = i % n
        if (i + (n - 1) - mod >= 0) unit(mod)
        else nonNegativeLessThan(n)
      }
    }
  }
}
