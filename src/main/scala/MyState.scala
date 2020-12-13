package scala_book {
  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] = {
      State((s: S) => {
        val (a, s2) = run(s)
        (f(a), s2)
      })
    }

    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] = {
      State(rng => {
        val (r1, rng1) = run(rng)
        val (r2, rng2) = rb.run(rng1)
        (f(r1, r2), rng2)
      })
    }

    def flatMap[B](g: A => State[S, B]): State[S, B] = {
      State((s: S) => {
        val (a, s2) = run(s)
        g(a).run(s2)
      })
    }
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: MyList[State[S, A]]): State[S, MyList[A]] = {
      fs.foldRight(unit[S, MyList[A]](MyList()))((f, acc) =>
        f.map2(acc)(_ :: _)
      )
    }

    def modify[S](f: S => S): State[S, Unit] =
      for {
        s <- get
        _ <- set(f(s))
      } yield ()

    def get[S]: State[S, S] = State(s => (s, s))

    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  }
}
