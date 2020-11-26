package scala_book {
  sealed trait MyOption[+A] {
    def map[B](f: A => B): MyOption[B] =
      this match {
        case MyNone    => MyNone
        case MySome(a) => MySome(f(a))
      }

    def flatMap[B](f: A => MyOption[B]): MyOption[B] =
      this match {
        case MyNone    => MyNone
        case MySome(a) => f(a)
      }

    def getOrElse[B >: A](default: => B): B =
      this match {
        case MyNone    => default
        case MySome(a) => a
      }

    def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
      this match {
        case MyNone => ob
        case _      => this
      }

    def filter(f: A => Boolean): MyOption[A] =
      this match {
        case MySome(a) => if (f(a)) this else MyNone
        case _         => MyNone
      }
  }

  case object MyNone extends MyOption[Nothing]
  case class MySome[+A](get: A) extends MyOption[A]

  object MyOption {
    def mean(xs: MyList[Double]): MyOption[Double] =
      if (xs == Nil) MyNone
      else MySome(MyList.sum(xs) / xs.length)

    def variance(xs: MyList[Double]): MyOption[Double] = {
      mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def map2[A, B, C](a: MyOption[A], b: MyOption[B])(
        f: (A, B) => C
    ): MyOption[C] = {
      a.flatMap(aa => b.map(bb => f(aa, bb)))
    }

    def traverse[A, B](
        as: MyList[A]
    )(f: A => MyOption[B]): MyOption[MyList[B]] = {
      as.foldRight[MyOption[MyList[B]]](MySome(Nil))((h, t) =>
        map2(f(h), t)(_ :: _)
      )
    }

    def sequence[A](l: MyList[MyOption[A]]): MyOption[MyList[A]] = {
      l match {
        case Nil        => MySome(Nil)
        case Cons(h, t) => h.flatMap(hh => sequence(t).map(hh :: _))
      }
    }
  }
}
