package scala_book {
  sealed trait MyEither[+E, +A] {
    def map[B](f: A => B): MyEither[E, B] = {
      this match {
        case MyRight(a) => MyRight(f(a))
        case MyLeft(e)  => MyLeft(e)
      }
    }
    def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = {
      this match {
        case MyRight(a) => f(a)
        case MyLeft(e)  => MyLeft(e)
      }
    }

    def orElse[EE >: E, AA >: A](b: => MyEither[EE, AA]): MyEither[EE, AA] = {
      this match {
        case MyRight(a) => MyRight(a)
        case MyLeft(e)  => b
      }
    }

    def map2[EE >: E, B, C](
        b: MyEither[EE, B]
    )(f: (A, B) => C): MyEither[EE, C] = {
      this match {
        case MyRight(a) => this.flatMap(aa => b.map(bb => f(aa, bb)))
        case MyLeft(e)  => MyLeft(e)
      }
    }

    def map2Alt[EE >: E, B, C](
        b: MyEither[EE, B]
    )(f: (A, B) => C): MyEither[EE, C] = {
      this match {
        case MyRight(a) => this.flatMap(aa => b.map(bb => f(aa, bb)))
        case MyLeft(e)  => MyLeft(e)
      }
    }
  }
  case class MyLeft[+E](value: E) extends MyEither[E, Nothing]
  case class MyRight[+A](value: A) extends MyEither[Nothing, A]

  object MyEither {
    def sequence[E, A](es: MyList[MyEither[E, A]]): MyEither[E, MyList[A]] = {
      traverse(es)(x => x)
    }

    def traverse[E, A, B](
        as: MyList[A]
    )(f: A => MyEither[E, B]): MyEither[E, MyList[B]] = {
      as match {
        case Nil        => MyRight(Nil)
        case Cons(h, t) => (f(h) map2 traverse(t)(f))(_ :: _)
      }
    }
  }
}
