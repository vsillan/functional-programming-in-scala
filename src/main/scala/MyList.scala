package scala_book {
  sealed trait MyList[+A] {
    //not stack-safe because it uses foldRight
    def map[B](f: A => B): MyList[B] = {
      MyList.foldRight(this, Nil: MyList[B])((h, t) => Cons(f(h), t))
    }
  }

  case object Nil extends MyList[Nothing]
  case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

  object MyList {
    def apply[A](as: A*): MyList[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    @annotation.tailrec
    def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil        => z
        case Cons(h, t) => foldLeft(t, f(z, h))(f)
      }
    }

    def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil        => z
        case Cons(h, t) => f(h, foldRight(t, z)(f))
      }

    def turnToString(as: MyList[Double]) = {
      as.map(d => d.toString)
    }

    def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = {
      // foldRight(as, Nil: MyList[B])((h, t) => concatenateLists(MyList(f(h), t))) // alternative implementation
      concatenateLists(as.map(f))
    }

    def zipWithIntegerAddition(l1: MyList[Int], l2: MyList[Int]): MyList[Int] =
      (l1, l2) match {
        case (_, Nil) => Nil
        case (Nil, _) => Nil
        case (Cons(h, t), Cons(h2, t2)) =>
          Cons(h + h2, zipWithIntegerAddition(t, t2))
      }

    def zipWith[A, B, C](l1: MyList[A], l2: MyList[B])(
        f: (A, B) => C
    ): MyList[C] =
      (l1, l2) match {
        case (_, Nil)                   => Nil
        case (Nil, _)                   => Nil
        case (Cons(h, t), Cons(h2, t2)) => Cons(f(h, h2), zipWith(t, t2)(f))
      }

    def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
      foldRight(as, Nil: MyList[A])((a, b) => if (f(a)) Cons(a, b) else b)
    }

    def filterWithFlatmap[A](as: MyList[A])(f: A => Boolean): MyList[A] = {
      flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)
    }

    def addOneToElement(l: MyList[Int]): MyList[Int] = {
      foldRight(l, Nil: MyList[Int])((h, t) => Cons(h + 1, t))
    }

    def append[A](as: MyList[A], n: A): MyList[A] = {
      foldRight(as, Cons(n, Nil))((a, b) => Cons(a, b))
    }

    def concatenateLists[A](ls: MyList[MyList[A]]): MyList[A] = {
      foldRight(ls, Nil: MyList[A])((a, b) =>
        foldRight(a, b)((a, b) => Cons(a, b))
      )
    }

    def reverse[A](as: MyList[A]): MyList[A] = {
      foldLeft(as, Nil: MyList[A])((b, a) => Cons(a, b))
    }

    def sum3(ns: MyList[Int]) =
      foldLeft(ns, 0)(_ + _)

    def product3(ns: MyList[Double]) =
      foldLeft(ns, 1.0)(_ * _)

    def length2[A](as: MyList[A]): Int = {
      foldLeft(as, 0)((a, b) => a + 1)
    }

    def length[A](as: MyList[A]): Int = {
      foldRight(as, 0)((a, b) => b + 1)
    }

    def sum2(ns: MyList[Int]) =
      foldRight(ns, 0)(_ + _)

    def product2(ns: MyList[Double]) =
      foldRight(ns, 1.0)(_ * _)

    def sum(ints: MyList[Int]): Int =
      ints match {
        case Nil         => 0
        case Cons(x, xs) => x + sum(xs)
      }

    def product(ds: MyList[Double]): Double =
      ds match {
        case Nil         => 1.0
        case Cons(x, xs) => x * product(xs)
      }

    def tail[A](l: MyList[A]): MyList[A] =
      l match {
        case Nil        => sys.error("tail of empty MyList")
        case Cons(_, t) => t
      }

    def drop[A](l: MyList[A], n: Int): MyList[A] = {
      if (n == 0) l
      else
        l match {
          case Nil        => l
          case Cons(_, t) => drop(t, n - 1)
        }
    }

    def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] =
      l match {
        case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
        case _          => l
      }

    def setHead[A](l: MyList[A], nh: A): MyList[A] =
      l match {
        case Nil        => sys.error("set head for empty MyList")
        case Cons(_, t) => Cons(nh, t)
      }

    def init[A](l: MyList[A]): MyList[A] =
      l match {
        case Cons(h, Nil) => Nil
        case Cons(h, t)   => Cons(h, init(t))
        case Nil          => l
      }

    def empty() = MyList()
  }
}
