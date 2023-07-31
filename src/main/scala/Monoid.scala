package scala_book {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  object Monoid {
    val intAdditionMonoid =
      new Monoid[Int] {
        def op(a1: Int, a2: Int) = a1 + a2
        val zero = 0
      }

    val intMultiplicationMonoid = new Monoid[Int] {
      def op(a1: Int, a2: Int) = a1 * a2
      val zero = 1
    }

    val booleanOr = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean) = a1 || a2
      val zero = false
    }

    val booleanAnd = new Monoid[Boolean] {
      def op(a1: Boolean, a2: Boolean) = a1 && a2
      val zero = true
    }

    def listConcat[A] =
      new Monoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1.concat((a2))
        val zero = List.empty
      }

    def optionMonoid[A] =
      new Monoid[Option[A]] {
        def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
        val zero = None
      }

    // Endo means having same parameter and return type
    def endoMonoid[A]: Monoid[A => A] =
      new Monoid[A => A] {
        def op(a1: A => A, a2: A => A) = a1 compose a2
        val zero = a => a
      }

    def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
      Prop.forAll(gen)(a => m.op(a, m.zero) == a) && Prop.forAll(gen ** gen)(
        a => m.op(m.op(a._1, a._2), a._1) == m.op(a._1, m.op(a._2, a._1))
      )
    }

    def dual[A](m: Monoid[A]): Monoid[A] = {
      new Monoid[A] {
        def op(a1: A, a2: A): A = m.op(a2, a1)
        val zero: A = m.zero
      }
    }

    def concatenate[A](as: List[A], m: Monoid[A]): A = {
      as.foldLeft(m.zero)(m.op)
    }

    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
      as.foldLeft(m.zero)((b, a) => m.op(f(a), b))
    }

    def foldRightViaFoldMap[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldMap(as, endoMonoid[B])(f.curried)(z)

    def foldLeftViaFoldMap[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      foldMap(as, dual(endoMonoid[B]))(a => b => f(b, a))(z)
    }

    def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
      if (as.length == 0)
        m.zero
      else if (as.length == 1)
        f(as(0))
      else {
        val (l, r) = as.splitAt(as.length / 2)
        m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
      }
    }

    def productMonoid[A, B](a: Monoid[A], b: Monoid[B]): Monoid[(A, B)] =
      new Monoid[(A, B)] {
        def op(a1: (A, B), a2: (A, B)): (A, B) =
          (a.op(a1._1, a2._1), b.op(a1._2, a2._2))

        val zero = (a.zero, b.zero)
      }

    def functionMonoid[A, B](m: Monoid[B]): Monoid[A => B] =
      new Monoid[A => B] {
        def op(f: A => B, g: A => B): A => B = a => m.op(f(a), g(a))
        val zero = a => m.zero
      }

    def bagMonoid[A] =
      new Monoid[Map[A, Int]] {
        def op(a: Map[A, Int], b: Map[A, Int]): Map[A, Int] =
          (a.keySet ++ b.keySet).foldLeft(zero)((acc, key) =>
            acc.updated(key, a.getOrElse(key, 0) + b.getOrElse(key, 0))
          )

        def zero: Map[A, Int] = Map[A, Int]()
      }

    def bag[A](as: IndexedSeq[A]): Map[A, Int] =
      IndexedSeqFoldable.foldMap(as)(a => Map((a -> 1)))(bagMonoid)
  }

  object WordCounter {
    sealed trait WC

    case class Stub(chars: String) extends WC
    case class Part(lStub: String, words: Int, rStub: String) extends WC

    val wcMonoid: Monoid[WC] = new Monoid[WC] {
      def op(x: WC, y: WC) =
        (x, y) match {
          case (Stub(xChars), Stub(yChars)) => Stub(xChars + yChars)
          case (Stub(xChars), Part(yLStub, yWords, yRStub)) =>
            Part(xChars + yLStub, yWords, yRStub)
          case (Part(xLStub, xWords, xRStub), Stub(yChars)) =>
            Part(xLStub, xWords, xRStub + yChars)
          case (Part(xLStub, xWords, xRStub), Part(yLStub, yWords, yRStub)) =>
            Part(
              xLStub,
              xWords + yWords + (if (yLStub != "" || xRStub != "") 1
                                 else 0),
              yRStub
            )
        }

      val zero = Stub("")
    }

    def count(s: String): Int = {
      def wc(c: Char): WC =
        if (c.isWhitespace)
          Part("", 0, "")
        else
          Stub(c.toString)
      def unstub(s: String) = s.length min 1

      Monoid.foldMapV(s.toIndexedSeq, wcMonoid)(wc) match {
        case Stub(s)       => unstub(s)
        case Part(l, w, r) => unstub(l) + w + unstub(r)
      }
    }
  }
}
