package scala_book {

  import scala.collection.immutable
  trait Foldable[F[_]] {
    def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = {
      foldMap(as)(f.curried)(Monoid.dual(Monoid.endoMonoid))(z)
    }

    def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

    def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B = {
      foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
    }

    def concatenate[A](as: F[A])(m: Monoid[A]): A =
      foldLeft(as)(m.zero)(m.op)

    def toList[A](fa: F[A]): List[A] = {
      this.foldRight(fa)(List[A]())(_ :: _)
      // this.foldMap(fa)(a => List(a))(Monoid.listConcat[A])
    }
  }

  object Foldables extends Foldable[List] {
    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }

    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = {
      as.foldRight(z)(f)
    }

    override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
      foldLeft(as)(mb.zero)((b, a) => mb.op(f(a), b))
    }
  }

  object StreamFoldable extends Foldable[Stream] {
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }

    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
      as.foldRight(z)(f)
    }
  }

  object IndexedSeqFoldable extends Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
      as.foldLeft(z)(f)
    }

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
      as.foldRight(z)(f)
    }

    override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
      Monoid.foldMapV(as, mb)(f)
  }

  object TreeFoldable extends Foldable[MyTree] {
    override def foldLeft[A, B](as: MyTree[A])(z: B)(f: (B, A) => B): B =
      as match {
        case Leaf(a)      => f(z, a)
        case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
      }

    override def foldRight[A, B](as: MyTree[A])(z: B)(f: (A, B) => B) =
      as match {
        case Leaf(a)      => f(a, z)
        case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
      }

    // Note: not using zero from the monoid
    override def foldMap[A, B](as: MyTree[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
      }
  }

  object OptionFoldable extends Foldable[Option] {
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as match {
        case None    => z
        case Some(a) => f(z, a)
      }

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
      as match {
        case None    => z
        case Some(a) => f(a, z)
      }

    override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
      as match {
        case None    => mb.zero
        case Some(a) => f(a)
      }
  }
}
