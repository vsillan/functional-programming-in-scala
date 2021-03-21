package scala_book {
  sealed trait MyTree[+A] {
    def size(): Int = {
      this match {
        case Leaf(_)      => 1
        case Branch(l, r) => l.size() + r.size() + 1
      }
    }
    def sizeWithFold(): Int = {
      MyTree.fold(this)(a => 1)(1 + _ + _)
    }
    def depth(): Int = {
      this match {
        case Leaf(_)      => 1
        case Branch(l, r) => l.depth().max(r.depth()) + 1
      }
    }
    def depthWithFold(): Int = {
      MyTree.fold(this)(a => 1)((l, r) => l.max(r) + 1)
    }
    def map[B](f: A => B): MyTree[B] = {
      this match {
        case Leaf(value)  => Leaf(f(value))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
      }
    }
    def mapWithFold[B](f: A => B): MyTree[B] = {
      MyTree.fold(this)(a => Leaf(f(a)): MyTree[B])(Branch(_, _))
    }
  }

  case class Leaf[A](value: A) extends MyTree[A]
  case class Branch[A](left: MyTree[A], right: MyTree[A]) extends MyTree[A]

  object MyTree {
    def maximum(a: MyTree[Int]): Int = {
      a match {
        case Leaf(value)  => value
        case Branch(l, r) => MyTree.maximum(l).max(MyTree.maximum(r))
      }
    }
    def fold[A, B](t: MyTree[A])(f: A => B)(g: (B, B) => B): B = {
      t match {
        case Leaf(a)      => f(a)
        case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
      }
    }
  }
}
