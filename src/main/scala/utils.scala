object MyModule {

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = { (a) => (b) =>
    f(a, b)
  }

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = { (a, b) =>
    f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = { (a) =>
    f(g(a))
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(cur: Int, next: Int, i: Int): Int = {
      if (i <= 0) cur
      else go(next, cur + next, i - 1)
    }
    go(0, 1, n)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean = {
      var next = n + 1
      if (n + 1 >= as.length) true
      else if (!ordered(as(n), as(next))) false
      else loop(n + 1)
    }

    loop(0)
  }

  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  def main(args: Array[String]): Unit = println(formatAbs(-42))
}
