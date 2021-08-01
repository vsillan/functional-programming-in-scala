import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

package scala_book {
  trait Par[A] {
    def unit[A](a: => A): Par[A]
    def run[A](a: Par[A]): A
    def map2[B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
  }

  object Par {
    type Par[A] = ExecutorService => Future[A]

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true
      def get(timeout: Long, units: TimeUnit) = get
      def isCancelled = false
      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
      (es: ExecutorService) => {
        val af = a(es)
        val bf = b(es)
        UnitFuture(f(af.get, bf.get))
      }

    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
      map2(pa, unit(()))((a, _) => f(a))

    def map3[A, B, C, D](ap: Par[A], bp: Par[B], cp: Par[C])(
        f: (A, B, C) => D
    ): Par[D] = {
      val abp = map2(ap, bp)((a, b) => (a, b))
      map2(cp, abp)((c, ab) => f(ab._1, ab._2, c))
    }

    def fork[A](a: => Par[A]): Par[A] =
      es =>
        es.submit(new Callable[A] {
          def call = a(es).get
        })

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    def asyncF[A, B](f: A => B): A => Par[B] =
      (a: A) => lazyUnit(f(a))

    def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
      ps.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _))
    }

    def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] =
      fork {
        val fbs: List[Par[B]] = ps.map(asyncF(f))
        sequence(fbs)
      }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
      fork {
        val optionListPar = parMap(as)(a => if (f(a)) Some(a) else None)
        map(optionListPar)(optionList =>
          optionList.flatMap[A](o =>
            o match {
              case Some(_) => List(o.get)
              case _       => List()
            }
          )
        )
      }
    }
  }
}
