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

    def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

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

    def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
      p(e).get == p2(e).get

    def sumOfWords[A, B](ints: List[String]): Par[Int] = {
      def wordsInString(s: String): Int = {
        s.split(" ").length
      }
      applyParallel(ints, 0)(wordsInString)(_ + _)
    }

    def applyParallel[A, B](
        li: List[A],
        z: B
    )(f: A => B)(g: (B, B) => B): Par[B] = {
      if (li.length == 0)
        Par.unit(z)
      else if (li.length == 1)
        Par.fork(Par.unit(f(li.head)))
      else {
        val (l, r) = li.splitAt(li.length / 2)
        Par.map2(applyParallel(l, z)(f)(g), applyParallel(r, z)(f)(g))(g)
      }
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      es => if (run(es)(cond).get) t(es) else f(es)

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = { es =>
      {
        val x = run(es)(n).get
        run(es)(choices(x))
      }
    }

    def choiceViaN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
      choiceN(Par.map(cond)(x => if (x == true) 1 else 0))(List(t, f))

    def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = { es =>
      {
        val x = run(es)(key).get
        run(es)(choices(x))
      }
    }

    def genericChoice[A, B, C](key: Par[B])(
        choices: C
    )(extractor: (B, C) => Par[A]): Par[A] = { es =>
      {
        val x = run(es)(key).get
        run(es)(extractor(x, choices))
      }
    }

    def choiceViaGenericChoice[A](
        cond: Par[Boolean]
    )(t: Par[A], f: Par[A]): Par[A] =
      genericChoice(cond)(List(t, f))((b, l) => if (b) l(0) else l(1))

    // Usually called bind or flatMap in functional libraries
    def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = { es =>
      {
        val x = run(es)(pa).get
        run(es)(f(x))
      }
    }

    def join[A](a: Par[Par[A]]): Par[A] = es => run(es)(run(es)(a).get)

    def flatMapViaJoin[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
      join(map(pa)(f))

    def joinViaFlatMap[A](a: Par[Par[A]]): Par[A] =
      flatMap(a)(x => x)
  }
}
