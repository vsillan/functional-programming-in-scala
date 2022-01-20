import scala_book._
import scala.util.matching.Regex

package scala_book {
  trait Parsers[ParseError, Parser[+_]] { self =>
    def run[A](p: Parser[A])(input: String): Either[ParseError, A]

    def char(c: Char): Parser[Char] =
      string(c.toString).map(_.charAt(0))

    implicit def string(s: String): Parser[String]

    implicit def operators[A](p: Parser[A]) = ParserOps[A](p)

    implicit def asStringParser[A](a: A)(implicit
        f: A => Parser[String]
    ): ParserOps[String] = ParserOps(f(a))

    implicit def regex(r: Regex): Parser[String]

    def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

    def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0) succeed(List.empty)
      else p.map2(listOfN(n - 1, p))((a, b) => a :: b)

    def succeed[A](a: A): Parser[A] = string("").map(_ => a)

    def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(r1 => p2.map(r2 => (r1, r2)))

    def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
      a.flatMap(r => succeed(f(r)))

    def map2[A, B, C](p: Parser[A], p2: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] =
      product(p, p2).map(f.tupled)

    def map2ViaFlatMap[A, B, C](p: Parser[A], p2: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] =
      for {
        r1 <- p
        r2 <- p2
      } yield f(r1, r2)

    def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

    def many[A](p: Parser[A]): Parser[List[A]] =
      p.map2(many(p))((a, b) => a :: b).or(succeed(List.empty))

    def many1[A](p: Parser[A]): Parser[List[A]] = p.map2(many(p))((a, b) => b)

    def slice[A](p: Parser[A]): Parser[String]

    case class ParserOps[A](p: Parser[A]) {
      def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
      def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
      def map[B](f: A => B): Parser[B] = self.map(p)(f)
      def map2[B, C](p2: Parser[B])(f: (A, B) => C): Parser[C] =
        self.map2(p, p2)(f)
      def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
      def **[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def product[B >: A](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
      def many(): Parser[List[A]] = self.many(p)
      def slice(): Parser[String] = self.slice(p)
    }

    object Laws {
      def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
        Prop.forAll(in)(s => run(p1)(s) == run(p2)(s))

      // What is the equal used here?
      // def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      //   equal(p, p.map(a => a))(in)
    }

    // Specific parsers
    def contextSensitive[A](p1: Parser[A]) =
      // Test this works
      regex("[0-9]*".r).map(_.toInt).flatMap(listOfN(_, p1))
  }

  trait JSON

  object JSON {
    case object JNull extends JSON
    case class JNumber(get: Double) extends JSON
    case class JString(get: String) extends JSON
    case class JBool(get: Boolean) extends JSON
    case class JArray(get: IndexedSeq[JSON]) extends JSON
    case class JObject(get: Map[String, JSON]) extends JSON
  }

  object JSONParser {
    def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
      import P._
      val spaces = char(' ').many.slice
      succeed(JSON.JNull)
    }
  }
}
