import scala_book._
import scala_book.Prop.Falsified
import scala_book.Prop.Passed
import scala_book.Prop.Proved

class MonoidTest extends UnitSpec {
  "monoidLaws" should "work on intMultiplicationMonoid" in {
    val prop =
      Monoid.monoidLaws(Monoid.intMultiplicationMonoid, Gen.choose(-100, 100))
    val result = prop.run(100, 100, TestRNG(10))

    assert(result == Passed)
  }

  "monoidLaws" should "work on intAdditionMonoid" in {
    val prop =
      Monoid.monoidLaws(Monoid.intAdditionMonoid, Gen.choose(-100, 100))
    val result = prop.run(100, 100, TestRNG(10))

    assert(result == Passed)
  }

  "monoidLaws" should "work on productMonoid" in {
    val prop =
      Monoid.monoidLaws(
        Monoid.productMonoid(
          Monoid.intMultiplicationMonoid,
          Monoid.intAdditionMonoid
        ),
        Gen.choose(-100, 100).map(a => (a, a))
      )
    val result = prop.run(100, 100, TestRNG(10))

    assert(result == Passed)
  }

  "bag" should "work" in {
    val bag = Monoid.bag(IndexedSeq("dog", "is", "a", "dog", "is", "a", "dog"))

    assert(bag("is") == 2)
    assert(bag("dog") == 3)
  }

  "WordCounter" should "count words" in {
    val result =
      WordCounter.count("left test test testing right")

    assert(result == 5)
  }
}
