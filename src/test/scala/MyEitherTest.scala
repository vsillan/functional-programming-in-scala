import scala_book._

class MyEitherTest extends UnitSpec {
  "flatMap" should "allow to map inner value to new either" in {
    assert(Right(3).flatMap(a => Right(a * 2)) == Right(6))
  }

  "map2" should "allow combining two inner values" in {
    assert(Right(2).map2(Right(3))((a, b) => a * b) == Right(6))
  }
}
