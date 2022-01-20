import scala_book._

class MyEitherTest extends UnitSpec {
  "flatMap" should "allow to map inner value to new either" in {
    assert(MyRight(3).flatMap(a => MyRight(a * 2)) == MyRight(6))
  }

  "map2" should "allow combining two inner values" in {
    assert(MyRight(2).map2(MyRight(3))((a, b) => a * b) == MyRight(6))
  }
}
