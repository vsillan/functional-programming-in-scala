import scala_book._

class MyOptionTest extends UnitSpec {
  "mean with empty list" should "return none" in {
    assert(
      MyOption.mean(MyList()) == MyNone
    )
  }

  "map" should "apply map function with some" in {
    assert(MySome(3).map(x => x * 2) == MySome(6))
  }
}
