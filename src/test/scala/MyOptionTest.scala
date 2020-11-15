import scala_book._

class MyOptionTest extends UnitSpec {
  "mean with empty list" should "return none" in {
    assert(
      MyOption.mean(MyList()) == MyNone
    )
  }
}
