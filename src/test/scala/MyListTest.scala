import scala_book._

class MyListTest extends UnitSpec {
  "Empty list" should "equal Nil" in {
    assert(MyList.empty() == Nil)
  }

  "Map" should "apply function correctly" in {
    assert(MyList.map(MyList(1, 2))(x => x * x) == MyList(1, 4))
  }
}
