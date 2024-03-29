import scala_book._

class MyListTest extends UnitSpec {
  "empty list" should "equal Nil" in {
    assert(MyList.empty() == Nil)
  }

  "map" should "apply function correctly" in {
    assert(MyList(1, 2).map(x => x * x) == MyList(1, 4))
  }

  "mapViaFoldLeft" should "apply function correctly (reverses the results)" in {
    assert(MyList(1, 2).mapViaFoldLeft(x => x * x) == MyList(4, 1))
  }

  "setHead" should "set the first item" in {
    assert(MyList.setHead(MyList(2, 3), 1) == MyList(1, 3))
  }

  "zipWith" should "work" in {
    assert(
      MyList.zipWith(MyList(1, 2, 3), MyList(2, 4, 6))((a, b) =>
        a * b
      ) == MyList(2, 8, 18)
    )
  }

  "fill" should "create a new list of count with defined value repeating" in {
    assert(MyList.fill(5)(2) == MyList(2, 2, 2, 2, 2))
  }

  "max" should "return the largest integer" in {
    assert(MyList(1, 3, 2).max[Int] == 3)
  }
}
