import scala_book._

case class TestRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = seed + 1
    return (newSeed.toInt, TestRNG(newSeed))
  }
}

class MyRandomTest extends UnitSpec {
  "ints" should "generate a list with certain amount of items using RNG" in {
    assert(MyRandom.ints(3)(TestRNG(0))._1 == (MyList(1, 2, 3)))
  }

  "intsWithSequence" should "generate a list with certain amount of items using RNG" in {
    assert(MyRandom.intsWithSequence(3)(TestRNG(0))._1 == (MyList(1, 2, 3)))
  }

  "map2" should "combine two state transitions" in {
    val x =
      MyRandom.map2(rng => rng.nextInt, rng => rng.nextInt)((a, b) => a * b)
    assert(
      x(TestRNG(0))._1 == 2
    )
  }

  "sequence" should "combine many state transitions" in {
    val x = MyRandom.sequence(
      MyList(
        (rng: RNG) => rng.nextInt,
        (rng: RNG) => rng.nextInt,
        (rng: RNG) => rng.nextInt
      )
    )

    assert(x(TestRNG(0))._1 == MyList(1, 2, 3))
  }

  "nonNegativeLessThan" should "give smaller positive value than the defined value" in {
    val x = MyRandom.nonNegativeLessThan(3)
    val r = x(TestRNG(4))._1
    assert(r < 3 && r >= 0)
  }
}
