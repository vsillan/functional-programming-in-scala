import scala_book._

class PropTestingTest extends UnitSpec {
  "Gen.choose" should "return a sampler which adheres to the passed range" in {
    val g = Gen.choose(-10, 10)
    val (x, s) = g.sample.run(TestRNG(10))
    assert(x == 1)
  }

  "Gen.unit" should "return a sampler which always returns the passed value" in {
    val g = Gen.unit(63)
    val (x, s) = g.sample.run(TestRNG(10))
    assert(x == 63)
  }

  "Gen.boolean" should "return a sampler which returns booleans" in {
    val (x1, s1) = Gen.boolean.sample.run(TestRNG(10))
    assert(x1 == false)

    val (x2, s2) = Gen.boolean.sample.run(s1)
    assert(x2 == true)
  }

  "Gen.listOfN" should "return a sampler which returns a list generated with gen" in {
    val g1 = Gen.choose(0, 4)
    val g2 = Gen.listOfN(3, g1)
    val (x, s) = g2.sample.run(TestRNG(0))
    assert(x == MyList(3, 2, 1))
  }

  "Gen.flatMap" should "combine gens" in {
    val g = Gen.choose(0, 10)
    val randomLengthListGen = g.flatMap(a => Gen.listOfN(a, Gen.choose(0, 10)))
    val (x, s) = randomLengthListGen.sample.run(TestRNG(2))
    assert(x.length() == 3)
  }

  "Gen.listOfN" should "uses other gen for the size" in {
    val g = Gen.choose(0, 10)
    val randomLengthListGen = g.listOfN(g)
    val (x, s) = randomLengthListGen.sample.run(TestRNG(2))
    assert(x.length() == 3)
  }

  "Gen.union" should "has equal chance of using either Gen" in {
    val g1 = Gen.choose(1, 10)
    val g2 = Gen.choose(-10, -1)
    val union = Gen.union(g1, g2)
    val (x, s) = union.sample.run(TestRNG(1))
    assert(x > 0)
  }

  "Gen.weighted" should "use weight to determine gen to use" in {
    val g1 = Gen.choose(1, 10)
    val g2 = Gen.choose(-10, -1)
    val union = Gen.weighted((g1, 0.3), (g2, 0.7))
    val (x, s) = union.sample.run(TestRNG(Int.MaxValue - 1))
    assert(x < 0)
  }
}
