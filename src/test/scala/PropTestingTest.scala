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
}
