import scala_book._
import scala_book.Prop.Falsified
import scala_book.Prop.Passed

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

  "Prop &&" should "work" in {
    val p1 = Prop.forAll(Gen.choose(1, 2))(a => a == 1)
    val p2 = Prop.forAll(Gen.choose(1, 2))(a => a != 1)
    val x = (p1 && p2).run(2, 1, TestRNG(1))
    assert(x.isFalsified == true)
  }

  "Prop ||" should "work" in {
    val p1 = Prop.forAll(Gen.choose(1, 2))(a => a == 1)
    val p2 = Prop.forAll(Gen.choose(1, 2))(a => a != 1)
    val r = (p1 || p2).run(2, 1, TestRNG(1))
    assert(r.isFalsified == false)

    r match {
      case Falsified(failure, successes) =>
        assert(failure == "")
      case y => y
    }
  }

  "Prop ||" should "write an error message about both failed cases" in {
    val p1 = Prop.forAll(Gen.choose(1, 3))(a => a == 1)
    val p2 = Prop.forAll(Gen.choose(1, 3))(a => a == 1)
    val r = (p1 || p2).run(2, 2, TestRNG(1))
    r match {
      case Falsified(failure, successes) =>
        assert(failure == "2, 2")
      case y => assert(false)
    }
  }

  "SGen" should "listOf creates list with size of the passed length" in {
    val x = SGen.listOf(Gen.choose(1, 3))
    val (r, s) = x.forSize(2).sample.run(TestRNG(1))
    assert(r == MyList(2, 1))
  }

  "MyList max" should "work with a prop test" in {
    val smallInt = Gen.choose(-10, 10)
    val x = Prop.forAll(SGen.listOf(smallInt)) { ns =>
      val max = ns.max[Int]
      !ns.exists(_ > max)
    }

    x.run(100, 100, TestRNG(1)) match {
      case Passed => assert(true)
      case _      => assert(false)
    }
  }

  "listOf1" should "create list with length of 1" in {
    val x = Gen.listOf1(Gen.choose(1, 2))
    val (r, s) = x.sample.run(TestRNG(1))

    assert(r.length() == 1)
  }

  "List sort" should "work with a prop test" in {
    val x = Gen.listOfN(10, Gen.choose(1, 10))

    val prop = Prop.forAll(x) { y =>
      val sorted = MyList.sorted(y)
      val zipped = MyList.zipWith(sorted, MyList.tail(sorted))((a, b) => a >= b)

      !zipped.exists(x => x == false)
    }

    prop.run(100, 100, SimpleRNG(1)) match {
      case Passed => assert(true)
      case _      => assert(false)
    }
  }
}
