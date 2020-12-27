import scala_book._

class CandyMachineTest extends UnitSpec {
  "inserting a coin" should "unlock a locked machine if there is candy left" in {
    val x = CandyMachine.simulate(MyList(Coin))
    val r = x.run(CandyMachine(true, 1, 0))
    assert(r._2.locked == false)
  }

  "turning a knob" should "dispense candy and become locked" in {
    val x = CandyMachine.simulate(MyList(Turn))
    val r = x.run(CandyMachine(false, 1, 0))
    assert(r._2.candies == 0)
    assert(r._2.locked == true)
  }

  "combining inserting coin and knob turn" should "work" in {
    val x = CandyMachine.simulate(MyList(Coin, Turn))
    val r = x.run(CandyMachine(true, 1, 0))
    assert(r._2.coins == 1)
    assert(r._2.candies == 0)
    assert(r._2.locked == true)
  }

  "turning the knob on a locked machine" should "do nothing" in {
    val x = CandyMachine.simulate(MyList(Turn))
    val r = x.run(CandyMachine(true, 1, 1))
    assert(r._2.candies == 1)
    assert(r._2.coins == 1)
  }

  "a machine that’s out of candy" should "ignores coins" in {
    val x = CandyMachine.simulate(MyList(Coin))
    val r = x.run(CandyMachine(true, 0, 0))
    assert(r._2.candies == 0)
    assert(r._2.coins == 0)
  }
}
