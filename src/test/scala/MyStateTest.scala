import scala_book._

case class TestState(a1: Int) {
  var a: Int = a1
}

class MyStateTest extends UnitSpec {
  "map" should "apply a function to the value" in {
    val state = State[TestState, Int](s => {
      s.a = s.a + s.a
      (s.a, s)
    }).map(a => a * 2)

    assert(state.run(new TestState(1))._1 == 4)
  }

  "flatMap" should "enable to change the type of the return value" in {
    val state = State[TestState, Int](s => {
      s.a = s.a + s.a
      (s.a, s)
    }).flatMap(a =>
      State[TestState, String](s => {
        (s.a.toString(), s)
      })
    )

    assert(state.run(new TestState(2))._1 == "4")
  }

  "sequence" should "combine many state transitions" in {
    val x = State.sequence(
      MyList(
        State((rng: RNG) => rng.nextInt),
        State((rng: RNG) => rng.nextInt),
        State((rng: RNG) => rng.nextInt)
      )
    )

    assert(x.run(TestRNG(0))._1 == MyList(1, 2, 3))
  }

  "modify" should "enable state modification" in {
    val x = State.modify[TestState](s => TestState(s.a * 2))
    assert(x.run(new TestState(1))._2.a == 2)
  }
}
