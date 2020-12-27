package scala_book {
  sealed trait Input {}

  case object Coin extends Input
  case object Turn extends Input

  // Inserting a coin into a locked machine will cause it to unlock if there’s any candy left.
  // Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
  // Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
  // A machine that’s out of candy ignores all inputs.

  case class CandyMachine(locked: Boolean, candies: Int, coins: Int) {}

  object CandyMachine {
    def simulate(inputs: MyList[Input]): State[CandyMachine, (Int, Int)] = {
      inputs match {
        case Nil => State(s => { ((s.candies, s.coins), s) })
        case Cons(h, t) =>
          h match {
            case Coin =>
              State(s => {
                if (s.locked == false || s.candies <= 0) {
                  simulate(t).run(s)
                } else {
                  simulate(t).run(CandyMachine(false, s.candies, s.coins + 1))
                }
              })
            case Turn =>
              State(s => {
                if (s.locked || s.candies <= 0) {
                  simulate(t).run(s)
                } else {
                  simulate(t).run(CandyMachine(true, s.candies - 1, s.coins))
                }
              })
          }
      }
    }
  }
}
