package riddles.twohourglasses

import riddles._

object TwoHourglassState {
  val H1C = 7
  val H2C = 11
}

// One can flip over one or both hourglasses, but only when one of them
// reaches zero.
case class TwoHourglassState(hour1: Int, hour2: Int, parent: Option[TwoHourglassState]) extends RiddleState[TwoHourglassState] {
  import TwoHourglassState.{H1C, H2C}

  def isGoal(): Boolean =
    (hour1 + hour2 == 15 || // can sequentially flip them
     hour1 == 0 && hour2 + H1C == 15 || // let hour2 run out then flip hour1
     hour2 == 0 && hour1 + H2C == 15) // let hour1 run out then flip hour2

  def copyp(hour1: Int = hour1, hour2: Int = hour2): TwoHourglassState =
    copy(hour1 = hour1, hour2 = hour2, parent = Some(this))

  def nextStates(): Seq[TwoHourglassState] = {
    if (hour1 == 0 && hour2 == 0) {
      // flip one or both
      Seq(copyp(hour1 = H1C),
          copyp(hour2 = H2C),
          copyp(hour1 = H1C,
                hour2 = H2C))
    } else if (hour1 == 0) {
      // can either flip hour1 or let hour2 run out
      Seq(copyp(hour1 = H1C),
          copyp(hour2 = 0))
    } else if (hour2 == 0) {
      Seq(copyp(hour2 = H2C),
          copyp(hour1 = 0))
    } else {
      // neither is 0 - let the smaller drain
      if (hour1 < hour2) {
        Seq(copyp(hour1 = 0,
                  hour2 = hour2 - hour1))
      } else {
        Seq(copyp(hour2 = 0,
                  hour1 = hour1 - hour2))
      }
    }
  }
}

object Main {
  def main(args: Array[String]) {
    new RiddleProcessor(
      TwoHourglassState(0, 0, None)).printResults()
  }
}
