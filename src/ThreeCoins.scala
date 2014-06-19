package riddles.threecoins
import riddles._

import scala.annotation.tailrec


// TODO: this could generalize to n coins if power set were used
// each state is a command to flip the first, second, and/or third coin
case class CoinRiddleState(flip1: Boolean, flip2: Boolean, flip3: Boolean, parent: Option[CoinRiddleState]) extends RiddleState[CoinRiddleState] {
  def operations(): List[CoinRiddleState] = {
    @tailrec
    def opts(me: CoinRiddleState, accum: List[CoinRiddleState]): List[CoinRiddleState] =
      me.parent match {
        case None => me :: accum
        case Some(p) => opts(p, me :: accum)
      }

    opts(this, List())
  }

  def maybeFlip(c: Boolean, doFlip: Boolean): Boolean =
    if (doFlip) !c else c

  // c1, c2, c3 specify the original configuation.  true is heads and false is tails.
  def isSolutionForOrig(c1: Boolean, c2: Boolean, c3: Boolean): Boolean = {
    @tailrec
    def withConfigAndOpts(c1: Boolean, c2: Boolean, c3: Boolean, opts: List[CoinRiddleState]): Boolean =
      if (c1 == c2 && c2 == c3) true
      else opts match {
        case CoinRiddleState(fc1, fc2, fc3, _) :: tail =>
          withConfigAndOpts(maybeFlip(c1, fc1), maybeFlip(c2, fc2),
                            maybeFlip(c3, fc3), tail)
        case Nil => false
      }

    withConfigAndOpts(c1, c2, c3, operations)
  }

  def isGoal(): Boolean =
    (isSolutionForOrig(true, true, false) &&
     isSolutionForOrig(true, false, true) &&
     isSolutionForOrig(true, false, false) &&
     isSolutionForOrig(false, true, true) &&
     isSolutionForOrig(false, true, false) &&
     isSolutionForOrig(false, false, true))
    
  def pcopy(flip1: Boolean = flip1, flip2: Boolean = flip2, flip3: Boolean = flip3): CoinRiddleState = 
    copy(flip1 = flip1, flip2 = flip2, flip3 = flip3, parent = Some(this))

  def nextStates(): Seq[CoinRiddleState] =
    // ignoring the case where we do nothing, since that will immediately
    // be discarded
    // [[1,2,3], [1,2], [1, 3], [1], [2,3], [2], [3]]
    Seq(pcopy(flip1 = !flip1,
              flip2 = !flip2,
              flip3 = !flip3), // [1,2,3]
        pcopy(flip1 = !flip1,
              flip2 = !flip2), // [1,2]
        pcopy(flip1 = !flip1,
              flip3 = !flip3), // [1,3]
        pcopy(flip1 = !flip1), // [1]
        pcopy(flip2 = !flip2,
              flip3 = !flip3), // [2,3]
        pcopy(flip2 = !flip2), // [2]
        pcopy(flip3 = !flip3)) // [3]
}

object Main {
  def main(args: Array[String]) {
    new RiddleProcessor(
      CoinRiddleState(false, false, false, None)).printResults()
  }
}
