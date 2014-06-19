package riddles.waterbuckets
import riddles._

case class Bucket(amount: Int, capacity: Int) {
  def empty() = copy(amount = 0)
  def fill() = copy(amount = capacity)
  def add(amt: Int) = {
    assert(amount + amt <= capacity)
    copy(amount = amount + amt)
  }
  def remove(amt: Int) = {
    assert(amount - amt >= 0)
    copy(amount = amount - amt)
  }
}

case class WaterBuckets(b1: Bucket, b2: Bucket, parent: Option[WaterBuckets]) extends RiddleState[WaterBuckets] {
  def copyp(b1: Bucket = b1, b2: Bucket = b2) =
    copy(b1 = b1, b2 = b2, parent = Some(this))

  def isGoal() = 
    b2.amount == 4

  def transfer(b1: Bucket, b2: Bucket): Option[(Bucket, Bucket)] = {
    val canSend = b1.amount
    val canReceive = b2.capacity - b2.amount
    if (canSend == 0 || canReceive == 0) None
    else {
      val toSend = math.min(canSend, canReceive)
      Some((b1.remove(toSend), b2.add(toSend)))
    }
  }

  def nextStates() = {
    val b1Tob2 =
      transfer(b1, b2).map( { case (b1, b2) => copyp(b1 = b1, b2 = b2) })
    val b2Tob1 =
      transfer(b2, b1).map( { case (b2, b1) => copyp(b1 = b1, b2 = b2) })

    Seq(copyp(b1 = b1.fill), copyp(b2 = b2.fill),
        copyp(b1 = b1.empty), copyp(b2 = b2.empty)
      ) ++ b1Tob2 ++ b2Tob1
  }
}

object Main {
  def main(args: Array[String]) {
    new RiddleProcessor(
      WaterBuckets(Bucket(0, 3), Bucket(0, 5), None)).printResults()
  }
}
