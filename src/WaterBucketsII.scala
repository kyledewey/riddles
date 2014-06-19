package riddles.waterbucketsii

import riddles._

sealed abstract class Bucket[T <: Bucket[T]](val capacity: Int, cons: Int => T) {
  def amount: Int
  def addAmount(add: Int): T = {
    val res = amount + add
    assert(res <= capacity)
    cons(res)
  }
  def removeAmount(rem: Int): T = {
    val res = amount - rem
    assert(res >= 0)
    cons(res)
  }
}
case class Bucket5(amount: Int) extends Bucket[Bucket5](5, (Bucket5))
case class Bucket8(amount: Int) extends Bucket[Bucket8](8, (Bucket8))
case class Bucket12(amount: Int) extends Bucket[Bucket12](12, (Bucket12))

// 12 gallon is initially full, other two empty
// goal: 2/3 buckets have 6 gallons of water.  Third bucket doesn't have a high
// enough capacity for this...
class WaterBucketsIIState(val b1: Bucket5, val b2: Bucket8, val b3: Bucket12, val parent: Option[WaterBucketsIIState]) extends RiddleSeparateToStringState[WaterBucketsIIState] {
  def me = this

  override def hashCode(): Int =
    b1.hashCode + b2.hashCode + b3.hashCode

  override def equals(other: Any): Boolean =
    other match {
      case o: WaterBucketsIIState =>
        b1 == o.b1 && b2 == o.b2 && b3 == o.b3
      case _ => false
    }

  def innerToString(): String =
    Seq(b1, b2, b3).mkString(", ")

  def copy(b1: Bucket5 = b1, b2: Bucket8 = b2, b3: Bucket12 = b3, parent: Option[WaterBucketsIIState] = parent) =
    new WaterBucketsIIState(b1, b2, b3, parent)

  def isGoal(): Boolean =
    b2.amount == 6 && b3.amount == 6

  // returns None if a transfer couldn't be performed (source empty or
  // destination full)
  def transferBetween[A <: Bucket[A], B <: Bucket[B]](src: A, dest: B): Option[(A, B)] = {
    val amountCanTransfer = src.amount
    val amountCanReceive = dest.capacity - dest.amount
    if (amountCanTransfer == 0 || amountCanReceive == 0)
      None
    else {
      val amountToTransfer = math.min(amountCanTransfer, amountCanReceive)
      Some((src.removeAmount(amountToTransfer),
            dest.addAmount(amountToTransfer)))
    }
  }

  def withBucket[T <: Bucket[T]](b: T): WaterBucketsIIState =
    b match {
      case b: Bucket5 => copy(b1 = b)
      case b: Bucket8 => copy(b2 = b)
      case b: Bucket12 => copy(b3 = b)
    }

  // basic idea: transfer water from one bucket into another
  def nextStates(): Seq[WaterBucketsIIState] = {
    def trans[A <: Bucket[A], B <: Bucket[B]](b1: A, b2: B): Option[WaterBucketsIIState] =
      transferBetween(b1, b2).map(
        { case (b1, b2) => withBucket(b1).withBucket(b2).copy(parent = Some(this)) })

    Seq(trans(b1, b2), trans(b2, b1),
        trans(b1, b3), trans(b3, b1),
        trans(b2, b3), trans(b3, b2)).flatMap(s => s)
  }
}

object Main {
  def main(args: Array[String]) {
    new RiddleProcessor(
      new WaterBucketsIIState(Bucket5(0), Bucket8(0), Bucket12(12), None)).printResults()
  }
}


