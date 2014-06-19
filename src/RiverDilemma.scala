package riddles.riverdilemma
import riddles._

sealed trait Item
case object Duck extends Item
case object Wolf extends Item
case object Seeds extends Item
case object Person extends Item

case class Side(items: Set[Item]) {
  def isInvalid(): Boolean =
    (!items(Person) && ((items(Duck) && items(Seeds)) ||
                        (items(Wolf) && items(Duck))))

  def isValid(): Boolean = !isInvalid
}

class RiverState(val left: Side, val right: Side, val parent: Option[RiverState]) extends RiddleSeparateToStringState[RiverState] {
  assert((left.items ++ right.items).size == 4)

  def me = this

  override def equals(other: Any): Boolean =
    other match {
      case r: RiverState =>
        left == r.left && right == r.right
      case _ => false
    }

  override def hashCode(): Int =
    left.hashCode + right.hashCode

  def innerToString(): String =
    left + ", " + right

  def isGoal(): Boolean = left.items.size == 4

  def withWithoutPerson(): (Set[Item], Set[Item]) =
    if (left.items(Person)) (left.items, right.items) else (right.items, left.items)

  def nextStatesTakeSomethingBack() = {
    val (p, np) = withWithoutPerson
    val pnop = p - Person
    val newStates =
      pnop.toSeq.map(item => (Side(np + item + Person), Side(pnop - item)))
          .filter( { case (p, np) => p.isValid && np.isValid })
    val pIsLeft = p eq left.items
    newStates.map( { case (p, np) =>
      if (pIsLeft) new RiverState(np, p, Some(this)) else new RiverState(p, np, Some(this)) })
  }
  
  def nextStateMovePerson(): Option[RiverState] = {
    val (newLeft, newRight) =
      if (left.items(Person)) {
        (Side(left.items - Person),
         Side(right.items + Person))
      } else {
        (Side(left.items + Person),
         Side(right.items - Person))
      }
    if (newLeft.isValid && newRight.isValid) {
      Some(new RiverState(newLeft, newRight, Some(this)))
    } else {
      None
    }
  }

  def nextStates() =
    nextStatesTakeSomethingBack ++ nextStateMovePerson
}

object Main {
  def main(args: Array[String]) {
    new RiddleProcessor(
      new RiverState(Side(Set()), Side(Set(Person, Wolf, Duck, Seeds)), None)).printResults()
  }
}
