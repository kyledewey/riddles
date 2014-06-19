package riddles
import scala.annotation.tailrec


trait RiddleState[T <: RiddleState[T]] {
  def depth(): Int = {
    @tailrec
    def depthAccum(myParent: Option[T], accum: Int): Int = 
      myParent match {
        case None => accum
        case Some(p) => depthAccum(p.parent, accum + 1)
      }
    depthAccum(parent, 1)
  }
  def parent(): Option[T]

  def parentUntilMe(initialAccum: List[T] = List()): Seq[T] = {
    @tailrec
    def rec(me: Option[T], accum: List[T]): List[T] = {
      me match {
        case None => accum
        case Some(p) => rec(p.parent, p :: accum)
      }
    }

    rec(parent, initialAccum).toSeq
  }
  
  def isGoal(): Boolean
  def nextStates(): Seq[T]
  def riddleString(): String = toString
}

object RiddleSeparateToStringState {
  val DIVIDER = "-------------------"
}

trait RiddleSeparateToStringState[T <: RiddleSeparateToStringState[T]] extends RiddleState[T] {
  def me: T // HACK - I have no idea why `this` doesn't work directly
  def innerToString(): String
  override def riddleString(): String = {
    import RiddleSeparateToStringState.DIVIDER
    DIVIDER + "\n" + parentUntilMe(List(me)).map(_.innerToString).mkString("\n") + "\n" + DIVIDER
  }
}

class RiddleProcessor[T <: RiddleState[T]](initStates: Seq[T]) {
  def this(initState: T) =
    this(Seq(initState))

  import scala.collection.mutable.{Queue, HashSet, Buffer}
  private val queue = (new Queue[T]()) ++ initStates
  private val seen = new HashSet[T]()

  @tailrec
  final def nextState(): Option[T] =
    if (queue.isEmpty) None 
    else {
      val candidate = queue.dequeue
      if (seen(candidate)) nextState
      else {
        seen += candidate
        Some(candidate)
      }
    }

  def getSolutionsAtDepth(d: Int): Seq[T] = {
    val retval = Buffer[T]()
    var shouldRun = true
    while (queue.nonEmpty && shouldRun) {
      val state = queue.dequeue
      if (state.depth >= d) {
        shouldRun = false
      } else {
        val next = state.nextStates
        assert(next.forall(_.depth == d))
        retval ++= next.filter(_.isGoal)
      }
    }

    retval.toSeq
  }

  @tailrec
  final def processUntilSolutions(): Seq[T] = {
     nextState match {
       case Some(state) => {
         val nextStates = state.nextStates
         val goals = nextStates.filter(_.isGoal)
         if (goals.nonEmpty) {
           // all goals have the same depth
           assert(goals.map(_.depth).toSet.size == 1)
           goals ++ getSolutionsAtDepth(goals.head.depth)
         } else {
           val notSeen = nextStates.filter(!seen(_))
           queue ++= notSeen
           processUntilSolutions()
           }
       }
       case None => Seq()
     }
  }

  def printResults() {
    val solutions = processUntilSolutions
    println((solutions.size match {
      case 0 => "No solutions found."
      case 1 => ("One solution found at depth " + 
                 solutions.head.depth + ":\n" + solutions.head.riddleString)
      case n => (n + " solutions found at depth " + solutions.head.depth + 
                 ":\n" + solutions.map(_.riddleString).mkString("\n"))
    }))
  }
}
