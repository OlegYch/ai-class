package mdp

import scala.collection.immutable.TreeMap


object mdp extends App {
  type P = (Char, Int)
  type V = BigDecimal
  type A = Symbol

  implicit def toV(i: Int) = new {val v = BigDecimal(i)}

  implicit def toV(i: Double) = new {val v = BigDecimal(i)}

  case class Action(name: Symbol, transitions: Traversable[(P => P, V)])

  case class Space(positions: Map[P, (V, Option[A])])

  val Death = ('b', 1)
  val Goal = ('b', 4)
  val terminalPositions = Set(Death, Goal)
  val initialSpace = Space(TreeMap((for (y <- ('a' to 'b'); x <- (1 to 4)) yield {
    (y, x) match {
      case p@Death => p -> (-100.v -> None)
      case p@Goal => p -> (100.v -> None)
      case p => p -> (0.v -> None)
    }
  }): _*))
  println(initialSpace)
  val directions = List('N ->(-1, 0), 'S ->(1, 0), 'W ->(0, -1), 'E ->(0, 1))
  val p = 0.8.v
  val cost = -4.v

  def modifyPosition(dp: (Int, Int)): (P => P) = p => {
    if (terminalPositions.contains(p)) {
      p
    } else {
      val newP = ((p._1 + dp._1).asInstanceOf[Char], p._2 + dp._2)
      initialSpace.positions.get(newP) match {
        case None => p
        case _ => newP
      }
    }
  }

  val actions = directions.map(d => {
    val (name, dp) = d
    val inverseDp = (dp._1 * -1, dp._2 * -1)
    Action(name, List(modifyPosition(dp) -> p, modifyPosition(inverseDp) -> (1 - p)))
  })
  println(actions)

  def updateSpaceValues(p: P, s: Space): Space = {
    if (terminalPositions.contains(p)) {
      s
    } else {
      val probableRewards = actions.flatMap {
        case Action(name, transitions) => {
          val reward = transitions.flatMap {
            case (t, probability) => Some(s.positions(t(p))._1 * probability)
          }.sum
          Some(name -> (reward + cost))
        }
      }
      val bestActionReward = probableRewards.maxBy(_._2)
      s.copy(positions = s.positions + (p ->(bestActionReward._2, Some(bestActionReward._1))))
    }
  }

  var finalSpace = initialSpace
  for (i <- 0 to 100) {
    finalSpace.positions.keys.foreach {
      p =>
        finalSpace = updateSpaceValues(p, finalSpace)
    }
  }
  println(finalSpace)
}