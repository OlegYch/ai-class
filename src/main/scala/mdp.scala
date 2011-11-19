package mdp

import scala.collection.immutable.TreeMap
import scala.collection.immutable.Range.Inclusive
import scala.collection.immutable.NumericRange.Inclusive


object mdp extends App {
  type P = (Char, Int)
  type V = BigDecimal

  implicit def toV(i: Int) = new {val v = BigDecimal(i)}

  implicit def toV(i: Double) = new {val v = BigDecimal(i)}

  case class Action(name: Symbol, outcomes: Traversable[(P => P, V)])

  case class Space(positions: Map[P, V])

  val Death = ('b', 1)
  val Goal = ('b', 4)
  val initialSpace = Space(TreeMap((for (y <- ('a' to 'b'); x <- (1 to 4)) yield {
    (y, x) match {
      case p@Death => p -> -100.v
      case p@Goal => p -> 100.v
      case p => p -> 0.v
    }
  }): _*))
  println(initialSpace)
  val directions = List('N ->(-1, 0), 'S ->(1, 0), 'W ->(0, -1), 'E ->(0, 1))
  val p = 0.8.v

  def modifyPosition(dp: (Int, Int)): (P => P) = p => {
    val newP = ((p._1 + dp._1).asInstanceOf[Char], p._2 + dp._2)
    initialSpace.positions.get(newP) match {
      case None => p
      case _ => newP
    }
  }

  val actions = directions.map(d => {
    val (name, dp) = d
    Action(name, List(p, 1 - p).map(p => (modifyPosition(dp), p)))
  })
  println(actions)

}