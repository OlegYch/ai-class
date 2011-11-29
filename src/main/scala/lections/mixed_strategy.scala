package lections

import utils.SymbolicVariables
import scala.collection.immutable.List

object mixed_strategy extends App with SymbolicVariables {
  type bd = BigDecimal
  val bd = BigDecimal

  implicit def toV(i: Int) = BigDecimal(i)

  implicit def toV(i: Double) = BigDecimal(i)

  //first sample
  solveTheGame(List(2, -3, -3, 4))
  //question 1
  solveTheGame(List(5, 3, 4, 2))
  //question 2
  solveTheGame(List(3, 6, 5, 4))

  def solveTheGame(usStart: List[Int]) {
    var us = usStart
    var utilities = List[Variable]()
    for (move1 <- List("1", "2"); move2 <- List("1", "2")) {
      utilities = ("u(max,%s,%s)".format(move1, move2) =: bd(us.head)) ::
        ("u(min,%s,%s)".format(move1, move2) =: -bd(us.head)) ::
        utilities
      us = us.tail

    }

    case class Solution(l: bd, r: bd, p: bd = 0)

    implicit def `with===`(l: bd) = new {def ===(r: bd) = Solution(l, r)}

    def solve(f: (bd => Solution)) = {
      val s = for (p <- (bd(0) to 1 by 0.0001)) yield f(p).copy(p = /*'p =: */ p)
      'p =: s.minBy(s => (/*'f =: */ (s.l - s.r).abs)).p
    }

    "p(max,1)" =: solve {p =>
      ("u(max,1,1)".bd * p + "u(max,2,1)".bd * (1 - p)) ===
        ("u(max,1,2)".bd * p + "u(max,2,2)".bd * (1 - p))
    }
    "p(max,2)" =: bd(1) - "p(max,1)"
    "p(min,1)" =: solve {p =>
      ("u(max,1,1)".bd * p + "u(max,1,2)".bd * (1 - p)) ===
        ("u(max,2,1)".bd * p + "u(max,2,2)".bd * (1 - p))
    }
    "p(min,2)" =: solve {p =>
      ("u(max,1,1)".bd * (1 - p) + "u(max,1,2)".bd * p) ===
        ("u(max,2,1)".bd * (1 - p) + "u(max,2,2)".bd * p)
    }

    "u(max)" =: List("u(max,1,1)", "u(max,2,1)").
      zip(List[bd]("p(max,1)", "p(max,2)")).
      map(p => p._1 * p._2).sum
  }
}