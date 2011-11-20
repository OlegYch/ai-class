package midterm

import lections.mdp
import scala.collection.immutable.TreeMap


object midterm_14 extends mdp with App {

  override lazy val Goal = ('a', 4)
  override lazy val Death = sys.error("not defined")
  override lazy val terminalPositions = Set(Goal)
  override lazy val p = 1.v
  override lazy val cost = -5.v

  override lazy val initialSpace = {
    val spaces = for (y <- ('a' to 'b'); x <- (1 to 4)) yield {
      (y, x) match {
        case p@Goal => Some(p -> (100.v -> None))
        case ('a', 3) => None
        case p => Some(p -> (0.v -> None))
      }
    }
    Space(TreeMap(spaces.flatten: _*))
  }

  iterate
}

