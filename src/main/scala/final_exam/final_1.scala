package final_exam

object final_1 extends App {
  val disks = 1 to 4
  val pegs = 1 to 3
  def disksOnPeg(availableDisks: Seq[Int]): Seq[Seq[Int]] = {
    (0 to availableDisks.size).flatMap(s => availableDisks.combinations(s.toInt))
    //      .flatMap(s => s.permutations)
  }
  val s = for {
    peg1 <- disksOnPeg(disks)
    peg2 <- disksOnPeg(disks.diff(peg1))
    peg3 <- disksOnPeg(disks.diff(peg1 ++ peg2))
    allPegs = (peg1 :: peg2 :: peg3 :: Nil)
    if (allPegs.flatten).sorted == disks
  //    if allPegs.forall(p => p == p.sorted)
  } yield allPegs
  println(s.mkString("\n"))
  println(s.toSet.size)
  val n = disks.size
  println(math.pow(2, n) - 1)
}