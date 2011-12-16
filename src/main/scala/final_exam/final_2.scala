package final_exam

@EnhanceStrings
object final_2 extends App {

  import lections.propositional._

  lections.propositional.main(Array())

  val Pink: Var = 'Pink
  val Green: Var = 'Green
  val terms = ('A' to 'D').map(_.toString).zip(List(Pink, Pink V Green, Pink A Green, (!Pink) ==> Green))
  val points = for {
    (r, row) <- terms
    (c, col) <- terms
  } yield {
    ("#r ==> #c", (row ==> col).`true`)
  }
  println(points.grouped(terms.size).toList.mkString("\n"))
}