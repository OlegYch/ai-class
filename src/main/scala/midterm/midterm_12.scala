package midterm

object midterm_12 extends App {
  lections.propositional.main(Array())

  import lections.propositional._

  def header = "Valid\tSat.\tUnsat."

  def inspect(v: Eval) = {
    def toS(b: Boolean) = (if (b) "*" else "\t") + "\t"
    (v.valid :: v.satisfiable :: v.unSatisfiable :: Nil).map(toS).mkString("") + v.toString
  }

  def inspect(vs: Eval*) {
    println(header)
    vs.foreach(inspect _ andThen println _)
  }

  inspect(
    !'A
    , 'A V !'A
    , ('A A !'A) ==> ('B ==> 'C)
    , ('A ==> 'B) A ('B ==> 'C) A ('C ==> 'A)
    , ('A ==> 'B) A !(!'A V 'B)
    , (('A ==> 'B) A ('B ==> 'C)) <==> ('A ==> 'C)
  )
}