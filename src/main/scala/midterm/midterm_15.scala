package midterm

import lections.spam

object midterm_15 extends App {

  import lections.spam._

  spam.main(Array())

  val p = "aaaab"
  val pp = p.zip(p.tail) map {case (a, b) => a.toString + b}
  "p(a0)" =: `ls(m)`("a", "a" :: Nil dict, "a b" :: Nil dict)(1)
  "p(a|a)" =: `ls(m|_)`("aa" :: Nil)(pp.filter(_.startsWith("a")), pp)(1)
  "p(a|b)" =: `ls(m|_)`("ba" :: Nil)(pp.filter(_.startsWith("b")), pp)(1)
}