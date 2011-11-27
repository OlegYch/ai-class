package hw_6

import lections.spam

object hw_6_1 extends App {

  import lections.spam._

  spam.main(Array())

  val ps = "abcabc" :: "aabbcc" :: "aaaccc" :: Nil
  val pp = ps.flatMap(p => p.zip(p.tail) map {case (a, b) => a.toString + b})
  println(pp)
  val k = 0
  val states = "abc" map (c => c.toString)
  //  "p(a0)" =: `ls(m)`("a", "a" :: Nil dict, states dict)(k)
  for (prior <- states; post <- states) {
    "p(%s -> %s)".format(prior, post) =: `ls(m|_)`((prior + post) :: Nil)(pp.filter(_.startsWith(prior)), pp)(k)
  }
}