package hw_8

import utils.BigDecimalSymbolicVariables
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class particle_filter extends FunSuite with BigDecimalSymbolicVariables {
  test("unit20_03") {
    def norm(vs: Variable*) = {
      "alpha" =: 1.v / (vs.map(_.bd).sum)
      vs map (v => v =: v.bd * "alpha")
    }
    "p(d|on)" =: 1.v - ("p(b|on)" =: 0.8.v)
    "p(b|off)" =: 1.v - ("p(d|off)" =: 0.9.v)
    val xs = Vector.fill(3)('off) ++ Vector.fill(3)('on)
    val z = 'b
    norm(xs.zipWithIndex.map {case (s, i) => "w(%s,%s)".format(i, s.name) =: "p(%s|%s)".format(z.name, s.name).bd}: _*)
    assert("w(0,off)".bd === 1.v / 27)
    assert("w(3,on)".bd === 8.v / 27)
  }
}