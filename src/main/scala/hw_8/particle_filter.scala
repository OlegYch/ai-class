package hw_8

import utils.BigDecimalSymbolicVariables
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import BigDecimal.RoundingMode._
import scala.collection.mutable.Queue

@RunWith(classOf[JUnitRunner])
class particle_filter extends FunSuite with BigDecimalSymbolicVariables {
  def norm(vs: Variable*) = {
    "alpha" =: 1.v / (vs.map(_.bd).sum)
    vs map (v => v =: v.bd * "alpha")
  }
  def weight(xs: Seq[Symbol], z: Symbol) = {
    norm(xs.zipWithIndex.map {case (s, i) => "w(%s,%s)".format(i, s.name) =: "p(%s|%s)".format(z.name, s.name).bd}: _*)
  }
  test("unit20_03") {
    "p(d|on)" =: 1.v - ("p(b|on)" =: 0.8.v)
    "p(b|off)" =: 1.v - ("p(d|off)" =: 0.9.v)
    val xs = Vector.fill(3)('off) ++ Vector.fill(3)('on)
    val z = 'b
    weight(xs, z)
    assert("w(0,off)".bd === 1.v / 27)
    assert("w(3,on)".bd === 8.v / 27)
  }
  test("hw8_4") {
    "p(W|black)" =: 1.v - ("p(B|black)" =: 0.8.v)
    "p(W|white)" =: 1.v - ("p(B|white)" =: 0.1.v)
    val xs = Vector.fill(2)('black) ++ Vector.fill(3)('white)
    val z = 'B
    val weights = weight(xs, z)
    val blackParticles = weights.filter(_.contains("black"))
    "b2" =: blackParticles.map(_.bd).sum
    "a2" =: "c2" =: "b3" =: (weights.filterNot(blackParticles contains _)).map(_.bd).head
    assert("b2".bd.setScale(3, HALF_UP) === 0.842.v)
    assert("a2".bd.setScale(3, HALF_UP) === 0.053.v)
  }
  test("hw8_5") {
    "p(W|black)" =: 1.v - ("p(B|black)" =: 0.8.v)
    "p(W|white)" =: 1.v - ("p(B|white)" =: 0.1.v)
    val xs = Vector.fill(2)('black) ++ Vector.fill(3)('white)
    val z = 'W
    val weights = weight(xs, z)
    val blackParticles = weights.filter(_.contains("black"))
    val whiteParticles = Queue(weights.filterNot(blackParticles contains _): _*)
    "b3" =: whiteParticles.dequeue().bd + whiteParticles.dequeue()
    "b4" =: blackParticles.map(_.bd).sum
    "c4" =: whiteParticles.dequeue().bd
    assert("b3".bd.setScale(3, HALF_UP) === 0.581.v)
    assert("b4".bd.setScale(3, HALF_UP) === 0.129.v)
    assert("c4".bd.setScale(3, HALF_UP) === 0.290.v)
  }
}