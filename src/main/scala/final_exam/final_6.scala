package final_exam

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import utils.BigDecimalSymbolicVariables
import hw_8.particle_filter

@RunWith(classOf[JUnitRunner])
@EnhanceStrings
class final_6 extends particle_filter with FunSuite with BigDecimalSymbolicVariables {
  case class Particle(name: Symbol, value: Symbol)
  def weightParticles(xs: Seq[Particle], z: Symbol) = {
    norm(xs.map {case Particle(name, value) => "w(#name.name)" =: "p(#z.name#|#value.name)".bd}: _*)
  }
  test("final") {
    "p(W#|black)" =: 1.v - ("p(B#|black)" =: 0.7.v)
    "p(B#|white)" =: 1.v - ("p(W#|white)" =: 0.6.v)
    val xs = List(Particle('A, 'white), Particle('B, 'black), Particle('C, 'black), Particle('D, 'white),
      Particle('E, 'black),
      Particle('F, 'black), Particle('G, 'black), Particle('H, 'black))
    weightParticles(xs, 'W)
    assert(rounded("w(A)") === 0.2.v)
    weightParticles(xs, 'B)
    assert(rounded("w(A)") === 0.08.v)
  }
}