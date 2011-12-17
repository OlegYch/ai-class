package final_exam

import org.scalatest.FunSuite

@EnhanceStrings
class final_9 extends FunSuite {

  import hw_7.hw_7_1._

  implicit def toDim(i: Int): Dim = i * m
  implicit def toDim(i: Symbol): Dim = Left(i.bd)

  case class ProjectionAssertion(modification: bd => bd, function: bd => bd, initial: bd = 100) {
    val before = function(initial)
    val after = function(modification(initial))
    val increases = after > before
    val decreases = after < before
    val halfAsLarge = after === before / 2
  }
  def decreasing(f: bd => bd) = ProjectionAssertion(_ - 1, f)
  def increasing(f: bd => bd) = ProjectionAssertion(_ + 1, f)
  def doubling(f: bd => bd) = ProjectionAssertion(_ * 2, f)

  test("if the object moves closer, the size of projected image increases") {
    assert(decreasing(distance => perspectiveProjection(Z = distance, f = 'f, X = 'X).x).increases)
  }
  test("if we use a camera with longer focal length, the size of projected image increases") {
    assert(increasing(focalLength => perspectiveProjection(Z = 'Z, f = focalLength, X = 'X).x).increases)
  }
  test("if we double the distance, the projected image will be half as large") {
    assert(doubling(distance => perspectiveProjection(Z = distance, f = 'f, X = 'X).x).halfAsLarge)
  }
  test("the ratio of the focal length over the distance is the same as the projected size") {
    assert(perspectiveProjection(Z = 'distance, f = 'focalLength, X = 'X).x / m === 'focalLength.bd / 'distance)
  }
}