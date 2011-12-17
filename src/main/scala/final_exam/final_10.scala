package final_exam

import org.scalatest.FunSuite

@EnhanceStrings
class final_10 extends FunSuite {

  import hw_7.hw_7_4._

  implicit def toDim(i: Symbol): Dim = i.bd

  test("calculate new displacement") {
    'Z =: 100 * m
    'disp =: 2 * mm
    'f =: 40 * mm
    'baseline =: StereoDistance(Z = 'Z, deltaX = 'disp, f = 'f).B / m
    'newDisp =: StereoDistance(Z = 'Z, B = 'baseline * 2 * m, f = 'f).deltaX / mm
    assert('newDisp.bd === 4)
  }
}