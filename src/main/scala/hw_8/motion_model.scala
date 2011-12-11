package hw_8

import utils.BigDecimalSymbolicVariables

object motion_model extends App with BigDecimalSymbolicVariables {

  override val delayedCalculation = true

  import math._

  def f = {
    'x =: 24.v
    'y =: 18.v
    'theta =: 0.v
    't =: 1.v
    'v =: 5.v
    'w =: Pi.v / 8
    'x1 =: 'x + 'v * 't * cos('theta.toDouble)
    'y1 =: 'y + 'v * 't * sin('theta.toDouble)
    'theta1 =: 'theta + 'w * 't

    'x =: 0.v
    'y =: 10.v
    'theta =: Pi.v / 4
    'w =: -Pi.v / 4
    'x1.l
    'y1.l
    'theta1.l
  }
  f
}