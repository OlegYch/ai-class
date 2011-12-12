package hw_8

import utils.BigDecimalSymbolicVariables
import scala.math.BigDecimal.RoundingMode

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

    'x =: 0.v
    'y =: 0.v
    'theta =: 0.v
    't =: 4.v
    'v =: 10.v
    'w =: Pi.v / 8
    'T =: 0.v
    while ('T < 16.v) {
      'T =:: 'T + 't
      'x =:: 'x1.bd
      'y =:: 'y1.bd
      'theta =:: 'theta1.bd
      "theta/pi" =:: 'theta / Pi
    }
  }

  f
  override def log(s: Variable, b: motion_model.bd) = {
    super.log(s, b.setScale(3, RoundingMode.HALF_UP))
  }
}
