package utils

trait FunctionSystems {
  type bd
  type function <: FunctionSystem
  type Dim = Either[bd, function => bd]
  implicit def toLeft(b: bd) = Left(b)
  implicit def toRight(b: function => bd) = Right(b)
  var currentFunction: () => function = () => sys.error("currentFunction is not assigned")
  implicit def fromDim(b: Dim): bd = b match {case Left(b) => b; case Right(b) => b(currentFunction())}

  trait FunctionSystem {self: function =>
    currentFunction = () => self
  }
}