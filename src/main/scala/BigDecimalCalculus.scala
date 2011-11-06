trait BigDecimalCalculus {
  type bd

  implicit def toVariable(s: Symbol) = Variable(s.name)

  implicit def toVariable(s: String) = Variable(s)

  implicit def fromVariable(s: Variable) = s.name

  case class Variable(name: String) {
    def bd: bd = variables(name)
  }

  var variables = Map[Variable, bd]()

  implicit def withAssignment(b: bd) = new {
    def =:(s: Variable) {
      variables += s -> b
      println("%s = %s".format(s, b))
    }
  }

  implicit def fromVars(s: String): bd = s.bd

  implicit def fromVars(s: Symbol): bd = s.bd
}