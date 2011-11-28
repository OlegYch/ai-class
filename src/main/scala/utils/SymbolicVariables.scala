package utils

trait SymbolicVariables {
  type bd

  implicit def toVariable(s: Symbol) = Variable(s.name)

  implicit def toVariable(s: String) = Variable(s)

  implicit def fromVariable(s: Variable) = s.name

  case class Variable(name: String) {
    def bd: bd = variables(name)

    override def toString = name
  }

  def default: (Variable) => bd = (v) => sys.error("%s was not defined".format(v))

  var variables = Map[Variable, bd]().withDefault(default)

  implicit def withAssignmentFromVariable(v: Variable) = withAssignmentFromValue(fromVars(v))

  implicit def withAssignmentFromValue(b: bd): {def =:(s: Variable): Variable} = new {
    def =:(s: Variable) = {
      variables += s -> b
      println("%s = %s".format(s, b))
      s
    }
  }

  implicit def fromVars(s: String): bd = s.bd

  implicit def fromVars(s: Symbol): bd = s.bd

  implicit def fromVars(v: Variable): bd = fromVars(v.name)
}