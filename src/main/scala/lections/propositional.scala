package lections

import utils.SymbolicVariables

object propositional extends App with SymbolicVariables {
  type bd = Var

  override def default = (v) => VarConst(Symbol(v))

  sealed trait Var {
    def ==>(r: Var) = new ==>(this, r)

    def <==>(r: Var) = new <==>(this, r)

    def A(r: Var) = new A(this, r)

    def V(r: Var) = new V(this, r)

    def unary_! = new !(this)
  }

  class Eval(variable: Var) {
    def toBoolean(v: Var = variable)(implicit dict: Map[Var, Boolean]): Boolean = v match {
      case v@(==>(l, r)) => v(toBoolean(l) -> toBoolean(r))
      case v@(<==>(l, r)) => v(toBoolean(l) -> toBoolean(r))
      case v@(V(l, r)) => v(toBoolean(l) -> toBoolean(r))
      case v@(A(l, r)) => v(toBoolean(l) -> toBoolean(r))
      case v@(!(l)) => v(toBoolean(l))
      case v@True => true
      case v@False => false
      case v: VarConst => dict(v)
    }

    val vars = {
      var result = Set[Var]()
      toBoolean()(Map.empty.withDefault(v => {result += v; false}))
      result
    }

    lazy val inputs = for (s <- (0 to vars.size); trueVars <- vars.toStream.combinations(s))
    yield vars.map(v => v -> trueVars.contains(v)).toMap

    lazy val eval = inputs.map {varsMap => (varsMap, toBoolean()(varsMap))}

    lazy val valid = eval.collectFirst {case (_, false) =>}.isEmpty
    lazy val satisfiable = !valid && eval.collectFirst {case (_, true) =>}.isDefined
    lazy val unSatisfiable = eval.collectFirst {case (_, true) =>}.isEmpty
    lazy val `true` = valid
    lazy val `false` = unSatisfiable
    lazy val ? = satisfiable

    override def toString = variable.toString
  }

  implicit def toEval(v: Var) = new Eval(v)

  abstract class VarImpl(val s: Symbol) extends Var {
    override def toString = s.name

    override def equals(obj: Any) = obj match {case v: VarImpl => v.s.equals(s) case _ => false}

    override def hashCode() = s.hashCode()
  }

  case object True extends VarImpl('True) {
    def apply = true
  }

  case object False extends VarImpl('False) {
    def apply = false
  }

  case class VarConst(override val s: Symbol) extends VarImpl(s)

  case class ==>(l: Var, r: Var) extends VarImpl(Symbol("(%s ⇒ %s)".format(l, r))) {
    def apply(p: (Boolean, Boolean)) = p match {
      case (false, false) => true
      case (false, true) => true
      case (true, false) => false
      case (true, true) => true
    }
  }

  case class <==>(l: Var, r: Var) extends VarImpl(Symbol("(%s ⇔ %s)".format(l, r))) {
    def apply(p: (Boolean, Boolean)) = p match {
      case (false, false) => true
      case (false, true) => false
      case (true, false) => false
      case (true, true) => true
    }
  }

  case class A(l: Var, r: Var) extends VarImpl(Symbol("(%s ∧ %s)".format(l, r))) {
    def apply(p: (Boolean, Boolean)) = p match {
      case (false, false) => false
      case (false, true) => false
      case (true, false) => false
      case (true, true) => true
    }
  }

  case class V(l: Var, r: Var) extends VarImpl(Symbol("(%s ∨ %s)".format(l, r))) {
    def apply(p: (Boolean, Boolean)) = p match {
      case (false, false) => false
      case (false, true) => true
      case (true, false) => true
      case (true, true) => true
    }
  }

  case class !(l: Var) extends VarImpl(Symbol("¬%s".format(l))) {
    def apply(p: Boolean) = !p
  }

  def inspect(v: Eval) = {
    def toS(b: Boolean) = (if (b) "*" else "\t") + "\t"
    (v.`true` :: v.`false` :: v.? :: Nil).map(toS).mkString("") + v.toString
  }

  val Smoke = 'Smoke
  val Fire = 'Fire
  val Big = 'Big
  val Dumb = 'Dumb

  def header = println("True\tFalse\t?\t")

  header
  println(inspect((Smoke ==> Fire) <==> (Smoke V !Fire)))
  println(inspect((Smoke ==> Fire) <==> (Smoke V !Fire)))
  println(inspect((Smoke ==> Fire) <==> (!Smoke ==> !Fire)))
  println(inspect((Smoke ==> Fire) <==> (!Fire ==> !Smoke)))
  println(inspect(Big V Dumb V (Big ==> Dumb)))
  println(inspect((Big A Dumb) <==> !(!Big V !Dumb)))
  compare('Cc A 'Bci A 'Bcs, 'Cc ==> ('Bci A 'Bcs))
  compare(('Cx A 'Cy A 'Bxy) ==> !'Mc, !'Cx V !'Cy V !'Bxy V !'Mc)

  compare("open" <==> ("dial" V ("already open" A "not lock")), "open" <==> ("dial" A "not lock"))

  def compare(_v1: Var, _v2: Var) {
    val allVars = (_v1.vars ++ _v2.vars).toSet
    val v1 = new Eval(_v1) {override val vars = allVars}
    val v2 = new Eval(_v2) {override val vars = allVars}
    println(inspect(v1))
    println(v1.eval)
    println(inspect(v2))
    println(v2.eval)
    println("Diff:")
    println(v1.eval == v2.eval)
    println(v1.eval.toSet -- v2.eval.toSet mkString ("\n"))
  }
}