package delegation.impl

trait A {
  def f = 1
}

object B {
  implicit def delegateToA(b: B) = b.a
}

class B(var a: A) {
  def qwe = this f
}

object c extends App {
  val b = new B(new A {override def f = 2})
  println(b.f)

  case class ff[T <% A](t: T) {def ff = t.f}
  case class fff[T <: A](t: T) {def ff = t.f}

  val ff1 = new ff(b) {override def ff = super.ff + t.qwe}
  val ff2 = new fff[A](b) {override def ff = super.ff /*+ t.qwe*/}
  def q(a: =>A = B.delegateToA(b))=  a.f
  println(q())
  b.a = new A {override def f = 3}
  println(ff1.ff)
  println(ff2.ff)
  println(q())
  println(q(new A{}))
}