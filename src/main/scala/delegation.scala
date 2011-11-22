package delegation.impl

trait A {
  def f = 1
}

trait C[T] {
  def t: T

  def t2(i: T): T
}

object B {
  implicit def delegateToA(b: B) = b.a

  implicit def delegateToC(b: B) = b.c

  implicit def delegateToC2(b: B) = b.c2
}

class B(var a: A, var c: C[Int], var c2: C[String]) {
  def qwe = this f
}

object cApp extends App {
  val b = new B(new A {override def f = 2}, new C[Int] {
    def t = 100500

    def t2(i: Int) = i + 100500
  }, new C[String] {
    def t = "hello"

    def t2(i: String) = i + " world"
  })
  println(b.f)

  case class ff[T <% A](t: T) {def ff = t.f}

  case class fff[T <: A](t: T) {def ff = t.f}

  val ff1 = new ff(b) {override def ff = super.ff + t.qwe}
  val ff2 = new fff[A](b) {override def ff = super.ff /*+ t.qwe*/}

  def q(a: => A = B.delegateToA(b)) = a.f

  println(q())
  b.a = new A {override def f = 3}
  println(ff1.ff)
  println(ff2.ff)
  println(q())
  println(q(new A {}))
  println((b: C[Int]).t)
  println((b: C[String]).t)
  println(b.t2(42))
  println(b.t2("hello"))
}