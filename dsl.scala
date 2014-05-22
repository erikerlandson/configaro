import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.postfixOps

trait Meta {
  val map: mutable.Map[String, String] = mutable.Map()

  trait HasTypeSym[T] {
    def sym:Symbol
  }

  implicit val typeSymLong = new HasTypeSym[Long] { def sym:Symbol = Symbol("Long") }

  def typ[T:HasTypeSym]:Symbol = {
    val sym = implicitly[HasTypeSym[T]]
    sym.sym
  }

  class Context(ss:String) {
    val key = ss

    def is(tsym:Symbol):this.type = {
      map(key) = map(key) + (" is"+ tsym.toString)
      this
    }

    def gt[T:Numeric](t:T):this.type = {
      map(key) = map(key) + s" > $t"
      this
    }

  }

  implicit def handleString(s:String):Context = {
    println(s"handling string $s")
    println(s"resetting string $s")
    map(s) = s
    println(s"returning new context for $s")
    new Context(s)
  }
}
