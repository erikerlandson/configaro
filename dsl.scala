import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.postfixOps

class CV[+T](op:Option[T]) {
  val o = op
}

trait Meta {
  val map: mutable.Map[String, Function[Option[String], CV[Any]]] = mutable.Map()

  trait TypeConverter[T] {
    def func: Function[Option[String], CV[T]]
  }

  implicit val tcLong = new TypeConverter[Long] {
     def func: Function[Option[String], CV[Long]] = (d:Option[String]) => new CV(d.map(_.toLong))
  }
  implicit val tcDouble = new TypeConverter[Double] {
     def func: Function[Option[String], CV[Double]] = (d:Option[String]) => new CV(d.map(_.toDouble))
  }
  implicit val tcString = new TypeConverter[String] {
     def func: Function[Option[String], CV[String]] = (d:Option[String]) => new CV(d)
  }


  def typ[T:TypeConverter]:Function[Option[String], CV[T]] = {
    val tc = implicitly[TypeConverter[T]]
    tc.func
  }

  class Context[R](k:String, f:Function[Option[String],R]) {
    val key = k
    val func = f

    def is[S](g:Function[R,CV[S]]):Context[CV[S]] = {
      val h = func andThen g
      map(key) = h
      new Context(key, h)
    }
  }

  implicit def handleString(s:String):Context[Option[String]] = {
    map -= s
    new Context(s, (x:Option[String])=>x)
  }
}
