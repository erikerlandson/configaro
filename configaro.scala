/*
Copyright (c) 2014 Erik Erlandson

Author:  Erik Erlandson <erikerlandson@yahoo.com>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.postfixOps


case class ConversionException(message:String) extends Exception(message)
case class PolicyViolation(msg:String) extends Exception(msg)

type Notifier = PolicyViolation=>Unit

def conversionMessage(v:Any):String = {
  val tString = v.getClass.getName
  s"Failed to convert $tString = $v to the requested type"
}

trait ConvertOptionToV[V] {
  def convert(cv:Any):V
}

implicit val objectOptionToLong = new ConvertOptionToV[Long] {
  def convert(cv:Any):Long = try { cv match {
      case v: Long => v
      case v: Double if v == v.toLong.toDouble => v.toLong
      case v: String => v.toLong
      case _ => throw new Exception
    }
  } catch {
    case _: Throwable => throw new ConversionException(conversionMessage(cv))
  }
}
implicit val objectOptionToInt = new ConvertOptionToV[Int] {
  def convert(cv:Any):Int = try { cv match {
      case v: Long if v == v.toInt.toLong => v.toInt
      case v: Double if v == v.toInt.toDouble => v.toInt
      case v: String => v.toInt
      case _ => throw new Exception
    }
  } catch {
    case _: Throwable => throw new ConversionException(conversionMessage(cv))
  }  
}
implicit val objectOptionToDouble = new ConvertOptionToV[Double] {
  def convert(cv:Any):Double = try { cv match {
      case v: Long => v.toDouble
      case v: Double => v
      case v: String => v.toDouble
      case _ => throw new Exception
    }
  } catch {
    case _: Throwable => throw new ConversionException(conversionMessage(cv))
  }
}
implicit val objectOptionToFloat = new ConvertOptionToV[Float] {
  def convert(cv:Any):Float = try { cv match {
      case v: Long => v.toFloat
      case v: Double => v.toFloat
      case v: String => v.toFloat
      case _ => throw new Exception
    }
  } catch {
    case _: Throwable => throw new ConversionException(conversionMessage(cv))
  }
}
implicit val objectOptionToString = new ConvertOptionToV[String] {
  def convert(cv:Any):String = try {
    cv.toString
  } catch {
    case _: Throwable => throw new ConversionException(conversionMessage(cv))
  }
}


trait MetaConfiguration {
  val policy = this

  val map: mutable.Map[String, Function[Option[String], Option[Any]]] = mutable.Map()

  trait TypeConverter[T] {
    def func: String => T
  }

  implicit val tcLong = new TypeConverter[Long] {
    def func = (s:String) => try { s.toLong } catch { case _ :Throwable => throw new PolicyViolation(conversionMessage(s)) }
  }
  implicit val tcInt = new TypeConverter[Int] {
    def func = (s:String) => try { s.toInt } catch { case _ :Throwable => throw new PolicyViolation(conversionMessage(s)) }
  }
  implicit val tcDouble = new TypeConverter[Double] {
    def func = (s:String) => try { s.toDouble } catch { case _ :Throwable => throw new PolicyViolation(conversionMessage(s)) }
  }
  implicit val tcFloat = new TypeConverter[Float] {
    def func = (s:String) => try { s.toFloat } catch { case _ :Throwable => throw new PolicyViolation(conversionMessage(s)) }
  }
  implicit val tcString = new TypeConverter[String] {
    def func = (s:String) => try { s.toString } catch { case _ :Throwable => throw new PolicyViolation(conversionMessage(s)) }
  }

  private var notifierGlobal:Notifier = (e:PolicyViolation)=>{}

  def notify(n:Notifier):Unit = { notifierGlobal = n }

  private def wrap[E,S](g:E=>S, notifier:Notifier):(Option[E]=>Option[S]) = {
    (d:Option[E]) => try {
      d.map(g)
    } catch {
      case pv:PolicyViolation => {
        notifier(pv)
        None
      }
    }
  }

  def tpe[T:TypeConverter]:(String=>T) = implicitly[TypeConverter[T]].func

  def opLT[T](a:T, b:T)(implicit n: Numeric[T]):Boolean = { n.lt(a,b) }

  class Context[R](k:String, f:Option[String]=>Option[R], n:Notifier) {
    private val key = k
    private val func = f
    private var notifier = n

    private def comp[S>:R,T](g:Option[S]=>Option[T]):Context[T] = {
      val h = func andThen g
      map(key) = h
      new Context(key, h, notifier)
    }

    // configures scoped to current parameter context
    def notify(n:Notifier):Context[R] = { 
      notifier = n 
      this
    }

    // pipe in a new function, automatically wrapped in 
    // Option, violation and logging boilerplate
    def pipe[S>:R,T](g:S=>T):Context[T] = comp(wrap(g, notifier))

    // for accepting type conversion/constraint predicates
    // i.e.  is tpe[Long]
    def is[S](g:R=>S):Context[S] = pipe(g)

    // replace any missing value (None) with a default
    def default(dv:R):Context[R] = comp(
      (d:Option[R]) => d match {
        case Some(v) => Some(v)
        case None => Some(dv)
      })

    // boundary checking
    def lt[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (!opLT(x,t)) throw PolicyViolation(s"$x >= $t")
      x})
    def le[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (opLT(t,x)) throw PolicyViolation(s"$x > $t")
      x})
    def gt[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (!opLT(t,x)) throw PolicyViolation(s"$x <= $t")
      x})
    def ge[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (opLT(x,t)) throw PolicyViolation(s"$x < $t")
      x})
  }

  implicit def handleString(s:String):Context[String] = {
    map -= s
    new Context(s, (x:Option[String])=>x, notifierGlobal)
  }
}

// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class Config(mc: MetaConfiguration) extends Function[String, Option[Any]] {
  val metaConfig = mc.map
  val conf: mutable.Map[String, String] = mutable.Map()

  def apply(s:String):Option[Any] = {
    metaConfig.getOrElse(s, (x:Option[String])=>x)(conf.get(s))
  }

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    conf.put(s, v.toString)
  }

  def get[T:ConvertOptionToV](s:String):Option[T] = {
    try { 
      this(s).map(implicitly[ConvertOptionToV[T]].convert) 
    } catch {
      // A failure to convert to a supported type goes to None,
      // for consistency with behavior of meta-policy type filters
      case ConversionException(_) => None
    }
  }

  def getOrElse[T:ConvertOptionToV](s:String, d:T):T = {
    get[T](s).getOrElse(d)
  }

  def require[T:ConvertOptionToV](s:String):T = {
    this(s) match {
      case Some(v) => implicitly[ConvertOptionToV[T]].convert(v)
      case None => throw new ConversionException(conversionMessage(None))
    }
  }
}

// here is where you define policies for each config variable
// this could reside in its own file for easy maintenance
object metaConfigExample extends MetaConfiguration {
  // configure a simple notification policy that dumps PolicyViolation 
  // exceptions to standard error
  policy notify ((e:PolicyViolation)=>{ System.err.println(e.toString) })

  "a" is tpe[Long] default 42L
  "b" is tpe[Int] default -1

  // notify can appear anywhere, and multiple times, for each param declaration
  // override notify for bounds checking on "radians" to be fatal:
  "radians" is tpe[Double] default 3.1415 notify ((e:PolicyViolation)=>{ throw e }) ge 0.0 lt 6.2830

  // update 'global' notify policy here
  policy notify ((e:PolicyViolation)=>{ System.err.println("Say it's not so!! " + e.toString) })
  "age" is tpe[Int] default 45 ge 0 le 150

  "name" is tpe[String] default "wowbagger"
}

val conf = new Config(metaConfigExample)

assert(conf.require[Int]("a") == 42)
assert(conf.require[Long]("a") == 42L)
assert(conf.require[Double]("a") == 42.0)
assert(conf.require[Float]("a") == 42f)
assert(conf.require[String]("a") == "42")

conf.put("a", 7)
assert(conf.require[Int]("a") == 7)

// this will cause a message to stderr
conf.put("a", "!!!")
assert(conf.require[Int]("a") == 42)

conf.put("q", "foo")
assert(conf.get[String]("q") == Some("foo"))
assert(conf.require[String]("q") == "foo")
assert(conf.get[Int]("q") == None)

// this will cause a different message to stderr
conf.put("age", -1)
assert(conf.get[Int]("age") == None)

try {
  conf.put("radians", 7.0)
  // this will throw, due to customized notify for "radians"
  conf.get[Double]("radians")
} catch {
  case e: Throwable => println(e.toString)
}