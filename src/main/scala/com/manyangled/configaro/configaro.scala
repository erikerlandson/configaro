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

package com.manyangled.configaro

import scala.collection.mutable
import scala.language.implicitConversions
import scala.language.postfixOps

// exception thrown when a requested type conversion fails
case class ConversionException(message:String) extends Exception(message)

// exception thrown by a core filter function if the incoming value
// did not satisfy some intended property.
sealed class PolicyViolation(val message:String) extends Exception(message)

case class TypePolicyViolation(msg:String) extends PolicyViolation(msg)
case class BoundPolicyViolation(msg:String) extends PolicyViolation(msg)
case class RegexPolicyViolation(msg:String) extends PolicyViolation(msg)

// enable PolicyViolation to act like a case class inside a match construct
object PolicyViolation {
  def unapply(pv:PolicyViolation):Option[String] = Some(pv.message)
}


private [configaro] object OutputConversions {

  def conversionMessage(v:Any):String = {
    val tString = v.getClass.getName
    s"Failed to convert $tString = $v to the requested type"
  }

  trait ConvertOptionToV[V] {
    def convert(cv:Any):V
  }

  implicit val objectOptionToInt = new ConvertOptionToV[Int] {
    def convert(cv:Any):Int = try { cv match {
        case v: Int => v
        case v: Long if v == v.toInt.toLong => v.toInt
        case v: Float if v == v.toInt.toFloat => v.toInt
        case v: Double if v == v.toInt.toDouble => v.toInt
        case v: String => v.toInt
        case _ => throw new Exception
      }
    } catch {
      case _: Exception => throw new ConversionException(conversionMessage(cv))
    }  
  }
  implicit val objectOptionToLong = new ConvertOptionToV[Long] {
    def convert(cv:Any):Long = try { cv match {
        case v: Int => v.toLong
        case v: Long => v
        case v: Float if v == v.toLong.toFloat => v.toLong
        case v: Double if v == v.toLong.toDouble => v.toLong
        case v: String => v.toLong
        case _ => throw new Exception
      }
    } catch {
      case _: Exception => throw new ConversionException(conversionMessage(cv))
    }
  }
  implicit val objectOptionToFloat = new ConvertOptionToV[Float] {
    def convert(cv:Any):Float = try { cv match {
        case v: Int => v.toFloat
        case v: Long => v.toFloat
        case v: Float => v
        case v: Double => v.toFloat
        case v: String => v.toFloat
        case _ => throw new Exception
      }
    } catch {
      case _: Exception => throw new ConversionException(conversionMessage(cv))
    }
  }
  implicit val objectOptionToDouble = new ConvertOptionToV[Double] {
    def convert(cv:Any):Double = try { cv match {
        case v: Int => v.toDouble
        case v: Long => v.toDouble
        case v: Float => v.toDouble
        case v: Double => v
        case v: String => v.toDouble
        case _ => throw new Exception
      }
    } catch {
      case _: Exception => throw new ConversionException(conversionMessage(cv))
    }
  }
  implicit val objectOptionToString = new ConvertOptionToV[String] {
    def convert(cv:Any):String = try {
      cv.toString
    } catch {
      case _: Exception => throw new ConversionException(conversionMessage(cv))
    }
  }

}

import OutputConversions._

object PropertyPolicy {
  // A 'notifier' is a client-configured function for handling
  // (or "notifying") policy violations from component filters.
  // Examples of policy violations are failure to convert a string to a type,
  // or failing to satisfy some predicate, such as 'value > threshold'.
  type Notifier = PolicyViolation => Unit

  private [configaro] type PolicyFunction = Function[Option[String], Option[Any]]

  implicit def convertToMap(pp:PropertyPolicy):Map[String, PolicyFunction] = pp.map.toMap
}

trait PropertyPolicy {
  type Notifier = PropertyPolicy.Notifier
  type PolicyFunction = PropertyPolicy.PolicyFunction
  
  private type Regex = scala.util.matching.Regex

  val policy = this

  private [configaro] val map: mutable.Map[String, PolicyFunction] = mutable.Map()

  trait TypeConverter[T] {
    def func: String => T
  }

  implicit val tcLong = new TypeConverter[Long] {
    def func = (s:String) => try { s.toLong } catch { case _ :Exception => throw new TypePolicyViolation(conversionMessage(s)) }
  }
  implicit val tcInt = new TypeConverter[Int] {
    def func = (s:String) => try { s.toInt } catch { case _ :Exception => throw new TypePolicyViolation(conversionMessage(s)) }
  }
  implicit val tcDouble = new TypeConverter[Double] {
    def func = (s:String) => try { s.toDouble } catch { case _ :Exception => throw new TypePolicyViolation(conversionMessage(s)) }
  }
  implicit val tcFloat = new TypeConverter[Float] {
    def func = (s:String) => try { s.toFloat } catch { case _ :Exception => throw new TypePolicyViolation(conversionMessage(s)) }
  }
  implicit val tcString = new TypeConverter[String] {
    def func = (s:String) => try { s.toString } catch { case _ :Exception => throw new TypePolicyViolation(conversionMessage(s)) }
  }

  private var notifierGlobal:Notifier = (e:PolicyViolation)=>{}

  def notify(n:Notifier):Unit = { notifierGlobal = n }

  // wrap embodies the core meta-config model: 
  // A meta policy for a parameter is a function from Option[String] => Option[T],
  // usually composed of multiple composed filtering functions.
  // A core filter function is applied to an Option using map, so None (missing value)
  // is never an error, it is simply passed along.
  // If a core filter throws type PolicyViolation, that signals the incoming value
  // violated some policy for that filter.  In this case, a 'notifier' is called for
  // that PolicyViolation instance, which is client specified (it might log a message,
  // throw an exception for 'fatal' behavior, etc) and defaults to 'do nothing'
  // Once the notifier is invoked, None is always returned (assuming the notifier did
  // not throw)
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

  private [configaro] class Context[R](val key:String, val func:Option[String]=>Option[R], var notifier:Notifier) {

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
    // Option and violation logic
    def pipe[S>:R,T](g:S=>T):Context[T] = comp(wrap(g, notifier))

    // for accepting type conversion/constraint predicates
    // i.e.  is tpe[Long]
    def is[S](g:R=>S):Context[S] = pipe(g)

    // replace any missing value (None) with a default
    def default[T <: R](dv:T):Context[R] = comp(
      (d:Option[R]) => d match {
        case Some(v) => Some(v)
        case None => Some(dv)
      })

    // boundary checking
    def lt[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (!opLT(x,t)) throw new BoundPolicyViolation(s"$x >= $t")
      x})
    def le[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (opLT(t,x)) throw new BoundPolicyViolation(s"$x > $t")
      x})
    def gt[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (!opLT(t,x)) throw new BoundPolicyViolation(s"$x <= $t")
      x})
    def ge[D >: R :Numeric](t:D):Context[D] = pipe((x:D) => {
      if (opLT(x,t)) throw new BoundPolicyViolation(s"$x < $t")
      x})

    def regex[S >: R  <: String](r:Regex):Context[S] = pipe((v:S) => {
      if (!(r.findFirstIn(v).nonEmpty)) throw new RegexPolicyViolation(s"String '$v' did not match regex '${r.toString}'")
      v
    })
  }

  implicit def stringToContext(s:String):Context[String] = {
    val id = identity[Option[String]]_
    map(s) = id
    new Context(s, id, notifierGlobal)
  }

  implicit def stringToRegex(s:String):Regex = s.r
}

// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class PropertyMap(val policy: PropertyPolicy) {
  val properties: mutable.Map[String, String] = mutable.Map()

  def apply(s:String):Option[Any] = {
    policy.getOrElse(s, identity[Option[String]]_)(properties.get(s))
  }

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    properties.put(s, v.toString)
  }

  // get operates like get usually does for a Map, except that a
  // specific requested type must be specified, since parameters can
  // have different output types from meta-config policies
  // e.g. conf.get[Int]("param")  -->  Option[Int] = Some(int-value) or None
  def get[T:ConvertOptionToV](s:String):Option[T] = {
    try {
      this(s).map(implicitly[ConvertOptionToV[T]].convert) 
    } catch {
      // A failure to convert to a supported type goes to None,
      // for consistency with behavior of meta-policy type filters
      case ConversionException(_) => None
    }
  }

  // like map.getOrElse, but requires a type:
  // e.g. conf.getOrElse[Float]("param", 4.5f)  -->  some-int-value
  def getOrElse[T:ConvertOptionToV](s:String, d:T):T = {
    get[T](s).getOrElse(d)
  }

  // Requires param to evaluate to the requested type.  In this case,
  // Neither conversion failures nor missing values (None) are tolerated
  // e.g. conf.require[Int]("param")  --> int-value (or die trying)
  def require[T:ConvertOptionToV](s:String):T = {
    this(s) match {
      case Some(v) => implicitly[ConvertOptionToV[T]].convert(v)
      case None => throw new ConversionException(conversionMessage(None))
    }
  }
}
