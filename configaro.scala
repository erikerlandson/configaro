import scala.collection.mutable
import scala.reflect.runtime.universe.{TypeTag, TypeRef, typeOf}
import scala.language.implicitConversions

// 1st-class union types would be groovy here
abstract class ConfiguredValue
case class OptionLong(o:Option[Long]) extends ConfiguredValue
case class OptionDouble(o:Option[Double]) extends ConfiguredValue
case class OptionString(o:Option[String]) extends ConfiguredValue

trait CVToValConverter[@specialized(Long, Int, Double, Float) T] {
  def cast(cv:ConfiguredValue):T
}
class CVToLongConverter extends CVToValConverter[Long] {
  def cast(cv:ConfiguredValue):Long = cv match {
    case OptionLong(Some(v)) => v
    case OptionDouble(Some(v)) if v == v.toLong.toDouble => v.toLong
    case _ => throw new Exception
  }
}
class CVToIntConverter extends CVToValConverter[Int] {
  def cast(cv:ConfiguredValue):Int = cv match {
    case OptionLong(Some(v)) if v == v.toInt.toLong => v.toInt
    case OptionDouble(Some(v)) if v == v.toInt.toDouble => v.toInt
    case _ => throw new Exception
  }
}
class CVToDoubleConverter extends CVToValConverter[Double] {
  def cast(cv:ConfiguredValue):Double = cv match {
    case OptionLong(Some(v)) => v.toDouble
    case OptionDouble(Some(v)) => v
    case _ => throw new Exception
  }
}
class CVToFloatConverter extends CVToValConverter[Float] {
  def cast(cv:ConfiguredValue):Float = cv match {
    case OptionLong(Some(v)) => v.toFloat
    case OptionDouble(Some(v)) => v.toFloat
    case _ => throw new Exception
  }
}

object CVImplicits {
  // implicit import of option values into ConfiguredValue
  implicit def OptionToCV[T:TypeTag](o:Option[T]):ConfiguredValue = {
    val typeOptionLong = typeOf[Option[Long]]
    val typeOptionInt = typeOf[Option[Int]]
    val typeOptionDouble = typeOf[Option[Double]]
    val typeOptionFloat = typeOf[Option[Float]]
    val typeOptionString = typeOf[Option[String]]
    typeOf[Option[T]] match {
      case `typeOptionLong` => OptionLong(o.asInstanceOf[Option[Long]])
      case `typeOptionDouble` => OptionDouble(o.asInstanceOf[Option[Double]])
      case `typeOptionString` => OptionString(o.asInstanceOf[Option[String]])
      case `typeOptionInt` => OptionLong(o.asInstanceOf[Option[Int]].map(_.toLong))
      case `typeOptionFloat` => OptionDouble(o.asInstanceOf[Option[Float]].map(_.toDouble))
      case _ => throw new Exception
    }
  }

  implicit val CVToLong = new CVToLongConverter
  implicit val CVToInt = new CVToIntConverter
  implicit val CVToDouble = new CVToDoubleConverter
  implicit val CVToFloat = new CVToFloatConverter

  implicit def CVToString(cv: ConfiguredValue):String = {
    cv match {
      case OptionLong(Some(v)) => v.toString
      case OptionDouble(Some(v)) => v.toString
      case OptionString(Some(v)) => v
      case _ => throw new Exception
    }
  }
}

import CVImplicits._

def castPred[R:TypeTag](f:String=>R):Function[Option[String],ConfiguredValue] = {
  (d:Option[String]) => {
    val r:Option[R] = d match {
      case Some(v) => {
        try {
          Some(f(v))
        } catch {
          case _ : Throwable => None
        }
      } 
      case None => None
    }
    // leverage implicit casting functions here:
    val cv:ConfiguredValue = r
    cv
  }
}

// type checking and casting
val isLong = castPred((x:String)=>x.toLong)
val isDouble = castPred((x:String)=>x.toDouble)
val isString = castPred((x:String)=>x)

def test[@specialized(Long,Int,Double,Float)T:CVToValConverter](cv:ConfiguredValue):T = {
  val caster = implicitly[CVToValConverter[T]]
  caster.cast(cv)
}

def opLT[T](a:T, b:T)(implicit n: Numeric[T]):Boolean = { n.lt(a,b) }

def numPred[@specialized(Long,Double,Int,Float)D: CVToValConverter :TypeTag](t:D, test:(D,D)=>Boolean):Function[ConfiguredValue,ConfiguredValue] = {
  (d:ConfiguredValue) => {
    val caster = implicitly[CVToValConverter[D]]
    val r:Option[D] = try {
      val v:D = caster.cast(d)
      if (test(v, t)) Some(v)
      else None
    } catch {
      case _ : Throwable => None
    }
    val cv: ConfiguredValue = r
    cv
  }
}

// boundary checking
def isLT[D :CVToValConverter :Numeric :TypeTag](t:D):Function[ConfiguredValue,ConfiguredValue] = numPred(t, (vv:D,tt:D)=>opLT(vv,tt))


// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class Config(metaConfig: Map[String, Option[String] => ConfiguredValue] = Map()) {
  val conf: mutable.Map[String, String] = mutable.Map()

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    conf.put(s, v.toString)
    if (check) conf.get(s)
  }

  def get(s:String):ConfiguredValue = {
    val f = metaConfig.getOrElse(s, isString)
    f(conf.get(s))
  }

/*
  def getAs[T:TypeTag](s:String):T = {
    val f = metaConfig.getOrElse(s, isString)
    val r:T = f(conf.get(s))
    r
  }
*/
}

// here is where you define policies for each config variable
// this could reside in its own file for easy maintenance
val reqmap:Map[String, Option[String] => ConfiguredValue] = Map(
  "L" -> (isLong andThen isLT(10)),
  "D" -> (isDouble),
  "S" -> (isString)
)

val conf = new Config(reqmap)
conf.put("L", 42)
conf.put("D", 3.14)
conf.put("S", "octocat")

/*


def isLE[D:Numeric](t:D):Function[Option[D],Option[D]] = numPred(t, (vv:D,tt:D)=>(!opLT(tt,vv)))
def isGT[D:Numeric](t:D):Function[Option[D],Option[D]] = numPred(t, (vv:D,tt:D)=>opLT(tt,vv))
def isGE[D:Numeric](t:D):Function[Option[D],Option[D]] = numPred(t, (vv:D,tt:D)=>(!opLT(vv,tt)))

// interval checking
def isOnClosed[D:Numeric](l:D,u:D):Function[Option[D],Option[D]] = isGE(l) andThen isLE(u)
def isOnOpen[D:Numeric](l:D,u:D):Function[Option[D],Option[D]] = isGT(l) andThen isLT(u)

// default assignment
def defaultTo[D](dv:D):Function[Option[D],Option[D]] = {
  (d:Option[D]) => {
    d match {
      case Some(v) => Some(v)
      case None => Some(dv)
    }
  }
}
*/
