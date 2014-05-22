
import scala.collection.mutable
import scala.language.implicitConversions

// a function that supports "|" operator as shorthand for andThen
class PF[D,R](f:D=>R) extends Function[D,R] {
  val func = f
  def apply(d:D):R = func(d)
  def |[S](g:R=>S):PF[D,S] = new PF(func andThen g)
}

// a configured value, for supporting type conversions
class CV[+T](op:Option[T]) {
  val o = op
  def isNone:Boolean = o match {
    case None => true
    case _ => false
  }
}

case class ConfigaroConversionException(message:String) extends Exception(message)

def conversionExceptionMessage[T](o: Option[T]):String = {
  val (tString, vString) = o match {
    case Some(v) => (v.getClass.getName, v.toString) 
    case None => ("None", "None")
  }
  s"Failed to convert CV[$tString] = $vString to the requested type"
}

trait ConvertCVToV[V] {
  def convert(cv:CV[Any]):V
}

object ConvertCVToVObjects {
  implicit val objectCVToLong = new ConvertCVToV[Long] {
    def convert(cv:CV[Any]):Long = cv.o match {
      case Some(v: Long) => v
      case Some(v: Double) if v == v.toLong.toDouble => v.toLong
      case _ => throw new ConfigaroConversionException(conversionExceptionMessage(cv.o))
    }
  }
  implicit val objectCVToInt = new ConvertCVToV[Int] {
    def convert(cv:CV[Any]):Int = cv.o match {
      case Some(v: Long) if v == v.toInt.toLong => v.toInt
      case Some(v: Double) if v == v.toInt.toDouble => v.toInt
      case _ => throw new ConfigaroConversionException(conversionExceptionMessage(cv.o))
    }
  }
  implicit val objectCVToDouble = new ConvertCVToV[Double] {
    def convert(cv:CV[Any]):Double = cv.o match {
      case Some(v: Long) => v.toDouble
      case Some(v:Double) => v
      case _ => throw new ConfigaroConversionException(conversionExceptionMessage(cv.o))
    }
  }
  implicit val objectCVToFloat = new ConvertCVToV[Float] {
    def convert(cv:CV[Any]):Float = cv.o match {
      case Some(v: Long) => v.toFloat
      case Some(v:Double) => v.toFloat
      case _ => throw new ConfigaroConversionException(conversionExceptionMessage(cv.o))
    }
  }
  implicit val objectCVToString = new ConvertCVToV[String] {
    def convert(cv:CV[Any]):String = cv.o match {
      case Some(v) => v.toString
      case _ => throw new ConfigaroConversionException(conversionExceptionMessage(cv.o))
    }
  }
}

import ConvertCVToVObjects._

def convertCVToV[V:ConvertCVToV](cv:CV[Any]):V = {
  val conv = implicitly[ConvertCVToV[V]]
  conv.convert(cv)
}

object Convert {
/*
  Scala does not seem to handle generic implicit conversion return types.
  It compiles, but it never matches at run time
  implicit def convertCVToV[V:ConvertCVToV](cv:CV[Any]):V = {
    val conv = implicitly[ConvertCVToV[V]]
    conv.convert(cv)
  }
*/

  implicit def convertCVToLong(cv:CV[Any]):Long = objectCVToLong.convert(cv)
  implicit def convertCVToInt(cv:CV[Any]):Int = objectCVToInt.convert(cv)
  implicit def convertCVToDouble(cv:CV[Any]):Double = objectCVToDouble.convert(cv)
  implicit def convertCVToFloat(cv:CV[Any]):Float = objectCVToFloat.convert(cv)
  implicit def convertCVToString(cv:CV[Any]):String = objectCVToString.convert(cv)
}
import Convert._


// Generates a function that applies a function 'f' to option payloads
def typedPred[D,R](f:D=>R):PF[Option[D],CV[R]] = {
  new PF((d:Option[D]) => new CV(try {
    d match {
      case Some(v) => Some(f(v))
      case None => throw new Exception
    }
  } catch {
    // support alternate failure response policies here
    case _ : Throwable => None
  }))
}

// default assignment
def defaultTo[D](dv:D):Function[CV[D],CV[D]] = {
  (d:CV[D]) => new CV(d.o match {
      case Some(v) => Some(v)
      case None => Some(dv)
    }
  )
}

def opLT[T](a:T, b:T)(implicit n: Numeric[T]):Boolean = { n.lt(a,b) }

def numTestPred[D:Numeric](t:D, test:(D,D)=>Boolean):(CV[D]=>CV[D]) = {
  (d:CV[D]) => new CV(
    try {
      d.o match {
        case Some(v) if (test(v, t)) => Some(v)
        case _ => throw new Exception
      }
    } catch {
      // support alternate failure response policies here
      case _: Throwable => None
    }
  )
}

// type checking and casting
// ignoring Int and Float as internal types results in
// shorter case statements for internal conversions, however
// it might be worth adding them in, particularly Int, since
// you can say defaultTo(7) instead of defaultTo(7L)
val isLong = typedPred((x:String)=>x.toLong)
val isDouble = typedPred((x:String)=>x.toDouble)
val isString = typedPred((x:String)=>x)

// boundary checking
def isLT[D:Numeric](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>opLT(vv,tt))
def isLE[D:Numeric](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>(!opLT(tt,vv)))
def isGT[D:Numeric](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>opLT(tt,vv))
def isGE[D:Numeric](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>(!opLT(vv,tt)))

type MetaConfiguration = Map[String, Function[Option[String], CV[_]]]

// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class Config(mc: MetaConfiguration) extends Function[String, CV[Any]] {
  val metaConfig = mc
  val conf: mutable.Map[String, String] = mutable.Map()

  def apply(s:String):CV[Any] = {
    metaConfig.getOrElse(s, isString)(conf.get(s))
  }

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    conf.put(s, v.toString)
  }

  def get[T:ConvertCVToV](s:String):Option[T] = {
    val conv = implicitly[ConvertCVToV[T]]
    val cv = this(s)
    if (cv.isNone) None else Some(conv.convert(cv))
  }

  def getOrElse[T:ConvertCVToV](s:String, d:T):T = {
    get[T](s).getOrElse(d)
  }

  def require[T:ConvertCVToV](s:String):T = {
    val conv = implicitly[ConvertCVToV[T]]
    conv.convert(this(s))
  }
}

// here is where you define policies for each config variable
// this could reside in its own file for easy maintenance
val metaConfigExample:MetaConfiguration = Map(
  "a" -> (isLong | defaultTo(42L) | isGE(0L)),
 
  "b" -> (isDouble | defaultTo(3.14) | isGE(0.0) | isLT(6.28)),

  "c" -> (isLong | defaultTo(1L) | isGT(0L)),

  "z" -> (isString | defaultTo("wowbagger"))
)

val conf = new Config(metaConfigExample)

assert(conf.require[Int]("a") == 42)
assert(conf.require[Long]("a") == 42L)
assert(conf.require[Double]("a") == 42.0)
assert(conf.require[Float]("a") == 42f)
assert(conf.require[String]("a") == "42")

conf.put("a", 7)
assert(conf.require[Int]("a") == 7)

conf.put("a", -1)
assert(conf.get[Int]("a") == None)

assert(conf.getOrElse[Int]("a", 8) == 8)
