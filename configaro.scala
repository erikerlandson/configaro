
import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.runtime.universe.{TypeTag, TypeRef, typeOf}

// a function that supports "|" operator
class PF[D,R](f:D=>R) extends Function[D,R] {
  val func = f
  def apply(d:D):R = func(d)
  def |[S](g:R=>S):PF[D,S] = new PF(func andThen g)
}

case class ConfigaroConversionException(message:String) extends Exception(message)

def conversionExceptionMessage[T:TypeTag](o: Option[T]):String = {
  val tString = typeOf[T].toString
  val vString = o match {
    case Some(v) => "Some(" + v.toString + ")"
    case None => "None"
  }
  s"Cannot convert Option[$tString] = $vString"
}

// a configured value, for supporting type conversions
class CV[+T:TypeTag](op:Option[T]) {
  val o = op

  def toLong:Long = o match {
    case Some(v: Long) => v
    case Some(v: Double) if v == v.toLong.toDouble => v.toLong
    case _ => throw new ConfigaroConversionException(conversionExceptionMessage(o))
  }

  def toInt:Int = o match {
    case Some(v: Long) if v == v.toInt.toLong => v.toInt
    case Some(v: Double) if v == v.toInt.toDouble => v.toInt
    case _ => throw new ConfigaroConversionException(conversionExceptionMessage(o))
  }

  def toDouble:Double = o match {
    case Some(v: Long) => v.toDouble
    case Some(v:Double) => v
    case _ => throw new ConfigaroConversionException(conversionExceptionMessage(o))
  }

  def toFloat:Float = o match {
    case Some(v: Long) => v.toFloat
    case Some(v:Double) => v.toFloat
    case _ => throw new ConfigaroConversionException(conversionExceptionMessage(o))
  }

  override def toString:String = o match {
    case Some(v) => v.toString
    // This is a compromise.  If I throw an exception here, like the toXxx above, it
    // will cause exception when something like conf.get("undefined") is invoked 
    // inside the REPL.  Also, this at least respects the convention that any value
    // can be converted to String.  
    // Maybe there is a better solution?
    case None => "None"
  }
}

object Convert {
  implicit def cvToOption[T](cv:CV[T]):Option[T] = cv.o

  implicit def cvToLong[T:TypeTag](cv:CV[T]):Long = cv.toLong
  implicit def cvToInt[T:TypeTag](cv:CV[T]):Int = cv.toInt
  implicit def cvToDouble[T:TypeTag](cv:CV[T]):Double = cv.toDouble
  implicit def cvToFloat[T:TypeTag](cv:CV[T]):Float = cv.toFloat
  implicit def cvToString[T:TypeTag](cv:CV[T]):String = cv.toString
}
import Convert._


// Generates a function that applies a function 'f' to option payloads
def typedPred[D,R:TypeTag](f:D=>R):PF[Option[D],CV[R]] = {
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
def defaultTo[D:TypeTag](dv:D):Function[CV[D],CV[D]] = {
  (d:CV[D]) => new CV(d.o match {
      case Some(v) => Some(v)
      case None => Some(dv)
    }
  )
}

def opLT[T](a:T, b:T)(implicit n: Numeric[T]):Boolean = { n.lt(a,b) }

def numTestPred[D:Numeric :TypeTag](t:D, test:(D,D)=>Boolean):(CV[D]=>CV[D]) = {
  (d:CV[D]) => new CV(d.o match {
      case Some(v) => {
        if (test(v, t)) Some(v)
        else None
      }
      case None => None
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
def isLT[D:Numeric :TypeTag](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>opLT(vv,tt))
def isLE[D:Numeric :TypeTag](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>(!opLT(tt,vv)))
def isGT[D:Numeric :TypeTag](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>opLT(tt,vv))
def isGE[D:Numeric :TypeTag](t:D):(CV[D]=>CV[D]) = numTestPred(t, (vv:D,tt:D)=>(!opLT(vv,tt)))

type MetaConfiguration = Map[String, Function[Option[String], CV[_]]]

// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class Config(mc: MetaConfiguration) {
  val metaConfig = mc 
  val conf: mutable.Map[String, String] = mutable.Map()

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    conf.put(s, v.toString)
    if (check) conf.get(s)
  }

  def get(s:String):CV[Any] = {
    val f = metaConfig.getOrElse(s, isString)
    f(conf.get(s))
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
