import scala.collection.mutable
import scala.reflect.runtime.universe.{TypeTag, TypeRef, typeOf}

// A function that also keeps its domain and range types as a payload
class FunctionTP[D: TypeTag, R:TypeTag](f: D=>R) extends Function[D,R] {
  val domainType = typeOf[D]
  val rangeType = typeOf[R]
  val func = f
  override def apply(x: D):R = func(x)
  def compose[E:TypeTag](g: E=>D):FunctionTP[E,R] = new FunctionTP(func compose g)
  def andThen[S:TypeTag](g: R=>S):FunctionTP[D,S] = new FunctionTP(func andThen g)
}

// Generates a function that applies a function 'f'
// on payload of an Option[_], -- returns None if arg is None, or if
// f threw an exception
// (might be typically used for type casting from string to other types)
def typedPred[D:TypeTag,R:TypeTag](f:D=>R):FunctionTP[Option[D],Option[R]] = {
  new FunctionTP((d:Option[D]) => {
    val r:Option[R] = { d match {
      case Some(v) => {
        try {
          Some(f(v))
        } catch {
          case _ : Throwable => None
        }
      } 
      case None => None
    }}
    r
  })
}


def opLT[T](a:T, b:T)(implicit n: Numeric[T]):Boolean = { n.lt(a,b) }

def numPred[D: Numeric](t:D, test:(D,D)=>Boolean):Function[Option[D],Option[D]] = {
  (d:Option[D]) => {
    val r:Option[D] = d match {
      case Some(v) => {
        if (test(v, t)) Some(v)
        else None
      }
      case None => None
    }
    r
  }
}

// type checking and casting
val isInt = typedPred((x:String)=>x.toInt)
val isLong = typedPred((x:String)=>x.toLong)
val isDouble = typedPred((x:String)=>x.toDouble)
val isString = typedPred((x:String)=>x)

// boundary checking
def isLT[D:Numeric](t:D):Function[Option[D],Option[D]] = numPred(t, (vv:D,tt:D)=>opLT(vv,tt))
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

// holds a configuration, and imports configuration requirements
// (requirements are defined below)
class Config extends ConfigReqs {
  val conf: mutable.Map[String, String] = mutable.Map()

  def put[T](s:String, v:T, check:Boolean=true):Unit = {
    conf.put(s, v.toString)
    if (check) conf.get(s)
  }

  def get(s:String):Any = {
    val f = reqmap.getOrElse(s, isString)
    f(conf.get(s))
  }
}

// here is where you define policies for each config variable
// this could reside in its own file for easy maintenance
trait ConfigReqs {
  val reqmap:Map[String, FunctionTP[Option[String], _]] = Map(
    // an integer, >= 10
    "a" -> (isInt andThen isGE(10) andThen defaultTo(10)),

    // a double, > 0.0 and < 1.0
    "b" -> (isDouble andThen isGT(0.0) andThen isLT(1.0) andThen defaultTo(0.75)),

    // a long >= 0 and <= 100
    "c" -> (isLong andThen isOnClosed(0L, 100L) andThen defaultTo(50L)),

    // a string, default to "foo"
    "z" -> (isString andThen defaultTo("foo"))
  )
}

val conf = new Config
conf.put("a", 15)
println(conf.get("a"))
conf.put("a", 5)
println(conf.get("a"))
