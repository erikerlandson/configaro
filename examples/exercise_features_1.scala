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


import com.manyangled.configaro._


// Example of a custom filter.  Tests for a proper name format.
// If incoming value meets the format, it is passed along, oetherwise a PolicyViolation
// is thrown to signal the failure.
def properName(v:String):String = {
   if (!("""^[A-Z][a-z]+$""".r.findFirstIn(v).nonEmpty)) throw new PolicyViolation(s"string $v is not proper name")
   v
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

  // pipe in a custom filter function
  // note that Option[] layer boilerplate is added automatically
  // type defaults to string: equivalent to 'is tpe[String]'
  "name" default "Wowbagger" pipe properName

  // regex accepts either a Regex type or a string to be converted to Regex
  "lastname" regex """^[A-Z][a-z]+$"""
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
  case e: Exception => println(e.toString)
}

assert(conf.get[String]("name") == Some("Wowbagger"))
conf.put("name", "zaphod")
// this will cause a warning message
assert(conf.get[String]("name") == None)

conf.put("lastname", "Beeblebrox")
assert(conf.require[String]("lastname") == "Beeblebrox")
// will cause a message to stderr:
conf.put("lastname", "beeblebrox")
assert(conf.get[String]("lastname") == None)
