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

import org.scalatest.FunSuite

class PropertyPolicySuite extends FunSuite {
  test("declare empty policy") {
    object policy extends PropertyPolicy {
    }
    assert(policy.isEmpty)
  }

  test("implicit String property") {
    object policy extends PropertyPolicy {
      "a" pipe identity[String]_
    }
    // smoke test basic map properties
    assert(policy.size == 1)
    assert(policy.get("a").nonEmpty)
    assert(policy.get("b") == None)

    // string type properties
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("xxx")) == Some("xxx"))
  }

  test("String property") {
    object policy extends PropertyPolicy {
      "a" is tpe[String]
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("xxx")) == Some("xxx"))    
  }

  test("Int property") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("3")) == Some(3))
    assert(policy("a")(Some("x")) == None)
  }

  test("Long property") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long]
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("3")) == Some(3L))
    assert(policy("a")(Some("x")) == None)
  }

  test("Float property") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float]
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("3")) == Some(3.0f))
    assert(policy("a")(Some("x")) == None)
  }

  test("Double property") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("3")) == Some(3.0))
    assert(policy("a")(Some("x")) == None)
  }

  test("String default") {
    object policy extends PropertyPolicy {
      "a" default "foo"
    }
    assert(policy("a")(None) == Some("foo"))
    assert(policy("a")(Some("xxx")) == Some("xxx"))
  }

  test("Int default") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int] default 7
    }
    assert(policy("a")(None) == Some(7))
    assert(policy("a")(Some("3")) == Some(3))
    assert(policy("a")(Some("x")) == Some(7))
  }

  test("Long default") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long] default 7L
    }
    assert(policy("a")(None) == Some(7L))
    assert(policy("a")(Some("3")) == Some(3L))
    assert(policy("a")(Some("x")) == Some(7L))
  }

  test("Float default") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float] default 7.0f
    }
    assert(policy("a")(None) == Some(7.0f))
    assert(policy("a")(Some("3")) == Some(3.0f))
    assert(policy("a")(Some("x")) == Some(7.0f))
  }

  test("Double default") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double] default 7.0
    }
    assert(policy("a")(None) == Some(7.0))
    assert(policy("a")(Some("3")) == Some(3.0))
    assert(policy("a")(Some("x")) == Some(7.0))
  }

  test("bound lt Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   lt 7
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5))
    assert(policy("a")(Some("6")) == Some(6))
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound le Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   le 7
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5))
    assert(policy("a")(Some("6")) == Some(6))
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   gt 7
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == Some(8))
    assert(policy("a")(Some("9")) == Some(9))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   ge 7
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == Some(8))
    assert(policy("a")(Some("9")) == Some(9))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound lt Long") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long]   lt 7L
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5L))
    assert(policy("a")(Some("6")) == Some(6L))
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound le Long") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long]   le 7L
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5L))
    assert(policy("a")(Some("6")) == Some(6L))
    assert(policy("a")(Some("7")) == Some(7L))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt Long") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long]   gt 7L
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == Some(8L))
    assert(policy("a")(Some("9")) == Some(9L))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge Long") {
    object policy extends PropertyPolicy {
      "a" is tpe[Long]   ge 7L
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7L))
    assert(policy("a")(Some("8")) == Some(8L))
    assert(policy("a")(Some("9")) == Some(9L))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound lt Float") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float]   lt 7f
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5f))
    assert(policy("a")(Some("6")) == Some(6f))
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound le Float") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float]   le 7f
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5f))
    assert(policy("a")(Some("6")) == Some(6f))
    assert(policy("a")(Some("7")) == Some(7f))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt Float") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float]   gt 7f
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == Some(8f))
    assert(policy("a")(Some("9")) == Some(9f))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge Float") {
    object policy extends PropertyPolicy {
      "a" is tpe[Float]   ge 7f
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7f))
    assert(policy("a")(Some("8")) == Some(8f))
    assert(policy("a")(Some("9")) == Some(9f))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound lt Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   lt 7.0
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5.0))
    assert(policy("a")(Some("6")) == Some(6.0))
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound le Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   le 7.0
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == Some(5.0))
    assert(policy("a")(Some("6")) == Some(6.0))
    assert(policy("a")(Some("7")) == Some(7.0))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   gt 7.0
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == None)
    assert(policy("a")(Some("8")) == Some(8.0))
    assert(policy("a")(Some("9")) == Some(9.0))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   ge 7.0
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7.0))
    assert(policy("a")(Some("8")) == Some(8.0))
    assert(policy("a")(Some("9")) == Some(9.0))
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt lt Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   gt 6   lt 8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt le Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   gt 6   le 8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == None)
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == Some(8))
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge lt Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   ge 6   lt 8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == Some(6))
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == None)
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge le Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int]   ge 6   le 8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("5")) == None)
    assert(policy("a")(Some("6")) == Some(6))
    assert(policy("a")(Some("7")) == Some(7))
    assert(policy("a")(Some("8")) == Some(8))
    assert(policy("a")(Some("9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt lt Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   gt 1.6   lt 1.8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("1.5")) == None)
    assert(policy("a")(Some("1.6")) == None)
    assert(policy("a")(Some("1.7")) == Some(1.7))
    assert(policy("a")(Some("1.8")) == None)
    assert(policy("a")(Some("1.9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound gt le Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   gt 1.6   le 1.8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("1.5")) == None)
    assert(policy("a")(Some("1.6")) == None)
    assert(policy("a")(Some("1.7")) == Some(1.7))
    assert(policy("a")(Some("1.8")) == Some(1.8))
    assert(policy("a")(Some("1.9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge lt Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   ge 1.6   lt 1.8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("1.5")) == None)
    assert(policy("a")(Some("1.6")) == Some(1.6))
    assert(policy("a")(Some("1.7")) == Some(1.7))
    assert(policy("a")(Some("1.8")) == None)
    assert(policy("a")(Some("1.9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("bound ge le Double") {
    object policy extends PropertyPolicy {
      "a" is tpe[Double]   ge 1.6   le 1.8
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("1.5")) == None)
    assert(policy("a")(Some("1.6")) == Some(1.6))
    assert(policy("a")(Some("1.7")) == Some(1.7))
    assert(policy("a")(Some("1.8")) == Some(1.8))
    assert(policy("a")(Some("1.9")) == None)
    assert(policy("a")(Some("x")) == None)
  }

  test("regex") {
    object policy extends PropertyPolicy {
      "a" regex """^[a-z]+$""".r
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("")) == None)
    assert(policy("a")(Some("q")) == Some("q"))
    assert(policy("a")(Some("qr")) == Some("qr"))
    assert(policy("a")(Some("Q")) == None)
    assert(policy("a")(Some("qR")) == None)
    assert(policy("a")(Some("Qr")) == None)
  }

  test("regex from string") {
    object policy extends PropertyPolicy {
      "a" regex """^[a-z]+$"""
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("")) == None)
    assert(policy("a")(Some("q")) == Some("q"))
    assert(policy("a")(Some("qr")) == Some("qr"))
    assert(policy("a")(Some("Q")) == None)
    assert(policy("a")(Some("qR")) == None)
    assert(policy("a")(Some("Qr")) == None)
  }

  test("pipe custom Int") {
    object policy extends PropertyPolicy {
      "a" is tpe[Int] pipe ((x:Int)=>10+x)
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("1")) == Some(11))
    assert(policy("a")(Some("7")) == Some(17))
    assert(policy("a")(Some("x")) == None)
  }

  test("pipe custom String") {
    object policy extends PropertyPolicy {
      "a" pipe ((x:String)=>"x"+x)
    }
    assert(policy("a")(None) == None)
    assert(policy("a")(Some("y")) == Some("xy"))
    assert(policy("a")(Some("z")) == Some("xz"))
  }
}
