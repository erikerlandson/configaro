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
}
