// Copyright (C) 2015 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
package s4m.exercise1

import s4m.smbd.api.BigDataFormat
import shapeless._
import org.scalatest._
import s4m.smbd.impl._

case class Teapot(a: String, b: Int, c: Boolean)

class StringyMapBigDataSpec extends FlatSpec with Matchers {

  "StringyMapBigData" should "marshall a case class" in {
    val teapot = Teapot("foo", 42, true)
    val format = implicitly[BigDataFormat[Teapot]]
    val stringyMap = format.toProperties(teapot)
    stringyMap.get("a") should be("foo")
    stringyMap.get("b") should be(42)
    stringyMap.get("c") shouldBe true

    format.fromProperties(stringyMap) should be(Right(teapot))
  }

}
