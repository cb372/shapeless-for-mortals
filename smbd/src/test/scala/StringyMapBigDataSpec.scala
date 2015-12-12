// Copyright (C) 2015 Sam Halliday
// License: http://www.apache.org/licenses/LICENSE-2.0
package s4m.exercise1

import s4m.smbd.api.BigDataFormat
import shapeless._
import org.scalatest._
import s4m.smbd.impl._

sealed trait Receptacle
case class Teapot(a: String, b: Int, c: Boolean) extends Receptacle
case class Bottle(foo: String, bar: Double) extends Receptacle
case object Glass extends Receptacle

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

  "StringyMapBigData" should "marshall a sealed trait" in {
    val teapot = Teapot("foo", 42, true)
    val bottle = Bottle("hello", 1.23)
    val glass = Glass
    val format = implicitly[BigDataFormat[Receptacle]]
    format.fromProperties(format.toProperties(teapot)) should be(Right(teapot))
    format.fromProperties(format.toProperties(bottle)) should be(Right(bottle))
    format.fromProperties(format.toProperties(glass)) should be(Right(glass))
  }

}
