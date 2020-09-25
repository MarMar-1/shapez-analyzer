import Analysis._
import Shapez._
import org.scalatest.FunSuite

import scala.language.implicitConversions

class DetectorsTests extends FunSuite {
  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  test("shapes which can be made by simply stacking layers are freeplay") {
    assert(Detectors.isFreeplay("CcCcCcCc:RrRrRrRr"))
    assert(Detectors.isFreeplay("Cc------:RrRrRrRr"))
    assert(Detectors.isFreeplay("Cc----Cc:------Rr"))
    assert(Detectors.isFreeplay("Cc----Cc:------Rr:SgSg--Sg:--Ww----"))
  }
  test("floating and drop-through shapes are not freeplay") {
    assert(!Detectors.isFreeplay("CcCc--Cc:----Rr--"))
    assert(!Detectors.isFreeplay("Cc------:--RrRrRr"))
    assert(!Detectors.isFreeplay("Cc----Cc:--RrRr--"))
    assert(!Detectors.isFreeplay("Cc----Cc:----Rr--:SgSg--Sg:----Ww--"))
  }
  test("empty shape is freeplay") {
    assert(Detectors.isFreeplay("--------"))
  }
  test("freeplay shapes are not simple floating") {
    assert(!Detectors.isSimpleFloating("CcCcCcCc:RrRrRrRr"))
    assert(!Detectors.isSimpleFloating("Cc------:RrRrRrRr"))
    assert(!Detectors.isSimpleFloating("Cc----Cc:------Rr"))
    assert(!Detectors.isSimpleFloating("Cc----Cc:------Rr:SgSg--Sg:--Ww----"))
  }
  test("logo-like shapes are simple floating") {
    assert(Detectors.isSimpleFloating("CcCcCc--:------Rr"))
    assert(Detectors.isSimpleFloating("Cc--Cc--:--Rr--Rr"))
    assert(Detectors.isSimpleFloating("--CcCc--:Rr----Rr"))
    assert(Detectors.isSimpleFloating("Cc----Cc:--RrRr--"))
    assert(Detectors.isSimpleFloating("CcCc----:----RrRr"))
    assert(Detectors.isSimpleFloating("Cc------:------Rr"))
    assert(Detectors.isSimpleFloating("--Cc----:----Rr--"))
  }
  test("logo-like shapes anywhere among freeplay layers are simple floating") {
    assert(Detectors.isSimpleFloating("Cc--Cc--:--Rr--Rr:WwWwWwWw:WwWwWwWw"))
    assert(Detectors.isSimpleFloating("WwWwWwWw:Cc--Cc--:--Rr--Rr:WwWwWwWw"))
    assert(Detectors.isSimpleFloating("WwWwWwWw:WwWwWwWw:Cc--Cc--:--Rr--Rr"))
  }
  test("impossible floating shapes are not simple floating") {
    assert(!Detectors.isSimpleFloating("Cc------:----Rr--"))
    assert(!Detectors.isSimpleFloating("Cc------:----RrRr"))
    assert(!Detectors.isSimpleFloating("Cc------:--RrRrRr"))
  }
  test("drop-through shapes are not simple floating") {
    assert(!Detectors.isSimpleFloating("Cc----Cc:----Rr--:SgSg--Sg:----Ww--"))
    assert(!Detectors.isSimpleFloating("Cc------:----RrRr:----Ww--"))
  }
  test("multi-layer scaffolding shapes are not simple floating") {
    assert(!Detectors.isSimpleFloating("Cc------:--Rr----:Ww------:--Sg----"))
    assert(!Detectors.isSimpleFloating("CcCc--Cc:--Rr----:Ww------:--Sg----"))
  }
  test("simple floating shapes are not dropthrough") {
    assert(!Detectors.isDropThrough("CcCcCc--:------Rr"))
    assert(!Detectors.isDropThrough("Cc------:------Rr"))
    assert(!Detectors.isDropThrough("--Cc----:----Rr--"))
    assert(!Detectors.isDropThrough("Cc--Cc--:--Rr--Rr:WwWwWwWw:WwWwWwWw"))
    assert(!Detectors.isDropThrough("WwWwWwWw:Cc--Cc--:--Rr--Rr:WwWwWwWw"))
    assert(!Detectors.isDropThrough("WwWwWwWw:WwWwWwWw:Cc--Cc--:--Rr--Rr"))
  }
  test("impossible floating shapes are not dropthrough") {
    assert(!Detectors.isDropThrough("Cc------:----Rr--"))
    assert(!Detectors.isDropThrough("Cc------:----RrRr"))
    assert(!Detectors.isDropThrough("Cc------:--RrRrRr"))
  }
  test("drop-through shapes are dropthrough") {
    assert(Detectors.isDropThrough("Cc----Cc:----Rr--:SgSg--Sg:----Ww--"))
    assert(Detectors.isDropThrough("Cc------:----RrRr:----Ww--"))
  }
  test("multi-layer scaffolding shapes are not dropthrough") {
    assert(!Detectors.isDropThrough("Cc------:--Rr----:Ww------:--Sg----"))
    assert(!Detectors.isDropThrough("CcCc--Cc:--Rr----:Ww------:--Sg----"))
  }
}