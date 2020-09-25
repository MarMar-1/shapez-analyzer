import Shapez._
import org.scalatest.FunSuite

class StackingTest extends FunSuite {
  test("overlapping layers stack") {
    assert(Layer(Circle()) stacks Layer(Circle()))
    assert(Layer(br = Rectangle()) stacks Layer(br = Star()))
    assert(Layer(bl = Windmill(), tr = Rectangle()) stacks Layer(bl = Star(), tl = Circle()))
    assert(Layer(Circle(), Circle(), Circle(), Circle()) stacks Layer(Circle(), Circle(), Circle(), Circle()))
  }
  test("non-overlapping layers don't stack") {
    assert(!(Layer(tl = Circle()) stacks Layer(tr = Rectangle())))
    assert(!(Layer(tl = Circle(), br = Circle()) stacks Layer(tr = Rectangle(), bl = Star())))
  }
  test("basic shapes add by stacking layers") {
    assert(
      CreateShape.fromString("CcCcCcCc") +
        CreateShape.fromString("RrRrRrRr") ==
        CreateShape.fromString("CcCcCcCc:RrRrRrRr"))
    assert(
      CreateShape.fromString("CcCc--Cc") +
        CreateShape.fromString("Rr----Rr") ==
        CreateShape.fromString("CcCc--Cc:Rr----Rr"))
    assert(
      CreateShape.fromString("CcCcCcCc:CcCcCcCc") +
        CreateShape.fromString("RrRrRrRr") ==
        CreateShape.fromString("CcCcCcCc:CcCcCcCc:RrRrRrRr"))
    assert(
      CreateShape.fromString("CcCcCcCc:CcCcCcCc:CcCcCcCc") +
        CreateShape.fromString("RrRrRrRr") ==
        CreateShape.fromString("CcCcCcCc:CcCcCcCc:CcCcCcCc:RrRrRrRr"))
    assert(
      CreateShape.fromString("CcCcCcCc:CcCcCcCc:CcCcCcCc:CcCcCcCc") +
        CreateShape.fromString("RrRrRrRr") ==
        CreateShape.fromString("CcCcCcCc:CcCcCcCc:CcCcCcCc:CcCcCcCc"))
  }
  test("layers fall through when adding shapes") {
    assert(
      CreateShape.fromString("Cc------") +
        CreateShape.fromString("--Cc----") ==
        CreateShape.fromString("CcCc----"))
    assert(
      CreateShape.fromString("Cc------") +
        CreateShape.fromString("--Cc----") +
        CreateShape.fromString("----Cc--") +
        CreateShape.fromString("------Cc") ==
        CreateShape.fromString("CcCcCcCc"))
    assert(
      CreateShape.fromString("Cc------:--Cc----") +
        CreateShape.fromString("----CgCg:--Cg----") ==
        CreateShape.fromString("Cc------:--CcCgCg:--Cg----"))
    assert(
      CreateShape.fromString("CcCcCcCc:Cc------:--Cc----") +
        CreateShape.fromString("----CgCg:--Cg----") ==
        CreateShape.fromString("CcCcCcCc:Cc------:--CcCgCg:--Cg----"))
    assert(
      CreateShape.fromString("CcCcCcCc:CcCcCcCc:Cc------:--Cc----") +
        CreateShape.fromString("----CgCg:--Cg----") ==
        CreateShape.fromString("CcCcCcCc:CcCcCcCc:Cc------:--CcCgCg"))
    assert(
      CreateShape.fromString("Cc------:--Cc----:Cc------:--Cc----") +
        CreateShape.fromString("------Cg:----Cg--:------Cg:RrRrRrRr") ==
        CreateShape.fromString("Cc------:--Cc--Cg:Cc--Cg--:--Cc--Cg"))
  }
}