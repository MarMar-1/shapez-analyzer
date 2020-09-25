package Analysis

import Shapez._

import scala.language.implicitConversions

object Detectors {
  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  def isFreeplay(in: Shape): Boolean = {
    (in.layers.slice(0, 3) zip in.layers.slice(1, 4))
      .filter { case (lower, upper) => !lower.empty && !upper.empty }
      .forall { case (lower, upper) => lower stacks upper }
  }

  private def isSimpleFloatingHelper(in: Shape): Boolean = {
    assert(in.l3.empty && in.l4.empty, "provided shape does not have exactly 2 layers")
    assert(!isFreeplay(in), "provided shape does not have a floating layer")
    in.l2.count <= in.l1.count &&
      ((in.l1.quadrants ++ in.l1.quadrants) zip (in.l2.rotatedCW.quadrants ++ in.l2.rotatedCCW.quadrants))
        .exists { case (lower, upper) => !lower.empty && !upper.empty }
  }

  def isSimpleFloating(in: Shape): Boolean = {
    if (isFreeplay(in)) false
    else {
      val l12 = Shape(in.l1, in.l2)
      val l23 = Shape(in.l2, in.l3)
      val l34 = Shape(in.l3, in.l4)
      if (isFreeplay(l12)) {
        if (isFreeplay(l23)) {
          isSimpleFloatingHelper(l34)
        } else {
          isSimpleFloatingHelper(l23) && isFreeplay(l34)
        }
      } else {
        isSimpleFloatingHelper(l12) && isFreeplay(l23) && (isFreeplay(l34) || isSimpleFloatingHelper(l34))
      }
    }
  }

  def isDropThrough(in: Shape): Boolean = {
    if (isFreeplay(in) || isSimpleFloating(in)) false
    else {
      true
    }
  }
}
