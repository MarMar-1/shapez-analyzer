package Analysis

import Shapez._

import scala.language.implicitConversions

object Detectors {
  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  def isPossible(in: Shape): Boolean = {
    isFreeplay(in) || isSimpleFloating(in) || isMultilayerScaffolding(in) || isDropThrough(in)
  }

  def isFreeplay(in: Shape): Boolean = {
    (in.layers.slice(0, 3) zip in.layers.slice(1, 4))
      .filter { case (lower, upper) => !lower.empty && !upper.empty }
      .forall { case (lower, upper) => lower stacks upper }
  }

  private def isSimpleFloatingHelper(in: Shape): Boolean = {
    assert(in.l3.empty && in.l4.empty && !in.l1.empty && !in.l2.empty, "provided shape does not have exactly 2 layers")
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

  def isMultilayerScaffoldingHelper(in: Shape): Boolean = {
    val scaffoldingTest = (in1: Shape, in2: Shape) => in1.empty && !in2.empty && in2.highestLayer == in2.numLayers
    scaffoldingTest(in.left, in.right) ||
      scaffoldingTest(in.right, in.left) ||
      scaffoldingTest(in.rotatedCCW.left, in.rotatedCCW.right) ||
      scaffoldingTest(in.rotatedCCW.right, in.rotatedCCW.left)
  }

  def isMultilayerScaffolding(in: Shape): Boolean = {
    if (isFreeplay(in) || isSimpleFloating(in)) false
    else {
      val l12 = Shape(in.l1, in.l2)
      if (isFreeplay(l12)) {
        isMultilayerScaffoldingHelper(Shape(in.l2, in.l3, in.l4))
      } else {
        isMultilayerScaffoldingHelper(in)
      }
    }
  }

  def is2lImpossible(in: Shape): Boolean = {
    assert(in.l3.empty && in.l4.empty && !in.l1.empty && !in.l2.empty, "provided shape does not have exactly 2 layers")
    in.l2.count == in.l1.count && in.l1.count == 1 &&
      ((in.l1.quadrants ++ in.l1.quadrants) zip (in.l2.rotatedCW.quadrants ++ in.l2.rotatedCCW.quadrants))
        .forall { case (lower, upper) => lower.empty || upper.empty }
  }

  def isDropThroughHelper(in: Shape): Boolean = {
    val validHalf = (shape: Shape) => isFreeplay(shape) || isSimpleFloating(shape) || isMultilayerScaffolding(shape)
    validHalf(in.left) && validHalf(in.right) || validHalf(in.rotatedCW.left) && validHalf(in.rotatedCW.right)
  }

  def isDropThrough(in: Shape): Boolean = {
    if (isFreeplay(in) || isSimpleFloating(in) || isMultilayerScaffolding(in)) false
    else {
      val l12 = Shape(in.l1, in.l2)
      if (isFreeplay(l12) || isSimpleFloating(l12)) {
        isDropThrough(Shape(in.l2, in.l3, in.l4))
      } else {
        if (is2lImpossible(l12)) false
        else {
          if (in.l3.empty) false
          else if (in.l2 stacks in.l3) {
            isPossible(Shape(in.l3, in.l4))
          } else {
            if (in.l4.empty) false
            else if (in.l3 stacks in.l4) {
              true
            } else {
              isDropThroughHelper(Shape(in.l2, in.l3, in.l4))
            }
          }
        }
      }
    }
  }
}
