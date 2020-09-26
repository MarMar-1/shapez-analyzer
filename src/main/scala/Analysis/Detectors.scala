package Analysis

import Shapez._

import scala.language.implicitConversions

object Detectors {
  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  def isPossible(in: Shape): Boolean = {
    (in.numLayers == in.highestLayer) && !in.empty &&
      (isFreeplay(in) || isSimpleFloating(in) || isMultilayerScaffolding(in) || isDropThrough(in))
  }

  def isFreeplay(in: Shape): Boolean = {
    if (in.empty) false
    else {
      (in.layers.slice(0, 3) zip in.layers.slice(1, 4))
        .filter { case (lower, upper) => !lower.empty && !upper.empty }
        .forall { case (lower, upper) => lower stacks upper }
    }
  }

  private def isSimpleFloatingHelper(in: Shape): Boolean = {
    assert(in.l3.empty && in.l4.empty, "provided shape has more than 2 layers")
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
          isSimpleFloatingHelper(l23) && (in.l4.empty || isFreeplay(l34))
        }
      } else {
        isSimpleFloatingHelper(l12) &&
          (in.l3.empty ||
            (isFreeplay(l23) &&
              ((in.l4.empty || isFreeplay(l34)) || isSimpleFloatingHelper(l34))))
      }
    }
  }

  private def scaffoldingTest(in1: Shape, in2: Shape): Boolean = in1.empty && !in2.empty && in2.highestLayer == in2.numLayers

  private def isMultilayerScaffoldingHelper(in: Shape): Boolean = {
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
        val shapeToTest = Shape(in.l2, in.l3, in.l4)
        isMultilayerScaffoldingHelper(shapeToTest) ||
          scaffoldingTest(Shape(), shapeToTest.left) && scaffoldingTest(Shape(), shapeToTest.right) ||
          scaffoldingTest(Shape(), shapeToTest.rotatedCW.left) && scaffoldingTest(Shape(), shapeToTest.rotatedCW.right)
      } else {
        isMultilayerScaffoldingHelper(in) ||
          scaffoldingTest(Shape(), in.left) && scaffoldingTest(Shape(), in.right) ||
          scaffoldingTest(Shape(), in.rotatedCW.left) && scaffoldingTest(Shape(), in.rotatedCW.right)
      }
    }
  }

  def is2lImpossible(in: Shape): Boolean = {
    assert(in.l3.empty && in.l4.empty, "provided shape does not have exactly 2 layers")
    in.l2.count == in.l1.count && in.l1.count == 1 && !isFreeplay(in) &&
      ((in.l1.quadrants ++ in.l1.quadrants) zip (in.l2.rotatedCW.quadrants ++ in.l2.rotatedCCW.quadrants))
        .forall { case (lower, upper) => lower.empty || upper.empty }
  }

  private def isDropThroughHelper(in: Shape, validHalf: Shape => Boolean): Boolean = {

    validHalf(in.left) && validHalf(in.right) || validHalf(in.rotatedCW.left) && validHalf(in.rotatedCW.right)
  }

  private def isDropThroughOverallHelper(in: Shape): Boolean = {
    def validHalfPartialDepth(shape: Shape): Boolean = {
      shape.numLayers == (5 - shape.lowestLayer) && !shape.l4.empty
    }

    val topSetValid =
      if (in.l2.empty) false
      else if (in.l1.empty) isDropThroughHelper(Shape(l3 = in.l3, l4 = in.l4), validHalfPartialDepth)
      else isDropThroughHelper(Shape(l2 = in.l2, l3 = in.l3, l4 = in.l4), validHalfPartialDepth)

    def validHalfFullDepth(shape: Shape): Boolean = {
      topSetValid && !shape.layerAt(in.lowestLayer).empty
    }

    isDropThroughHelper(in, validHalfFullDepth) || isDropThroughHelper(in, validHalfPartialDepth)
  }

  def dropThroughShapes(in: Shape): Array[(Shape, Shape)] = {
    val bottom = in.l1
    val middle = in.l2
    val top = in.l3
    val potTopShapes = if (top.count == 1) {
      val newBotQuads = (middle.quadrants zip top.quadrants).map { case (bottomQ, topQ) => if (topQ.empty) bottomQ else EmptyQuadrant }
      Array(Shape(Layer(newBotQuads(0), newBotQuads(1), newBotQuads(2), newBotQuads(3)), top))
    } else {
      val potQuads: Array[(Quadrant, Quadrant)] = (bottom.quadrants zip (middle.rotatedCW.quadrants zip middle.rotatedCCW.quadrants))
        .map(in => {
          val (botQ, (nextMidQ, prevMidQ)) = in
          if (!botQ.empty) {
            if (!nextMidQ.empty && !prevMidQ.empty) (EmptyQuadrant, EmptyQuadrant)
            else if (!prevMidQ.empty) (nextMidQ, EmptyQuadrant)
            else if (!nextMidQ.empty) (EmptyQuadrant, prevMidQ)
            else (nextMidQ, prevMidQ)
          } else {
            (nextMidQ, prevMidQ)
          }
        })
      val (potNewBotCW: Array[Quadrant], potNewBotCCW: Array[Quadrant]) = potQuads.unzip[Quadrant, Quadrant]
      val potNewBot1 = Layer(potNewBotCW(0), potNewBotCW(1), potNewBotCW(2), potNewBotCW(3)).rotatedCCW
      val potNewBot2 = Layer(potNewBotCCW(0), potNewBotCCW(1), potNewBotCCW(2), potNewBotCCW(3)).rotatedCW
      val potShape1 = Shape(potNewBot1, top)
      val potShape2 = Shape(potNewBot2, top)
      Array(potShape1, potShape2)
    }
    val potBotShapes = potTopShapes.map(topShape => CreateShape.fromQuadrants(
      Array(
        bottom.quadrants,
        middle.quadrants.zip(topShape.l1.quadrants)
          .map { case (original, newTop) => if (newTop.empty) original else EmptyQuadrant })))
    potBotShapes.zip(potTopShapes).filter { case (botShape, topShape) => validate3lDropThrough(botShape, topShape, in) }
  }

  private def validate3lDropThrough(botShape: Shape, topShape: Shape, goal: Shape): Boolean = {
    !is2lImpossible(botShape) && !is2lImpossible(topShape) && (botShape + topShape == goal)
  }

  def is3lDropThroughPossible(in: Shape): Boolean = {
    val validShapeCombos = dropThroughShapes(in)
    if (validShapeCombos.isEmpty) false
    else true

  }

  def isDropThrough(in: Shape): Boolean = {
    if (isFreeplay(in) || isSimpleFloating(in) || isMultilayerScaffolding(in)) false
    else {
      val l12 = Shape(in.l1, in.l2)
      val l23 = Shape(in.l2, in.l3)
      if (isFreeplay(l12)) {
        isDropThrough(Shape(in.l2, in.l3, in.l4, Layer(Circle(), Circle(), Circle(), Circle()))) ||
        isDropThrough(Shape(in.l2, in.l3, in.l4))
      } else if (isSimpleFloating(l12) && isFreeplay(l23)) {
        isDropThrough(Shape(in.l3, in.l4, Layer(Circle(), Circle(), Circle(), Circle())))
      } else {
        if (is2lImpossible(l12)) false
        else {
          if (in.l3.empty) false
          else if (is3lDropThroughPossible(Shape(in.l1, in.l2, in.l3))) {
            if (in.l4.empty) true
            else !is2lImpossible(Shape(in.l3, in.l4))
          } else {
            if (in.l4.empty) false
            else if ((in.l3 stacks in.l4) && !is2lImpossible(Shape(in.l2, in.l4)) &&
              (isFreeplay(Shape(in.l2, in.l3, in.l4)) || is3lDropThroughPossible(Shape(in.l2, in.l3, in.l4)))) {
              if (isDropThroughOverallHelper(Shape(l2 = in.l1, l3 = in.l2, l4 = in.l3))) true
              else isDropThroughOverallHelper(in)
            } else {
              isDropThroughOverallHelper(in)
            }
          }
        }
      }
    }
  }
}
