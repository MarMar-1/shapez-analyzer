package Shapez

import scala.language.implicitConversions


case class Shape(l1: Layer = Layer(), l2: Layer = Layer(), l3: Layer = Layer(), l4: Layer = Layer()) {
  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  def layers = Array(l1, l2, l3, l4)

  override def toString: String = layers.filter(!_.empty).map(_.toString).mkString(":")

  def empty: Boolean = layers.forall(_.empty)

  def numLayers: Int = layers.count(!_.empty)

  def +(that: Shape): Shape = {
    def fallthroughDepth = {
      if (this.l4 stacks that.l1) {
        0
      } else if ((this.l3 stacks that.l1) || (this.l4 stacks that.l2)) {
        1
      } else if ((this.l2 stacks that.l1) || (this.l3 stacks that.l2) || (this.l4 stacks that.l3)) {
        2
      } else if ((this.l1 stacks that.l1) || (this.l2 stacks that.l2) || (this.l3 stacks that.l3) || (this.l4 stacks that.l4)) {
        3
      } else {
        4
      }
    }

    fallthroughDepth match {
      case 0 => this
      case 1 => Shape(this.l1, this.l2, this.l3, this.l4 + that.l1)
      case 2 => Shape(this.l1, this.l2, this.l3 + that.l1, this.l4 + that.l2)
      case 3 => Shape(this.l1, this.l2 + that.l1, this.l3 + that.l2, this.l4 + that.l3)
      case 4 => Shape(this.l1 + that.l1, this.l2 + that.l2, this.l3 + that.l3, this.l4 + that.l4)
    }
  }

  def rotatedCW: Shape = {
    Shape(l1.rotatedCW, l2.rotatedCW, l3.rotatedCW, l4.rotatedCW)
  }

  def rotatedCCW: Shape = {
    Shape(l1.rotatedCCW, l2.rotatedCCW, l3.rotatedCCW, l4.rotatedCCW)
  }
}

object CreateShape {
  def fromString(in: String): Shape = {
    val color = "[rgbycpwu-]"
    val shape = "[CRWS-]"
    val quadrant = "(?:--|[CRWS][rgbycpwu])"
    val quadrantRegex = s"($shape)($color)".r
    val layer = quadrant * 4
    val layerRegex = s"($quadrant)($quadrant)($quadrant)($quadrant)".r
    val overallRegex = s"($layer)(?::($layer))?(?::($layer))?(?::($layer))?".r

    def layerStringtoLayer(layerString: String): Layer = {
      if (layerString == null) {
        Layer()
      } else {
        val layerRegex(tr, br, bl, tl) = layerString

        def quadrantStringtoQuadrant(quadrantString: String): Quadrant = {
          def colorChartoColor(colorChar: String): Color = {
            colorChar match {
              case "r" => Red
              case "g" => Green
              case "b" => Blue
              case "y" => Yellow
              case "c" => Cyan
              case "p" => Purple
              case "w" => White
              case "u" => Uncolored
              case "-" => NoneColor
            }
          }

          val quadrantRegex(shapeChar, colorChar) = quadrantString
          val color = colorChartoColor(colorChar)
          shapeChar match {
            case "C" => Circle(color)
            case "R" => Rectangle(color)
            case "W" => Windmill(color)
            case "S" => Star(color)
            case "-" => EmptyQuadrant
          }
        }

        Layer(quadrantStringtoQuadrant(tr), quadrantStringtoQuadrant(br), quadrantStringtoQuadrant(bl), quadrantStringtoQuadrant(tl))
      }
    }

    val overallRegex(l1, l2, l3, l4) = in
    val layers = Array(layerStringtoLayer(l1), layerStringtoLayer(l2), layerStringtoLayer(l3), layerStringtoLayer(l4))
    layers.filter(!_.empty)
    layers.length match {
      case 0 => Shape()
      case 1 => Shape(layers(0))
      case 2 => Shape(layers(0), layers(1))
      case 3 => Shape(layers(0), layers(1), layers(2))
      case 4 => Shape(layers(0), layers(1), layers(2), layers(3))
    }
  }
}