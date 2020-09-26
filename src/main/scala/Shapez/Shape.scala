package Shapez

import scala.language.implicitConversions


case class Shape(l1: Layer = Layer(), l2: Layer = Layer(), l3: Layer = Layer(), l4: Layer = Layer()) {
  def layerAt(layerNum: Int): Layer = {
    assert(layerNum > 0 && layerNum < 5, "Invalid layer number")
    layers(layerNum - 1)
  }

  implicit def fromString(in: String): Shape = CreateShape.fromString(in)

  def layers = Array(l1, l2, l3, l4)

  override def toString: String = layers.filter(!_.empty).map(_.toString).mkString(":")

  def empty: Boolean = layers.forall(_.empty)

  def numLayers: Int = layers.count(!_.empty)

  def highestLayer: Int = layers.zipWithIndex.map { case (layer, index) => if (layer.empty) -1 else index }.max + 1

  def lowestLayer: Int = layers.zipWithIndex.map { case (layer, index) => if (layer.empty) 3 else index }.min + 1

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

  private def withoutEmptyLayers: Shape = if (this.empty) this else CreateShape.fromString(this.toString)

  def cut: (Shape, Shape) = {
    (this.left.withoutEmptyLayers, this.right.withoutEmptyLayers)
  }

  def right: Shape = {
    Shape(l1.right, l2.right, l3.right, l4.right)
  }

  def left: Shape = {
    Shape(l1.left, l2.left, l3.left, l4.left)
  }

  def listAllStructures: Array[Shape] = {
    l1.listAllStructures.map(Shape(_)) ++
      l1.listAllStructures.flatMap(struct1 => l1.listAllStructures.map(Shape(struct1, _))) ++
      l1.listAllStructures.flatMap(struct1 => l1.listAllStructures.flatMap(struct2 => l1.listAllStructures.map(Shape(struct1, struct2, _)))) ++
      l1.listAllStructures.flatMap(struct1 => l1.listAllStructures.flatMap(struct2 => l1.listAllStructures.flatMap(struct3 => l1.listAllStructures.map(Shape(struct1, struct2, struct3, _)))))
  }

  def bruteForceProducableShapes: Set[Shape] = {
    val depth1 = l1.listAllStructures.map(Shape(_)).toSet
    val depth2_stacked = depth1.flatMap(shape => depth1.map(shape + _))
    val depth2_stacked_cut = depth2_stacked.map(shape => shape.cut).flatMap { case (left, right) => Array(left, right) }
    val depth2_stacked_cut_rotated = depth2_stacked_cut.map(shape => shape.rotatedCW)
    val assorted_stacking =
      depth1.flatMap(shape => depth2_stacked.map(shape + _)) ++
        depth1.flatMap(shape => depth2_stacked_cut.map(shape + _)) ++
        depth1.flatMap(shape => depth2_stacked_cut_rotated.map(shape + _)) ++
        depth2_stacked.flatMap(shape => depth2_stacked.map(shape + _)) ++
        depth2_stacked.flatMap(shape => depth2_stacked_cut.map(shape + _)) ++
        depth2_stacked.flatMap(shape => depth2_stacked_cut_rotated.map(shape + _)) ++
        depth2_stacked_cut.flatMap(shape => depth2_stacked.map(shape + _)) ++
        depth2_stacked_cut.flatMap(shape => depth2_stacked_cut.map(shape + _)) ++
        depth2_stacked_cut.flatMap(shape => depth2_stacked_cut_rotated.map(shape + _)) ++
        depth2_stacked_cut_rotated.flatMap(shape => depth2_stacked.map(shape + _)) ++
        depth2_stacked_cut_rotated.flatMap(shape => depth2_stacked_cut.map(shape + _)) ++
        depth2_stacked_cut_rotated.flatMap(shape => depth2_stacked_cut_rotated.map(shape + _))
    val aggregate = depth1 ++ depth2_stacked ++ depth2_stacked_cut ++ depth2_stacked_cut_rotated ++ assorted_stacking
    val agg2 = aggregate.flatMap(shape => aggregate.map(shape + _)) ++ aggregate
    val agg2_cut = agg2.map(_.cut).flatMap({ case (left, right) => Array(left, right) })
    val agg2_cut_rotated = agg2_cut.map(_.rotatedCW)
    val agg3 = agg2 ++ agg2_cut ++ agg2_cut_rotated
    agg3.flatMap(shape => agg3.map(shape + _)) ++ agg3
  }
}

object CreateShape {
  def fromLayers(in: Array[Layer]): Shape = {
    val withFiller = in ++ Array(Layer(), Layer(), Layer(), Layer())
    Shape(withFiller(0), withFiller(1), withFiller(2), withFiller(3))
  }

  def fromQuadrants(in: Array[Array[Quadrant]]): Shape = {
    fromLayers(in.map(quadsArr => {
      val withFiller = quadsArr ++ Array(EmptyQuadrant, EmptyQuadrant, EmptyQuadrant, EmptyQuadrant)
      Layer(withFiller(0), withFiller(1), withFiller(2), withFiller(3))
    }))
  }
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