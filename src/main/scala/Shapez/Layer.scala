package Shapez

case class Layer(tr: Quadrant = EmptyQuadrant, br: Quadrant = EmptyQuadrant, bl: Quadrant = EmptyQuadrant, tl: Quadrant = EmptyQuadrant) {
  def left: Layer = Layer(bl = bl, tl = tl)

  def right: Layer = Layer(br = br, tr = tr)

  def rotatedCW: Layer = Layer(tl, tr, br, bl)

  def rotatedCCW: Layer = this.rotatedCW.rotatedCW.rotatedCW

  def quadrants = Array(tr, br, bl, tl)

  def count: Int = quadrants.count(!_.empty)

  override def toString: String = quadrants.map(_.toString).mkString

  def empty: Boolean = quadrants.forall(_.empty)

  def stacks(that: Layer): Boolean = {
    (this.quadrants zip that.quadrants).exists { case (lower, higher) => !lower.empty && !higher.empty }
  }

  def +(that: Layer): Layer = {
    Layer(
      if (this.tr.empty) that.tr else this.tr,
      if (this.br.empty) that.br else this.br,
      if (this.bl.empty) that.bl else this.bl,
      if (this.tl.empty) that.tl else this.tl
    )
  }

  def listAllStructures: Array[Layer] =
    Array(
      Layer(Circle()), Layer(br = Circle()), Layer(bl = Circle()), Layer(tl = Circle()),
      Layer(Circle(), Circle()), Layer(br = Circle(), bl = Circle()), Layer(bl = Circle(), tl = Circle()), Layer(tl = Circle(), tr = Circle()),
      Layer(tr = Circle(), bl = Circle()), Layer(br = Circle(), tl = Circle()),
      Layer(Circle(), Circle(), Circle()), Layer(br = Circle(), bl = Circle(), tl = Circle()),
      Layer(bl = Circle(), tl = Circle(), tr = Circle()), Layer(tl = Circle(), tr = Circle(), br = Circle()),
      Layer(Circle(), Circle(), Circle(), Circle()),
    )
}