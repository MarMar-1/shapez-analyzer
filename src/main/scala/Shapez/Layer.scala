package Shapez

case class Layer(tr: Quadrant = EmptyQuadrant, br: Quadrant = EmptyQuadrant, bl: Quadrant = EmptyQuadrant, tl: Quadrant = EmptyQuadrant) {
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
}