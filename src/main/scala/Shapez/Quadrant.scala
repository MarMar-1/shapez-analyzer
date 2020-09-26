package Shapez

sealed trait Quadrant {
  override def toString: String = color.toString

  def color: Color

  def +(newColor: Color): Quadrant

  def empty: Boolean = false

  def listAllUncolored: Array[Quadrant] = Array(Circle(), Rectangle(), Windmill(), Star())

  def listAll: Array[Quadrant] = listAllUncolored.flatMap(quad => NoneColor.listAll.map(color => quad + color))
}

case class Circle(color: Color = Uncolored) extends Quadrant {
  override def toString: String = "C" + super.toString

  override def +(newColor: Color): Quadrant = Circle(newColor)
}

case class Rectangle(color: Color = Uncolored) extends Quadrant {
  override def toString: String = "R" + super.toString

  override def +(newColor: Color): Quadrant = Rectangle(newColor)
}

case class Windmill(color: Color = Uncolored) extends Quadrant {
  override def toString: String = "W" + super.toString

  override def +(newColor: Color): Quadrant = Windmill(newColor)
}

case class Star(color: Color = Uncolored) extends Quadrant {
  override def toString: String = "S" + super.toString

  override def +(newColor: Color): Quadrant = Star(newColor)
}

object EmptyQuadrant extends Quadrant {
  override def toString: String = "-" + super.toString

  override def color: Color = NoneColor

  override def +(newColor: Color): Quadrant = EmptyQuadrant

  override def empty: Boolean = true
}

