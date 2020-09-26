package Shapez

sealed trait Color {
  override def toString: String
  def +(that: Color): Color

  def listAll: Array[Color] = Array(Red, Green, Blue, Yellow, Cyan, Purple, White, Uncolored)
}

object Uncolored extends Color {
  override def toString: String = "u"

  override def +(that: Color): Color = that
}

object Red extends Color {
  override def toString: String = "r"

  override def +(that: Color): Color = that match {
    case Red => Red
    case Green => Yellow
    case Blue => Purple
    case Yellow => Yellow
    case Cyan => White
    case Purple => Purple
    case _ => that + this
  }
}

object Green extends Color {
  override def toString: String = "g"

  override def +(that: Color): Color = that match {
    case Green => Green
    case Blue => Cyan
    case Yellow => Yellow
    case Cyan => Cyan
    case Purple => White
    case _ => that + this
  }
}

object Blue extends Color {
  override def toString: String = "b"

  override def +(that: Color): Color = that match {
    case Blue => Blue
    case Yellow => White
    case Cyan => Cyan
    case Purple => Purple
    case _ => that + this
  }
}

object Yellow extends Color {
  override def toString: String = "y"

  override def +(that: Color): Color = that match {
    case Yellow => Yellow
    case Cyan => White
    case Purple => White
    case _ => that + this
  }
}

object Cyan extends Color {
  override def toString: String = "c"

  override def +(that: Color): Color = that match {
    case Cyan => Cyan
    case Purple => White
    case _ => that + this
  }
}

object Purple extends Color {
  override def toString: String = "p"

  override def +(that: Color): Color = that match {
    case Purple => Purple
    case _ => that + this
  }
}

object White extends Color {
  override def toString: String = "w"

  override def +(that: Color): Color = this
}

object NoneColor extends Color {
  override def toString: String = "-"

  override def +(that: Color): Color = NoneColor
}