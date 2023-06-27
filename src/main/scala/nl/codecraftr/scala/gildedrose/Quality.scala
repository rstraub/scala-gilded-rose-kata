package nl.codecraftr.scala.gildedrose

object Quality {
  def apply(value: Int): Quality =
    value match {
      case x if x > MaximumQuality.value => MaximumQuality
      case x if x < MinimumQuality.value => MinimumQuality
      case _                             => RegularQuality(value)
    }
}

sealed trait Quality {
  def value: Int

  def -(amount: Int): Quality
  def +(amount: Int): Quality
  def >(other: Quality): Boolean = value > other.value
  def <(other: Quality): Boolean = value < other.value
}

object MaximumQuality extends Quality {
  override def value: Int = 50
  override def -(amount: Int): Quality = RegularQuality(value - amount)
  override def +(amount: Int): Quality = this
}

object MinimumQuality extends Quality {
  override def value: Int = 0
  override def -(amount: Int): Quality = this
  override def +(amount: Int): Quality = RegularQuality(amount)
}

object LegendaryQuality extends Quality {
  override def value: Int = 80
  override def -(amount: Int): Quality = this
  override def +(amount: Int): Quality = this
}

case class RegularQuality private (value: Int) extends Quality {
  def +(amount: Int): Quality = Quality(value + amount)
  def -(amount: Int): Quality = Quality(value - amount)

  // Here to prevent copy from bypassing lower and upper bounds for regular quality
  def copy(value: Int = value): Quality = Quality(value)
}
