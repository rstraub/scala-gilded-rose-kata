package nl.codecraftr.scala.gildedrose

object Quality {
  def apply(value: Int): Quality = {
    if (value > MaximumQuality.value) MaximumQuality
    else if (value < MinimumQuality.value) MinimumQuality
    else RegularQuality(value)
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

case class RegularQuality private(value: Int) extends Quality {
  def +(amount: Int): Quality = Quality(value + amount)
  def -(amount: Int): Quality = Quality(value - amount)
}
