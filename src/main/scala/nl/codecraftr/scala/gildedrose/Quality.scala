package nl.codecraftr.scala.gildedrose

import nl.codecraftr.scala.gildedrose.Quality.{
  MAXIMUM,
  MAX_QUALITY,
  MINIMUM,
  MIN_QUALITY
}

object Quality {
  private val MIN_QUALITY = 0
  private val MAX_QUALITY = 50
  val MAXIMUM: Quality = Quality(MAX_QUALITY)
  val MINIMUM: Quality = Quality(MIN_QUALITY)
}

// TODO als constrain creation within the bounds
case class Quality(value: Int) extends AnyVal {
  def +(amount: Int): Quality = {
    val updatedQuality = value + amount

    if (updatedQuality > MAX_QUALITY) MAXIMUM
    else copy(value = updatedQuality)
  }

  def -(amount: Int): Quality = {
    val updatedQuality = value - amount

    if (updatedQuality < MIN_QUALITY) MINIMUM
    else copy(value = updatedQuality)
  }
}
