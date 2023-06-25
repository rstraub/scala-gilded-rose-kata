package nl.codecraftr.scala.gildedrose

import nl.codecraftr.scala.gildedrose.Quality.{MAX_QUALITY, MIN_QUALITY}

object Quality {
  private val MIN_QUALITY = 0
  private val MAX_QUALITY = 50
}

case class Quality(value: Int) extends AnyVal {
  def +(amount: Int): Quality = {
    val updatedQuality = value + amount

    if (updatedQuality > MAX_QUALITY) copy(value = MAX_QUALITY)
    else copy(value = updatedQuality)
  }

  def -(amount: Int): Quality = {
    val updatedQuality = value - amount

    if (updatedQuality < MIN_QUALITY) copy(value = MIN_QUALITY)
    else copy(value = updatedQuality)
  }
}
