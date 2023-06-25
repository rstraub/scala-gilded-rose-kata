package nl.codecraftr.scala.gildedrose

// D -> immutable
case class StoreItem(name: String, sellIn: Int, quality: Int) {
  private val MIN_QUALITY = 0
  private val MAX_QUALITY = 50
  private val SULFURAS = "Sulfuras, Hand of Ragnaros"
  private val AGED_BRIE = "Aged Brie"
  private val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"

  private def calculateNewQuality: Int = {
    name match {
      case SULFURAS => quality
      case AGED_BRIE =>
        sellIn match {
          case x if x < 0 => quality + 2
          case _          => quality + 1
        }
      case BACKSTAGE_PASS =>
        sellIn match {
          case x if x < 0   => 0
          case x if x <= 5  => quality + 3
          case x if x <= 10 => quality + 2
          case _            => quality + 1
        }
      case _ =>
        sellIn match {
          case x if x < 0 => quality - 2
          case _          => quality - 1
        }
    }
  }

  def updatedQuality: StoreItem = {
    if (name == SULFURAS) this
    else {
      val updatedQuality = calculateNewQuality

      if (updatedQuality > MAX_QUALITY) copy(quality = MAX_QUALITY)
      else if (updatedQuality < MIN_QUALITY) copy(quality = MIN_QUALITY)
      else copy(quality = updatedQuality)
    }
  }

  def updatedSellBy: StoreItem = {
    if (name == SULFURAS) this
    else copy(sellIn = sellIn - 1)
  }
}

object StoreItem {
  def from(item: Item): StoreItem =
    StoreItem(item.name, item.sellIn, item.quality)
}
