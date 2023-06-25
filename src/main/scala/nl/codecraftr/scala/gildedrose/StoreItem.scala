package nl.codecraftr.scala.gildedrose

import nl.codecraftr.scala.gildedrose.StoreItem.{
  AGED_BRIE,
  BACKSTAGE_PASS,
  MAX_QUALITY,
  MIN_QUALITY,
  SULFURAS
}

sealed trait StoreItem {
  def name: String
  def sellIn: Int
  def quality: Int

  def updatedQuality: StoreItem
  def updatedSellBy: StoreItem
}

case class Sulfuras(sellIn: Int) extends StoreItem {
  override def name: String = SULFURAS
  override def quality: Int = 80

  override def updatedQuality: StoreItem = this
  override def updatedSellBy: StoreItem = this
}

case class AgedBrie(sellIn: Int, quality: Int) extends StoreItem {
  override def name: String = AGED_BRIE

  override def updatedQuality: StoreItem = {
    val newQuality =
      sellIn match {
        case x if x < 0 => quality + 2
        case _          => quality + 1
      }

    if (newQuality > MAX_QUALITY) copy(quality = MAX_QUALITY)
    else copy(quality = newQuality)
  }

  override def updatedSellBy: StoreItem = copy(sellIn = sellIn - 1)
}

// D -> immutable
// Only here until the types are correct
case class TempItem(name: String, sellIn: Int, quality: Int) extends StoreItem {

  private def calculateNewQuality: Int = {
    name match {
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
  val MIN_QUALITY = 0
  val MAX_QUALITY = 50
  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val AGED_BRIE = "Aged Brie"
  val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  def from(item: Item): StoreItem = {
    item.name match {
      case SULFURAS => Sulfuras(item.sellIn)
      case AGED_BRIE => AgedBrie(item.sellIn, item.quality)
      case _        => TempItem(item.name, item.sellIn, item.quality)
    }
  }
}
