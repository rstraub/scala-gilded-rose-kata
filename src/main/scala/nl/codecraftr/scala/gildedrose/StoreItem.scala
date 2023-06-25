package nl.codecraftr.scala.gildedrose

import nl.codecraftr.scala.gildedrose.StoreItem.{
  AGED_BRIE,
  BACKSTAGE_PASS,
  MAX_QUALITY,
  MIN_QUALITY,
  SULFURAS
}

object StoreItem {
  val MIN_QUALITY = 0
  val MAX_QUALITY = 50
  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val AGED_BRIE = "Aged Brie"
  val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"

  def from(item: Item): StoreItem = {
    val quality = Quality(item.quality)
    item.name match {
      case SULFURAS       => Sulfuras(item.sellIn)
      case BACKSTAGE_PASS => BackstagePass(item.sellIn, quality)
      case AGED_BRIE      => AgedBrie(item.sellIn, quality)
      case _              => RegularItem(item.name, item.sellIn, quality)
    }
  }
}

sealed trait StoreItem {
  def name: String
  def sellIn: Int
  def quality: Quality

  def updatedQuality: StoreItem
  def updatedSellBy: StoreItem
}

case class Sulfuras(sellIn: Int) extends StoreItem {
  override def name: String = SULFURAS
  override def quality: Quality = Quality(80)

  override def updatedQuality: StoreItem = this
  override def updatedSellBy: StoreItem = this
}

case class AgedBrie(sellIn: Int, quality: Quality) extends StoreItem {
  override def name: String = AGED_BRIE

  override def updatedQuality: StoreItem = {
    val newQuality =
      sellIn match {
        case x if x < 0 => quality + 2
        case _          => quality + 1
      }

    copy(quality = newQuality)
  }

  override def updatedSellBy: StoreItem = copy(sellIn = sellIn - 1)
}

case class BackstagePass(sellIn: Int, quality: Quality) extends StoreItem {
  override def name: String = BACKSTAGE_PASS

  override def updatedQuality: StoreItem = {
    val newQuality =
      sellIn match {
        case x if x < 0   => Quality(0)
        case x if x <= 5  => quality + 3
        case x if x <= 10 => quality + 2
        case _            => quality + 1
      }

    copy(quality = newQuality)
  }

  override def updatedSellBy: StoreItem = copy(sellIn = sellIn - 1)
}

// D -> immutable
case class RegularItem(name: String, sellIn: Int, quality: Quality)
    extends StoreItem {
  def updatedQuality: StoreItem = {
    val updatedQuality = sellIn match {
      case x if x < 0 => quality - 2
      case _          => quality - 1
    }

    copy(quality = updatedQuality)
  }

  def updatedSellBy: StoreItem = copy(sellIn = sellIn - 1)
}
