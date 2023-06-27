package nl.codecraftr.scala.gildedrose

import nl.codecraftr.scala.gildedrose.StoreItem.{
  AGED_BRIE,
  BACKSTAGE_PASS,
  SULFURAS
}

object StoreItem {
  val SULFURAS = "Sulfuras, Hand of Ragnaros"
  val AGED_BRIE = "Aged Brie"
  val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  private val CONJURED = "Conjured"

  def from(item: Item): StoreItem = {
    val quality = Quality(item.quality)
    val sellIn = SellIn(item.sellIn)
    item.name match {
      case SULFURAS                    => Sulfuras(sellIn)
      case BACKSTAGE_PASS              => BackstagePass(sellIn, quality)
      case AGED_BRIE                   => AgedBrie(sellIn, quality)
      case n if n.startsWith(CONJURED) => ConjuredItem(n, sellIn, quality)
      case _ => RegularItem(item.name, sellIn, quality)
    }
  }
}

sealed trait StoreItem {
  def name: String
  def sellIn: SellIn
  def quality: Quality

  protected def updatedQuality: StoreItem
  protected def updatedSellIn: StoreItem

  def updated: StoreItem = updatedSellIn.updatedQuality
}

case class Sulfuras(sellIn: SellIn) extends StoreItem {
  override def name: String = SULFURAS
  override def quality: Quality = LegendaryQuality

  override def updatedQuality: StoreItem = this
  override def updatedSellIn: StoreItem = this
}

case class AgedBrie(sellIn: SellIn, quality: Quality) extends StoreItem {
  override def name: String = AGED_BRIE

  override def updatedQuality: StoreItem = {
    val newQuality =
      sellIn match {
        case x if x < 0 => quality + 2
        case _          => quality + 1
      }

    copy(quality = newQuality)
  }

  override def updatedSellIn: StoreItem = copy(sellIn = sellIn.decreased)
}

case class BackstagePass(sellIn: SellIn, quality: Quality) extends StoreItem {
  override def name: String = BACKSTAGE_PASS

  override def updatedQuality: StoreItem = {
    val newQuality =
      sellIn match {
        case x if x < 0   => MinimumQuality
        case x if x <= 5  => quality + 3
        case x if x <= 10 => quality + 2
        case _            => quality + 1
      }

    copy(quality = newQuality)
  }

  override def updatedSellIn: StoreItem = copy(sellIn = sellIn.decreased)
}

case class ConjuredItem(name: String, sellIn: SellIn, quality: Quality)
    extends StoreItem {
  override def updatedQuality: StoreItem = {
    val updatedQuality = sellIn match {
      case x if x < 0 => quality - 4
      case _          => quality - 2
    }

    copy(quality = updatedQuality)
  }

  override def updatedSellIn: StoreItem = copy(sellIn = sellIn.decreased)
}

// D -> immutable
case class RegularItem(name: String, sellIn: SellIn, quality: Quality)
    extends StoreItem {
  def updatedQuality: StoreItem = {
    val updatedQuality = sellIn match {
      case x if x < 0 => quality - 2
      case _          => quality - 1
    }

    copy(quality = updatedQuality)
  }

  def updatedSellIn: StoreItem = copy(sellIn = sellIn.decreased)
}
