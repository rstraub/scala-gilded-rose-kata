package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  private val AGED_BRIE = "Aged Brie"
  private val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"

  // A -> mutates state of the store
  def updateQuality(): Unit = {
    val updatedItems = updated(items.map(StoreItem.from))

    items.zip(updatedItems).foreach {
      case (original, updated) => {
        original.sellIn = updated.sellIn
        original.quality = updated.quality
      }
    }
  }

  // C -> Copies on write, no side effects
  def updated(items: Seq[StoreItem]): Seq[StoreItem] = {
    items.map(item => updated(item))
  }

  private def updated(item: StoreItem): StoreItem = {
    val decreasedSellIn = item.decreaseSellBy

    val updatedItem =
      decreasedSellIn.name match {
        case AGED_BRIE => decreasedSellIn.increaseQuality()
        case BACKSTAGE_PASS => {
          decreasedSellIn.sellIn match {
            case x if x <= 5  => decreasedSellIn.increaseQuality(3)
            case x if x <= 10 => decreasedSellIn.increaseQuality(2)
            case _            => decreasedSellIn.increaseQuality()
          }
        }
        case _ => decreasedSellIn.decreaseQuality
      }

    val afterSellIn =
      if (updatedItem.sellIn < 0) {
        updatedItem.name match {
          case AGED_BRIE      => updatedItem.increaseQuality()
          case BACKSTAGE_PASS => worthless(updatedItem)
          case _              => updatedItem.decreaseQuality
        }
      } else updatedItem

    afterSellIn
  }

  // C -> copies argument, returns new value
  private def worthless(item: StoreItem) = {
    item.copy(quality = 0)
  }
}

// D -> immutable
case class StoreItem(name: String, sellIn: Int, quality: Int) {
  private val MIN_QUALITY = 0
  private val MAX_QUALITY = 50
  private val SULFURAS = "Sulfuras, Hand of Ragnaros"

  def increaseQuality(amount: Int = 1): StoreItem = {
    if (quality + amount > MAX_QUALITY)
      copy(quality = MAX_QUALITY)
    else copy(quality = quality + amount)
  }

  def decreaseQuality: StoreItem = {
    if (name == SULFURAS) this
    else if (quality > MIN_QUALITY)
      copy(quality = quality - 1)
    else copy(quality = 0)
  }

  def decreaseSellBy: StoreItem = {
    if (name == SULFURAS) this
    else copy(sellIn = sellIn - 1)
  }
}

object StoreItem {
  def from(item: Item): StoreItem =
    StoreItem(item.name, item.sellIn, item.quality)
}
