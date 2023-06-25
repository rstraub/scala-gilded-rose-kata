package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  private val MIN_QUALITY = 0
  private val MAX_QUALITY = 50

  private val AGED_BRIE = "Aged Brie"
  private val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  private val SULFURAS = "Sulfuras, Hand of Ragnaros"

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

  // C -> Copies on write, no side effects
  private def updated(item: StoreItem): StoreItem = {
    val updatedItem =
      item.name match {
        case AGED_BRIE => increaseQuality(item)
        case BACKSTAGE_PASS => {
          item.sellIn match {
            case x if x <= 5  => increaseQuality(item, 3)
            case x if x <= 10 => increaseQuality(item, 2)
            case _            => increaseQuality(item)
          }
        }
        case _ => decreaseQuality(item)
      }

    val decreasedSellIn = decreaseSellBy(updatedItem)

    val afterSellIn =
      if (decreasedSellIn.sellIn < 0) {
        decreasedSellIn.name match {
          case AGED_BRIE      => increaseQuality(decreasedSellIn)
          case BACKSTAGE_PASS => withQuality(decreasedSellIn, 0)
          case _              => decreaseQuality(decreasedSellIn)
        }
      } else decreasedSellIn

    afterSellIn
  }

  // C -> copies argument, returns new value
  private def increaseQuality(item: StoreItem, amount: Int = 1): StoreItem = {
    if (item.quality + amount > MAX_QUALITY)
      withQuality(item, MAX_QUALITY)
    else withQuality(item, item.quality + amount)
  }

  // C -> copies argument, returns new value
  private def decreaseQuality(item: StoreItem): StoreItem = {
    if (item.name == SULFURAS) item
    else if (item.quality > MIN_QUALITY)
      withQuality(item, item.quality - 1)
    else withQuality(item, 0)
  }

  // C -> copies argument, returns new value
  private def withQuality(item: StoreItem, quality: Int): StoreItem = {
    item.copy(quality = quality)
  }

  // C -> copies argument, returns new value
  private def decreaseSellBy(item: StoreItem): StoreItem = {
    if (item.name == SULFURAS) item
    else withSellIn(item, item.sellIn - 1)
  }

  private def withSellIn(item: StoreItem, sellIn: Int): StoreItem = {
    item.copy(sellIn = sellIn)
  }
}

// D -> immutable
case class StoreItem(name: String, sellIn: Int, quality: Int)

object StoreItem {
  def from(item: Item): StoreItem = StoreItem(item.name, item.sellIn, item.quality)
}
