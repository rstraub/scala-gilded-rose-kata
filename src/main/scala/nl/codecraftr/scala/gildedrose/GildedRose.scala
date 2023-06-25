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
    val updatedItems = updated(items)

    items.zip(updatedItems).foreach {
      case (original, updated) => {
        original.sellIn = updated.sellIn
        original.quality = updated.quality
      }
    }
  }

  // C -> Copies on write, no side effects
  private def updated(items: Array[Item]): Array[Item] = {
    val updatedItems = items.clone()
    updatedItems.map(item => updated(item))
  }

  // C -> Copies on write, no side effects
  private def updated(item: Item): Item = {
    val updatedItem = copied(item)
    updatedItem.name match {
      case AGED_BRIE => increaseQuality(updatedItem)
      case BACKSTAGE_PASS => {
        updatedItem.sellIn match {
          case x if x <= 5  => increaseQuality(updatedItem, 3)
          case x if x <= 10 => increaseQuality(updatedItem, 2)
          case _            => increaseQuality(updatedItem)
        }
      }
      case _ => decreaseQuality(updatedItem)
    }

    decreaseSellBy(updatedItem)

    if (updatedItem.sellIn < 0) {
      updatedItem.name match {
        case AGED_BRIE      => increaseQuality(updatedItem)
        case BACKSTAGE_PASS => updatedItem.quality = 0
        case _              => decreaseQuality(updatedItem)
      }
    }
    updatedItem
  }

  // A -> mutates arguments, no return value
  private def decreaseSellBy(item: Item): Unit = {
    if (item.name == SULFURAS) return
    item.sellIn = item.sellIn - 1
  }

  // A -> mutates arguments, no return value
  private def increaseQuality(item: Item, amount: Int = 1): Unit = {
    item.quality = item.quality + amount
    if (item.quality > MAX_QUALITY)
      item.quality = MAX_QUALITY
  }

  // A -> mutates arguments, no return value
  private def decreaseQuality(item: Item): Unit = {
    if (item.name == SULFURAS) return
    if (item.quality > MIN_QUALITY)
      item.quality = item.quality - 1
  }

  private def copied(item: Item): Item = {
    new Item(item.name, item.sellIn, item.quality)
  }
}
