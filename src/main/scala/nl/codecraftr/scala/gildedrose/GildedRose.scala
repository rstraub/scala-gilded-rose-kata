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

  // A
  def updateQuality(): Unit = {
    val updatedItems = items.clone()

    updateItems(updatedItems)

    items.zip(updatedItems).foreach {
      case (original, updated) => {
        original.sellIn = updated.sellIn
        original.quality = updated.quality
      }
    }
  }

  // A -> mutates arguments, no return value
  private def updateItems(updatedItems: Array[Item]): Unit = {
    updatedItems.foreach(item => {
      if (item.name == AGED_BRIE || item.name == BACKSTAGE_PASS) {
        increaseQuality(item)

        if (item.name == BACKSTAGE_PASS) {
          if (item.sellIn <= 10) {
            increaseQuality(item)
          }

          if (item.sellIn <= 5) {
            increaseQuality(item)
          }
        }
      } else {
        decreaseQuality(item)
      }

      decreaseSellBy(item)

      if (item.sellIn < 0) {
        item.name match {
          case AGED_BRIE      => increaseQuality(item)
          case BACKSTAGE_PASS => item.quality = 0
          case _              => decreaseQuality(item)
        }
      }
    })
  }

  // A -> mutates arguments, no return value
  private def decreaseSellBy(item: Item): Unit = {
    if (item.name == SULFURAS) return
    item.sellIn = item.sellIn - 1
  }

  // A -> mutates arguments, no return value
  private def increaseQuality(item: Item, amount: Int = 1): Unit = {
    if (item.quality < MAX_QUALITY)
      item.quality = item.quality + amount
  }

  // A -> mutates arguments, no return value
  private def decreaseQuality(item: Item): Unit = {
    if (item.name == SULFURAS) return
    if (item.quality > MIN_QUALITY)
      item.quality = item.quality - 1
  }
}
