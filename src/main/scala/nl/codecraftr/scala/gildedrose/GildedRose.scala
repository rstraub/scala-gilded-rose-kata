package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  private val MAX_QUALITY = 50

  private val AGED_BRIE = "Aged Brie"
  private val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"
  private val SULFURAS = "Sulfuras, Hand of Ragnaros"

  // A
  def updateQuality(): Unit = {
    items.foreach(item => {
      if (item.name.equals(AGED_BRIE) || item.name.equals(BACKSTAGE_PASS)) {
        if (item.quality < 50) {
          increaseQuality(item)

          if (item.name.equals(BACKSTAGE_PASS)) {
            if (item.sellIn < 11) {
              increaseQuality(item)

            }

            if (item.sellIn < 6) {
              increaseQuality(item)
            }
          }
        }
      } else {
        if (item.quality > 0) {
          if (!item.name.equals(SULFURAS)) {
            decreaseQuality(item)
          }
        }
      }

      if (!item.name.equals(SULFURAS)) {
        decreaseSellBy(item)
      }

      if (item.sellIn < 0) {
        if (!item.name.equals(AGED_BRIE)) {
          if (!item.name.equals(BACKSTAGE_PASS)) {
            if (item.quality > 0) {
              if (!item.name.equals(SULFURAS)) {
                decreaseQuality(item)
              }
            }
          } else {
            item.quality = 0
          }
        } else {
          increaseQuality(item)
        }
      }
    })
  }

  private def decreaseSellBy(item: Item): Unit = {
    item.sellIn = item.sellIn - 1
  }

  private def increaseQuality(item: Item): Unit = {
    if (item.quality < MAX_QUALITY)
      item.quality = item.quality + 1
  }

  private def decreaseQuality(item: Item): Unit = {
    item.quality = item.quality - 1
  }
}
