package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  private val AGED_BRIE = "Aged Brie"

  private val BACKSTAGE_PASS = "Backstage passes to a TAFKAL80ETC concert"

  private val SULFURAS = "Sulfuras, Hand of Ragnaros"

  // A
  def updateQuality(): Unit = {
    items.foreach(item => {
      if (item.name.equals(AGED_BRIE) || item.name.equals(BACKSTAGE_PASS)) {
        if (item.quality < 50) {
          item.quality = increaseQuality(item)

          if (item.name.equals(BACKSTAGE_PASS)) {
            if (item.sellIn < 11) {
              if (item.quality < 50) {
                item.quality = increaseQuality(item)
              }
            }

            if (item.sellIn < 6) {
              if (item.quality < 50) {
                item.quality = increaseQuality(item)
              }
            }
          }
        }
      } else {
        if (item.quality > 0) {
          if (!item.name.equals(SULFURAS)) {
            item.quality = decreaseQuality(item)
          }
        }
      }

      if (!item.name.equals(SULFURAS)) {
        item.sellIn = decreaseSellBy(item)
      }

      if (item.sellIn < 0) {
        if (!item.name.equals(AGED_BRIE)) {
          if (!item.name.equals(BACKSTAGE_PASS)) {
            if (item.quality > 0) {
              if (!item.name.equals(SULFURAS)) {
                item.quality = decreaseQuality(item)
              }
            }
          } else {
            item.quality = 0
          }
        } else {
          if (item.quality < 50) {
            item.quality = increaseQuality(item)
          }
        }
      }
    })
  }

  private def decreaseSellBy(item: Item) = {
    item.sellIn - 1
  }

  private def increaseQuality(item: Item) = {
    item.quality + 1
  }

  private def decreaseQuality(item: Item) = {
    item.quality - 1
  }
}
