package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  // A
  def updateQuality(): Unit = {
    items.foreach(item => {
      if (
        !item.name.equals("Aged Brie")
        && !item.name.equals("Backstage passes to a TAFKAL80ETC concert")
      ) {
        if (item.quality > 0) {
          if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
            item.quality = decreaseQuality(item)
          }
        }
      } else {
        if (item.quality < 50) {
          item.quality = increaseQuality(item)

          if (item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
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
      }

      if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
        item.sellIn = item.sellIn - 1
      }

      if (item.sellIn < 0) {
        if (!item.name.equals("Aged Brie")) {
          if (!item.name.equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (item.quality > 0) {
              if (!item.name.equals("Sulfuras, Hand of Ragnaros")) {
                item.quality = decreaseQuality(item)
              }
            }
          } else {
            item.quality = item.quality - item.quality
          }
        } else {
          if (item.quality < 50) {
            item.quality = increaseQuality(item)
          }
        }
      }
    })
  }

  private def increaseQuality(item: Item) = {
    item.quality + 1
  }

  private def decreaseQuality(item: Item) = {
    item.quality - 1
  }
}
