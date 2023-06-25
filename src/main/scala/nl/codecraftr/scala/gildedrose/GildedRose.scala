package nl.codecraftr.scala.gildedrose

/*
 * A -> Action / Side Effect
 * C -> Calculation / Pure Function
 * D -> Data / Facts about Events
 */
class GildedRose(val items: Array[Item]) {
  // A -> mutates state of the store
  def updateQuality(): Unit = {
    val updatedItems = updated(items.map(StoreItem.from))

    items.zip(updatedItems).foreach {
      case (original, updated) =>
          original.sellIn = updated.sellIn.value
          original.quality = updated.quality.value
    }
  }

  // C -> Copies on write, no side effects
  private def updated(items: Seq[StoreItem]): Seq[StoreItem] =
    items.map(updated)

  private def updated(item: StoreItem): StoreItem =
    item.updatedSellIn.updatedQuality
}
