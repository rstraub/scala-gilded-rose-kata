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
  private def increaseQuality(item: Item, amount: Int = 1): Item = {
    if (item.quality + amount > MAX_QUALITY)
      withQuality(item, MAX_QUALITY)
    else withQuality(item, item.quality + amount)
  }

  // C -> copies argument, returns new value
  private def decreaseQuality(item: Item): Item = {
    if (item.name == SULFURAS) item
    else if (item.quality > MIN_QUALITY)
      withQuality(item, item.quality - 1)
    else withQuality(item, 0)
  }

  // C -> copies argument, returns new value
  private def withQuality(item: Item, quality: Int): Item = {
    val copy = copied(item)
    copy.quality = quality
    copy
  }

  // C -> copies argument, returns new value
  private def decreaseSellBy(item: Item): Item = {
    if (item.name == SULFURAS) item
    else withSellIn(item, item.sellIn - 1)
  }

  private def withSellIn(item: Item, sellIn: Int): Item = {
    val copy = copied(item)
    copy.sellIn = sellIn
    copy
  }

  private def copied(item: Item): Item = {
    new Item(item.name, item.sellIn, item.quality)
  }
}
