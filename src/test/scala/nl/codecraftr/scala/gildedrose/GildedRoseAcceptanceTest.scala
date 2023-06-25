package nl.codecraftr.scala.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseAcceptanceTest extends AnyFlatSpec with Matchers {
  "gilded rose" should "update inventory correctly" in {
    val startingInventory = Array(
      new Item("+5 Dexterity Vest", 10, 20),
      new Item("Aged Brie", 2, 0),
      new Item("Elixir of the Mongoose", 5, 7),
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
      new Item("Sulfuras, Hand of Ragnaros", -1, 80),
      new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
      new Item("Backstage passes to a TAFKAL80ETC concert", 10, 49),
      new Item("Backstage passes to a TAFKAL80ETC concert", 5, 49)
      // this conjured item does not work properly yet
//      new Item("Conjured Mana Cake", 3, 6)
    )

    val inventoryAfterTwoDays = Array(
      new Item("+5 Dexterity Vest", 8, 18),
      new Item("Aged Brie", 0, 2),
      new Item("Elixir of the Mongoose", 3, 5),
      new Item("Sulfuras, Hand of Ragnaros", 0, 80),
      new Item("Sulfuras, Hand of Ragnaros", -1, 80),
      new Item("Backstage passes to a TAFKAL80ETC concert", 13, 22),
      new Item("Backstage passes to a TAFKAL80ETC concert", 8, 50),
      new Item("Backstage passes to a TAFKAL80ETC concert", 3, 50)
      // this conjured item does not work properly yet
//        new Item("Conjured Mana Cake", 3, 6)
    )

    val app = new GildedRose(startingInventory)

    (0 until 2)
      .foreach(_ => app.updateQuality())

    startingInventory
      .zip(inventoryAfterTwoDays)
      .foreach { case (actual, expected) =>
        actual.name shouldBe expected.name
        actual.sellIn shouldBe expected.sellIn
        actual.quality shouldBe expected.quality
      }
  }
}
