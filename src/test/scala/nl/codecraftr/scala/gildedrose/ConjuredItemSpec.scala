package nl.codecraftr.scala.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConjuredItemSpec extends AnyFlatSpec with Matchers {
  private val anItem = ConjuredItem("Conjured Mana Cake", SellIn(9), Quality(10))

  "updated" should "reduce quality by 2 given sell by date hasn't passed" in {
    val item = anItem.copy(sellIn = SellIn(1), quality = Quality(10))

    val updatedItem = item.updated

    updatedItem.sellIn shouldBe SellIn(0)
    updatedItem.quality shouldBe Quality(10 - 2)
  }

  it should "reduce quality by 4 given sell by date passed" in {
    val item = anItem.copy(sellIn = SellIn(0), quality = Quality(10))

    val updatedItem = item.updated

    updatedItem.sellIn shouldBe SellIn(-1)
    updatedItem.quality shouldBe Quality(10 - 4)
  }
}
