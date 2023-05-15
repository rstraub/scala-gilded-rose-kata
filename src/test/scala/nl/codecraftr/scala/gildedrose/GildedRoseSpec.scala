package nl.codecraftr.scala.gildedrose

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GildedRoseSpec  extends AnyFlatSpec with Matchers {
    it should "foo" in {
        val items = Array[Item](new Item("foo", 0, 0))
        val app = new GildedRose(items)
        app.updateQuality()
        app.items(0).name should equal ("fixme")
    }
}
