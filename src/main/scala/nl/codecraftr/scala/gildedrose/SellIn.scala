package nl.codecraftr.scala.gildedrose

case class SellIn(value: Int) {
  def <(other: Int): Boolean = value < other
  def <=(other: Int): Boolean = value <= other
  def decreased: SellIn = SellIn(value - 1)
}
