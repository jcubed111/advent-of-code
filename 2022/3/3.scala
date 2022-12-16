val input = scala.io.Source.fromFile("input.txt").mkString

case class Rucksack(a: List[Char], b: List[Char]) {
    def commonItem: Char = {
        val as = a.toSet
        val bs = b.toSet
        (as & bs).head
    }

    def itemSet: Set[Char] = a.toSet | b.toSet
}

def itemPriority(x: Char): Int = x match {
    case x if x <= 'Z' => x - 'A' + 1 + 26
    case _ => x - 'a' + 1
}

val rucksacks = input
    .trim
    .split("\n")
    .toList
    .map(_.toList)
    .map(items => items.splitAt(items.size / 2))
    .map({
        case (a, b) => Rucksack(a, b)
    })


val part1 = rucksacks.map(_.commonItem).map(itemPriority).sum

println(part1)


val part2 = rucksacks.grouped(3).map{
    case List(a, b, c) => (a.itemSet & b.itemSet & c.itemSet).head
}.map(itemPriority).sum
println(part2)
