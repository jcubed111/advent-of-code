object Numbers{
    val numR = raw"([\d.]+)".r
    def unapplySeq(in: String) =
        numR.findAllIn(in).toList.map(_.toInt) match {
            case Nil => None
            case a => Some(a)
        }
}

val input = scala.io.Source.fromFile("input.txt").mkString.split("\n").toList

val pairs = input.map{
    case Numbers(a, b, c, d) => ((a to b).toSet, (c to d).toSet)
}

val part1 = pairs.filter({
    case (e1, e2) => e1.subsetOf(e2) || e2.subsetOf(e1)
}).size

println(part1)

val part2 = pairs.filter({
    case (e1, e2) => (e1 & e2).nonEmpty
}).size
println(part2)
