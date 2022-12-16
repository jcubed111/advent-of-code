val input = scala.io.Source.fromFile("input.txt").mkString.split("\n").toList


case class Line(x1: Int, y1: Int, x2: Int, y2: Int) {
    def isDiag: Boolean = x1 != x2 && y1 != y2
    def points: List[(Int, Int)] = {
        val xs = x1 to x2 by (if(x2 < x1) -1 else 1)
        val ys = y1 to y2 by (if(y2 < y1) -1 else 1)
        xs.zipAll(ys, xs.head, ys.head).toList
    }
}

val lineR = raw"(\d+),(\d+) -> (\d+),(\d+)".r
val lines = input.map{
    case lineR(x1, y1, x2, y2) => Line(x1.toInt, y1.toInt, x2.toInt, y2.toInt)
}

val part1 = lines.flatMap(_.points).groupBy(identity).values.map(_.size).filter(_ > 1).size
println(part1)
