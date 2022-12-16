import scala.collection.mutable.ListBuffer


val Array(stacksStr, movesStr) = scala.io.Source.fromFile("input.txt").mkString
    .split("\n\n")


val lineR = raw"move (\d+) from (\d+) to (\d+)".r

val parsedLines = movesStr.split("\n").toList.map{
    case lineR(a, b, c) => (a.toInt, b.toInt, c.toInt)
}

val stackLines = stacksStr.split("\n")
val longestLineLen = stackLines.map(_.trim.size).max
val numStacks = (longestLineLen + 1) / 4
val stacks = Range(0, numStacks).map(
    stackIndex =>
        stackLines.dropRight(1).map(
            line =>
                line.toList.lift(stackIndex * 4 + 1).getOrElse(' ')
        ).toList.filter(_ != ' ')
).toList

println(stacks)

val endStacks = parsedLines.foldLeft(stacks) {
    case (stacks: List[List[Char]], (num, from, to)) =>
        val popped = stacks(from - 1).take(num)
        stacks
            .updated(from - 1, stacks(from - 1).drop(num))
            .updated(to - 1, popped ++ stacks(to - 1))
}

val part1 = endStacks.map(_.headOption.getOrElse(' ')).mkString
println(part1)
