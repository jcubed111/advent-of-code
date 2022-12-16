val input = scala.io.Source.fromFile("input.txt").mkString

val elfTotals = input
    .split("\n\n")
    .map{
        elf => elf.split("\n").map(_.toInt).sum
    }

println(elfTotals.max)
println(elfTotals.sorted.takeRight(3).sum)
