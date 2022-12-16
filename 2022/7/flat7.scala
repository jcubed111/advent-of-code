val commands = scala.io.Source.fromFile("input.txt").mkString
    .split("\n\\$ ")
    .toList

val totalSizeByDir = commands
    .drop(1) // skip `cd /`
    .map(_.split("\n").toList.map(_.split(" ").toList))
    .foldLeft(
        // starting state is no files seen, pwd=/
        List("/") -> List("" -> 0)
    ) {
        case (pwd -> fs, List("ls") :: output) =>
            pwd -> output
                .map(_.head)
                .filter(_ != "dir")
                .flatMap(s => pwd.map(_ -> s.toInt))
                .++(fs)
        case ((_ :: p) -> fs, List("cd", "..") :: _) => p -> fs
        case (pwd -> fs, List(_, to) :: _) => (pwd.head + to + "/" :: pwd) -> fs
        case _ => ???
    }._2
    .groupMapReduce(_._1)(_._2)(_ + _)

val part1 = totalSizeByDir.values.filter(_ <= 100000).sum
println(part1)

val unusedSpace = 70000000 - totalSizeByDir("/")
val neededSpace = 30000000 - unusedSpace
val part2 = totalSizeByDir.values.filter(_ >= neededSpace).min
println(part2)

// 1844187
// 4978279
