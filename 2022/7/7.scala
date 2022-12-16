val commands = scala.io.Source.fromFile("input.txt").mkString
    .split("\n\\$ ")
    .toList

case class File(dir: List[String], name: String, size: Int) {
    override def toString: String = {
        dir.mkString("/") + "/" + name + " " + size.toString
    }
}

val fsAsList: List[File] = commands
    .drop(1) // skip `cd /`
    .foldLeft(
        // starting state is no files seen, pwd=/
        (List[File](), List(""))
    ) {
        case ((fs, pwd), command) =>
            command.split("\n").toList match {
                case ls :: parts if ls == "ls" =>
                    (
                        fs ++ parts.flatMap(
                            _.split(" ") match{
                                case Array("dir", dirName) => List()
                                case Array(size, fileName) => List(File(pwd, fileName, size.toInt))
                            }
                        ).toList,
                        pwd
                    )
                case cd :: parts =>
                    // cd
                    val toDir = cd.split(" ")(1)
                    toDir match {
                        case ".." => (fs, pwd.dropRight(1))
                        case _ => (fs, pwd ++ Some(toDir))
                    }
                case Nil =>
                    throw new IllegalArgumentException("invalid input")
            }
    }._1

val totalSizeByDir = fsAsList
    .flatMap{
        // explode each file out to every directory it contributes size to
        // ie, `/a/b/c.txt @ 100`
        // becomes:
        // - / @ 100
        // - /a/ @ 100
        // - /a/b/ @ 100
        case File(dir, name, size) =>
            Range(1, dir.size + 1).map(
                numParts => dir.take(numParts).mkString("/") + "/" -> size
            )
    }
    // then sum up all the files in each dir
    .groupMapReduce(_._1)(_._2)(_ + _)

// println(fsAsList.sortBy(_.toString).mkString("\n"))
// println(totalSizeByDir)

val part1 = totalSizeByDir.values.filter(_ <= 100000).sum
println(part1)

val unusedSpace = 70000000 - totalSizeByDir("/")
val neededSpace = 30000000 - unusedSpace
val part2 = totalSizeByDir.values.filter(_ >= neededSpace).min
println(part2)

// 1844187
// 4978279
