println{
scala.io.Source.fromFile("input.txt").mkString
    .split("\n\\$ ")
    .toList
    .drop(1)
    .foldLeft(
        (List[(List[String], String, Int)](), List(""))
    ) {
        case ((fs, pwd), command) =>
            command.split("\n").toList match {
                case ls :: parts if ls == "ls" =>
                    (
                        fs ++ parts.flatMap(
                            _.split(" ") match{
                                case Array("dir", dirName) => None
                                case Array(size, fileName) => Some((pwd, fileName, size.toInt))
                            }
                        ).toList,
                        pwd
                    )
                case cd :: parts =>
                    cd.split(" ")(1) match {
                        case ".." => (fs, pwd.dropRight(1))
                        case a => (fs, pwd ++ Some(a))
                    }
                case Nil => ???
            }
    } match { case (fsAsList, _) =>
        (fsAsList
                    .flatMap{
                        case (dir, name, size) =>
                            (1 to dir.size).map(
                                numParts => dir.take(numParts).mkString("/") + "/" -> size
                            )
                    }
                    // then sum up all the files in each dir
                    .groupMapReduce(_._1)(_._2)(_ + _) match {
                        case a => List(
                            a.values.filter(_ <= 100000).sum,
                            a.values.filter(_ >= 30000000 - (70000000 - a("/"))).min
                        )
                    }
                    )

    }


}
