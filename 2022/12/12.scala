object Numbers{
    val numR = raw"([\d.]+)".r
    def unapplySeq(in: String) =
        numR.findAllIn(in).toList.map(_.toInt) match {
            case Nil => None
            case a => Some(a)
        }
}

object Words{
    val numR = raw"(\S+)".r
    def unapplySeq(in: String) =
        numR.findAllIn(in).toList match {
            case Nil => None
            case a => Some(a)
        }
}

object Chars{
    def unapplySeq(in: String) = Some(in.toList)
}

object Int{
    def unapply(in: String) = scala.util.Try(in.toInt).toOption
}


implicit class UtilList[U](self: List[U]) {
    def mapLeftAcc[T, V](initialState: T)(cb: ((T, U)) => (T, V)): List[V] = {
        self.foldLeft((initialState, List[V]())) {
            case ((acc, outs), next) =>
                val (nextAcc, out) = cb(acc, next)
                (nextAcc, outs ++ List(out))
        }._2
    }
}


val LineRegex = raw"(\d+)-(\d+),(\d+)-(\d+)".r

println{

    val input = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .zipWithIndex
        .flatMap{case (l, i) => l.toList.zipWithIndex.map{
            case (c, j) =>
                (i, j) -> (c match {
                    case 'S' => 0
                    case _ => (c - 'a')
                })
        }}
        .toMap

    val end: (Int, Int) = input.find{case (_, v) => v == 'E' - 'a'}.get._1

    val world = input ++ Map(end -> 25)

    val starts: List[(Int, Int)] = world.toList.filter{case (_, v) => v == 0}.map(_._1).toList

    def getHeight(coord: (Int, Int)) = world.getOrElse(coord, 100)

    case class Step(x: Int, y: Int, distFromStart: Int) {
        def coord = (x, y)
    }

    def evaluateStart(start: (Int, Int)): Int = {
        var fringe = Set(Step(start._1, start._2, 0))
        var visited = Set[(Int, Int)]()

        while(fringe.size > 0) {
            val current = fringe.minBy{
                case Step(x, y, d) => (x - end._1).abs + (y - end._2).abs + d
            }
            fringe = fringe - current

            val Step(x, y, dist) = current

            if(current.coord == end) {
                return dist
            }

            visited = visited + current.coord

            val currentHeight = getHeight(current.coord)
            val adjacents = Set(
                Step(x + 1, y, dist + 1),
                Step(x - 1, y, dist + 1),
                Step(x, y + 1, dist + 1),
                Step(x, y - 1, dist + 1),
            )
            .filter{
                case Step(i, j, d) => {
                    (getHeight(i, j) <= currentHeight + 1) && !visited.contains((i, j))
                }
            }

            fringe = fringe ++ adjacents
        }

        1000000
    }

    starts.map(evaluateStart _).min

}
