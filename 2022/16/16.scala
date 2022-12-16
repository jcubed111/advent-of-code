import scala.annotation.tailrec

/**************** Vectors ****************/
case class Vec2(x: Int, y: Int) {
    def +(b: Vec2) = Vec2(x + b.x, y + b.y)
    def -(b: Vec2) = Vec2(x - b.x, y - b.y)
    def ==(b: Vec2) = x == b.x && y == b.y
    def *(b: Int) = Vec2(x * b, y * b)
    def dot(b: Vec2) = x * b.x + y * b.y
}

class Grid[T](input: String) {
    val map = input
        .split("\n")
        .toList
        .zipWithIndex
        .flatMap{
            case (l, i) =>
                l.toList.zipWithIndex.map{
                    case (c, j) => Vec2(i, j) -> normalizer(c)
                }
        }
        .toMap

    def normalizer(in: Char): T = in.asInstanceOf[T]

    def apply(at: Vec2, default: T) = map.getOrElse(at, default)
}

/**************** Matchers ****************/
object Numbers{
    val numR = raw"(-?[\d.]+)".r
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


/**************** Solution ****************/

println{

    case class Input(valve: String, rate: Int, to: List[String]) {
        def connections = valve -> to
    }

    val input = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .map{
            case Words(_, valve, _, _, Numbers(rate), _, _, _, _, dests @ _*) =>
                Input(valve, rate, dests.toList.map(_.substring(0, 2)))
        }

    val connectionsByValve = input.map(_.connections).toMap
    val rateByValve = input.map(i => i.valve -> i.rate).toMap

    def depthViaBfs(visited: Set[String], d: Int = 0): Map[String, Int] = {
        // returns map of dest -> dist
        // never returns source -> source
        val nextSteps = visited.flatMap(connectionsByValve(_)).toSet -- visited
        if(nextSteps.size == 0) {
            Map()
        }else{
            (
                nextSteps.map(_ -> (d + 1)).toMap
                ++ depthViaBfs(visited ++ nextSteps, d + 1)
            )
        }
    }

    val connectionsWithDist: Map[String, Map[String, Int]] =
        connectionsByValve.keys.map{
            case source =>
                source -> depthViaBfs(Set(source)).filter{
                    case to -> dist =>
                        // we'll never move to a blocked valve
                        rateByValve(to) > 0
                }
        }.toMap

    // input

    // strategy:
    // start with Path(0, "AA", Nil, 0)
    // each step, find the path with max maxFutureTotal, and turn
    // it into all it's possible next steps.
    // Stop once the Path with max maxFutureTotal has no possibleSteps,
    // that one is the best path.

    case class Path(
        time: Int,
        at: String,
        turnedOn: Set[String],
        pressureTotal: Int,
    ) {
        lazy val maxFutureTotal: Int = {
            // it takes at least two steps to turn on a valve
            val bestValvesRatesRemaining = rateByValve.filter{
                case key -> value => !turnedOn.contains(key) && value > 0
            }.values.toList.sortBy(-_)

            Range(30 - time - 2, 0, -2).zip(bestValvesRatesRemaining).map{
                case timeRemaining -> rate => timeRemaining * rate
            }.sum + pressureTotal
        }

        def possibleSteps: Set[Path] = {
            val timeRemaining = 30 - time
            if(timeRemaining <= 0) {
                Set()
            } else {
                Set(
                    // wait is always an option
                    this.copy(time=30)
                ) ++ {
                    connectionsWithDist(at).filter{
                        case to -> dist =>
                            !turnedOn.contains(to) && dist + 1 < timeRemaining
                    }.map{
                        case to -> dist =>
                            val newTime = time + dist + 1  // +1 because we have to turn the valve on
                            val newRemaining = 30 - newTime
                            Path(
                                newTime,
                                to,
                                turnedOn + to,
                                pressureTotal + newRemaining * rateByValve(to)
                            )
                    }
                }
            }
        }
    }

    val basePaths = Set(
        Path(0, "AA", Set(), 0),
        Path(1, "AA", Set("AA"), rateByValve("AA") * 29),  // if we turn on AA
    )

    @tailrec
    def findBest(ends: Set[Path]): Path = {
        val bestToExplore = ends.maxBy(_.maxFutureTotal)
        println{bestToExplore}
        val nexts = bestToExplore.possibleSteps
        if(nexts.isEmpty) {
            bestToExplore
        }else{
            findBest((ends - bestToExplore) ++ nexts)
        }
    }

    val best = findBest(basePaths)  // 1862
    println{"Best:"}
    best

}
