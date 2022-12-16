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
        meTime: Int,
        meAt: String,
        eleTime: Int,
        eleAt: String,
        turnedOn: Set[String],
        pressureTotal: Int,
    ) {
        lazy val maxFutureTotal: Int = {
            // it takes at least two steps to turn on a valve
            val bestValvesRatesRemaining = rateByValve.filter{
                case key -> value => !turnedOn.contains(key) && value > 0
            }.values.toList.sortBy(-_)

            val myMoves = Range(30 - meTime - 2, 0, -2)
            val eleMoves = Range(30 - eleTime - 2, 0, -2)

            (myMoves ++ eleMoves).sortBy(-_).zip(bestValvesRatesRemaining).map{
                case timeRemaining -> rate => timeRemaining * rate
            }.sum + pressureTotal
        }

        def possibleSteps: Set[Path] = {
            if(meTime >= 30 && eleTime >= 30) {
                Set()
            }else if(meTime < eleTime) {
                stepsForMe
            }else{
                stepsForEle
            }
        }

        def stepsForMe: Set[Path] = {
            val timeRemaining = 30 - meTime
            if(timeRemaining <= 0) {
                Set()
            } else {
                Set(
                    // wait is always an option
                    this.copy(meTime=30)
                ) ++ {
                    connectionsWithDist(meAt).filter{
                        case to -> dist =>
                            !turnedOn.contains(to) && dist + 1 < timeRemaining
                    }.map{
                        case to -> dist =>
                            val newTime = meTime + dist + 1  // +1 because we have to turn the valve on
                            val newRemaining = 30 - newTime
                            Path(
                                newTime,
                                to,
                                eleTime,
                                eleAt,
                                turnedOn + to,
                                pressureTotal + newRemaining * rateByValve(to)
                            )
                    }
                }
            }
        }

        def stepsForEle: Set[Path] = {
            val timeRemaining = 30 - eleTime
            if(timeRemaining <= 0) {
                Set()
            } else {
                Set(
                    // wait is always an option
                    this.copy(eleTime=30)
                ) ++ {
                    connectionsWithDist(eleAt).filter{
                        case to -> dist =>
                            !turnedOn.contains(to) && dist + 1 < timeRemaining
                    }.map{
                        case to -> dist =>
                            val newTime = eleTime + dist + 1  // +1 because we have to turn the valve on
                            val newRemaining = 30 - newTime
                            Path(
                                meTime,
                                meAt,
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
        Path(4, "AA", 4, "AA", Set(), 0),
    )

    @tailrec
    def findBest(ends: Set[Path]): Path = {
        val bestToExplore = ends.maxBy(_.maxFutureTotal)
        println{(ends.size, bestToExplore)}

        val nexts = bestToExplore.possibleSteps
        if(nexts.isEmpty) {
            bestToExplore
        }else{
            val newEnds = (ends - bestToExplore) ++ nexts
            val highestRealized = newEnds.map(_.pressureTotal).max
            findBest(
                newEnds.filter{
                    // throw out anything where the maxFutureTotal < another end's released
                    case e => e.maxFutureTotal >= highestRealized
                }
            )
        }
    }

    val best = findBest(basePaths)  // takes 10 minutes to run lol
    println{"Best:"}
    best

}
