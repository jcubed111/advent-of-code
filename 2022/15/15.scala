import scala.annotation.tailrec

/**************** Vectors ****************/
case class Vec2(x: Int, y: Int) {
    def +(b: Vec2) = Vec2(x + b.x, y + b.y)
    def -(b: Vec2) = Vec2(x - b.x, y - b.y)
    def ==(b: Vec2) = x == b.x && y == b.y
    def *(b: Int) = Vec2(x * b, y * b)
    def dot(b: Vec2) = x * b.x + y * b.y
    def mDist(b: Vec2): Int = (x - b.x).abs + (y - b.y).abs
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



    val sensorsToBeacons = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .map{
            case Numbers(x1, y1, x2, y2) => Vec2(x1, y1) -> Vec2(x2, y2)
        }
    val sensorsToRange =
        sensorsToBeacons
        .map{
            case sensor -> beacon => sensor -> sensor.mDist(beacon)
        }

    println(sensorsToRange)

    def inSensorRange(a: Vec2): Boolean = {
        sensorsToRange.exists{
            case sensor -> range => a.mDist(sensor) <= range
        }
    }

    // part 1
    println{
        val minX = sensorsToRange.map{case sensor -> range => sensor.x - range}.min - 2
        val maxX = sensorsToRange.map{case sensor -> range => sensor.x + range}.max + 2

        val testY = 2000000

        val beaconsInRow = sensorsToBeacons.map(_._2).filter(_.y == testY).map(_.x).toSet.size

        Range(minX, maxX + 1).filter(x => inSensorRange(Vec2(x, testY))).size - beaconsInRow
    }

    // part 2: RIP O(n^2)
    println{
        val maxValue = 4000000

        val xRanges = sensorsToRange
            .flatMap{
                case sensor -> range =>
                    Range(-range, range + 1).map{
                        case x =>
                            (x + sensor.x) -> (
                                sensor.y - (range - x.abs),
                                sensor.y + (range - x.abs)
                            )
                    }
            }.filter{
                case (x , (_, _)) => (x >= 0 && x <= maxValue)
            }.map{
                case (x, (yMin, yMax)) =>
                    x -> (List(0, yMin).max, List(maxValue, yMax).min)
            }
            .groupBy(_._1)
            .view.mapValues(_.map(_._2).sortBy(_._1))
            .toMap

        // println(xRanges)

        def findGap: Vec2 = {
            xRanges.map{
                case x -> yRanges =>
                    yRanges
                        .foldLeft(0){
                            case (prevMax, (nextMin, nextMax)) =>
                                if(nextMin > prevMax + 1) {
                                    return Vec2(x, prevMax + 1)
                                    ???
                                }else{
                                    List(prevMax, nextMax).max
                                }
                        }
            }
            ???
        }

        val gap = findGap

        println{gap}

        gap.x.asInstanceOf[Long] * 4000000L + gap.y.asInstanceOf[Long]

    }
