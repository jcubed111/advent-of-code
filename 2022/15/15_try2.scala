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


    case class SensorRange(location: Vec2, range: Int) {
        def inRange(b: Vec2): Boolean = {
            b.mDist(location) <= range
        }

        def propagatePoint(p: Vec2): Vec2 = {
            // takes point p, and find the next point along the same y
            // where p is not in sensor range
            val dy = (location.y - p.y).abs
            Vec2(location.x + range - dy + 1, p.y)
        }
    }

    val sensors =
        scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .map{
            case Numbers(x1, y1, x2, y2) => Vec2(x1, y1) -> Vec2(x2, y2)
        }
        .map{
            case sensor -> beacon => SensorRange(sensor, sensor.mDist(beacon))
        }
        .sortBy(-_.location.x)  // improves speed

    // println(sensors)

    // part 2: RIP O(n^2)
    println{
        val maxValue = 4_000_000

        case class ResultException(result: Vec2) extends Exception;

        @tailrec
        def stepTestPoints(points: List[Vec2], i: Long = 0): Vec2 = {
            val checked = i + points.size.toLong
            val next = points.flatMap{case p =>
                sensors.find(_.inRange(p)) match {
                    case None =>
                        println{"Found after checking: " + checked.toString + " points."}
                        throw ResultException(p)
                    case Some(sensor) =>
                        val nextP = sensor.propagatePoint(p)
                        if(nextP.x <= maxValue) {
                            Some(nextP)
                        }else{
                            None
                        }
                }
            }
            stepTestPoints(next, checked)
        }

        val testPoints = Range(0, maxValue).map(n => Vec2(0, n)).toList
        val gap = try{
            stepTestPoints(testPoints)
        } catch {
            case ResultException(r) => r
        }

        println{gap}

        println{gap.x.asInstanceOf[Long] * 4000000L + gap.y.asInstanceOf[Long]}


        println(12817603219131L)
    }
