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

    val input = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList

    ???

}
