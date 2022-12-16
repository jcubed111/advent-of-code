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


/**************** Solution ****************/

println{

    type World = Map[Vec2, Char]

    val world: World = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .flatMap{
            case Numbers(ns @ _*) =>
                ns.grouped(2).map{
                    case Seq(a, b) => Vec2(a, b)
                }
                .sliding(2, 1)
                .map(_.toList)
                .toList
                .map{
                    case List(Vec2(a, b), Vec2(c, d)) if a == c =>
                        val ends = List(b, d)
                        Range(ends.min, ends.max + 1).map(y => Vec2(a, y))
                    case List(Vec2(a, b), Vec2(c, d)) if b == d =>
                        val ends = List(a, c)
                        Range(ends.min, ends.max + 1).map(x => Vec2(x, b))
                    case _ => ???
                }
                .flatten
        }
        .map(_ -> '#')
        .toMap

    val floorY = world.keys.map(_.y).max + 2

    def printMap(m: World) = {
        val minX = m.keys.map(_.x).min
        val maxX = m.keys.map(_.x).max
        val minY = m.keys.map(_.y).min
        val maxY = m.keys.map(_.y).max

        println(Range(0, maxY + 1).map{
            case y =>
                Range(minX, maxX + 1).map{
                    case x => m.getOrElse(Vec2(x, y), '.')
                }.mkString
        }.mkString("\n"))
        println(Vec2(minX, minY))
        println(Vec2(maxX, maxY))
    }

    def filled(pos: Vec2)(implicit w: World) = w.contains(pos)

    @tailrec
    def sandEndPos(sandPos: Vec2 = Vec2(500, 0))(implicit w: World): Vec2 = {
        if(sandPos.y < floorY - 1) {
            val falls = List(Vec2(0, 1), Vec2(-1, 1), Vec2(1, 1)).map(_ + sandPos)
            falls.find(!filled(_)) match {
                case Some(to) =>
                    sandEndPos(to)
                case None =>
                    return sandPos
            }
        }else{
            return sandPos
        }
    }

    @tailrec
    def fill(w: World, i: Int = 0): Int = {
        // printMap(w)
        sandEndPos()(w) match {
            case Vec2(500, 0) => i + 1
            case x => fill(w ++ Map(x -> 'o'), i + 1)
        }
    }


    fill(world)

}
