val List(dotCoords, instructions) = scala.io.Source.fromFile("input.txt").mkString
    .split("\n\n")
    .toList
    .map(_.split("\n").toList)

val dots = dotCoords.map(_.split(",").map(_.toInt)).map({case Array(a, b) => (a, b)}).toSet


val foldX = raw"fold along x=(\d+)".r
val foldY = raw"fold along y=(\d+)".r

abstract class Fold(){
    def apply(dots: Set[(Int, Int)]): Set[(Int, Int)] = ???
}
class FoldX(x: Int) extends Fold {
    override def apply(dots: Set[(Int, Int)]): Set[(Int, Int)] = {
        dots.map{
            case (dx, dy) if dx < x => (dx, dy)
            case (dx, dy) => (2 * x - dx, dy)
        }
    }
}
class FoldY(y: Int) extends Fold {
    override def apply(dots: Set[(Int, Int)]): Set[(Int, Int)] = {
        dots.map{
            case (dx, dy) if dy < y => (dx, dy)
            case (dx, dy) => (dx, 2 * y - dy)
        }
    }
}

val folds: List[Fold] = instructions.map{
    case foldX(x) => new FoldX(x.toInt)
    case foldY(y) => new FoldY(y.toInt)
}.toList


def doFolds(dots: Set[(Int, Int)], folds: List[Fold]): Set[(Int, Int)] = folds match {
    case Nil => dots
    case f :: rest => doFolds(f.apply(dots), rest)
}

val part1 = folds.head.apply(dots)
println(part1.size)

val part2 = doFolds(dots, folds)
(0 to 50).foreach{y =>
    println(
        (0 to 100)
            .map(x => if(part2.contains((x, y))) '#' else '.')
            .mkString
    )
}
println(part2)
