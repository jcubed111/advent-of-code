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

val LineRegex = raw"(\d+)-(\d+),(\d+)-(\d+)".r

println{

    val input = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .map(_.toList.map(_.toString.toInt))

    val width = input(0).size
    val height = input.size

    def get(x: Int, y: Int) = {
        if(x < 0 || y < 0 || x >= width || y >= height) -1 else input(y)(x)
    }

    val part1 = Range(0, width).flatMap(x => Range(0, height).map({y =>
        val left = Range(-1, x).map(get(_, y)).max
        val right = Range(x + 1, width + 1).map(get(_, y)).max
        val below = Range(-1, y).map(get(x, _)).max
        val above = Range(y + 1, height + 1).map(get(x, _)).max

        val self = get(x, y)
        self > left || self > right || self > above || self > below
    })).filter(identity).size

    val part2 = Range(0, width).flatMap(x => Range(0, height).map({y =>
        val left = Range(0, x).map(get(_, y)).reverse
        val right = Range(x + 1, width).map(get(_, y))
        val below = Range(0, y).map(get(x, _)).reverse
        val above = Range(y + 1, height).map(get(x, _))

        val self = get(x, y)

        val lookingOut = List(left,right,below,above)

        val score = lookingOut.map({sightLine =>
            (-1 :: sightLine.toList)
                .sliding(2, 1)
                .takeWhile{
                    case List(a, b) => a < self
                    case _ => false
                }
                .size
        }).reduce(_ * _)

        if(x == 2 && y == 3) {
            println((x, y, score))
            println(self)
            println(left)
            println(right)
            println(below)
            println(above)
        }

        score
    })).max

    part2
}
