import scala.annotation.tailrec


val input = scala.io.Source.fromFile("input.txt").mkString.trim.split(",").toList.map(_.toInt)

type Fish = Map[Int, Long]

def step(fish: Fish): Fish = {
    fish.toList.flatMap{
        case 0 -> v => List(6 -> v, 8 -> v)
        case k -> v => List(k - 1 -> v)
    }.groupMapReduce(_._1)(_._2)(_ + _).toMap
}

@tailrec
def stepN(fish: Fish, steps: Int): Fish = {
    if(steps <= 0) fish else stepN(step(fish), steps - 1)
}

val fs = input.groupBy(identity).map({case k -> v => k -> v.size.toLong}).toMap
val part1 = stepN(fs, 256).values.sum
println(part1)
