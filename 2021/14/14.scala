val Array(start, keyStr) = scala.io.Source.fromFile("input.txt").mkString
    .split("\n\n")

val key = keyStr.split("\n").map(_.split(" -> ") match {case Array(a, b) => (a, b(0))}).toMap

val startMap = (" " + start + " ").sliding(2, 1).toList.groupBy(identity).view.mapValues(_.size.toLong).toMap

def step(m: Map[String, Long]): Map[String, Long] = {
    m.toList.flatMap{
        case (pair, count) =>
            if(key.contains(pair)) {
                val insert = key.get(pair).get
                List(
                    List(pair(0), insert).mkString -> count,
                    List(insert, pair(1)).mkString -> count,
                )
            }else{
                List(pair -> count)
            }
    }.toList.groupMapReduce(_._1)(_._2)(_ + _)
}

val end = Range(0, 40).foldLeft(startMap){case (m, _) => step(m)}
val endCounts = end.toList
    .flatMap{
        case (k, count) => List(k(0) -> count, k(1) -> count)
    }
    .filter{case (k, _) => k != ' '}
    .groupMapReduce(_._1)(_._2)(_ + _)
    .view.mapValues(_ / 2)
    .toMap
val part1 = endCounts.values.max - endCounts.values.min
println(part1)



// val part1 = ???
// println(part1)
