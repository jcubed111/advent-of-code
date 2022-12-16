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

println{

    val l = scala.io.Source.fromFile("input.txt").mkString
        .split("\n")
        .toList
        .flatMap{
            case Words("noop") => List(0)
            case Words("addx", Int(x)) => List(0, x)
        }
        .mapLeftAcc(1) {
            case (acc, toAdd) =>
                val n = acc + toAdd
                (n, n)
        }
        .zipWithIndex
        .map{case (value, i) => (i + 2, value)}
        // part1
        // .filter{case (cycle, x) => (cycle - 20) % 40 == 0}
        // .map{case (cycle, x) => x * cycle}
        // .sum
        // part2
    (List((1,1)) ++ l)
        .map{
            case (cycle, x) =>
                val target = (cycle - 1) % 40
                if(target >= x - 1 && target <= x + 1) '#' else '.'
        }
        .grouped(40)
        .map(_.mkString)
        .mkString("\n")

}
