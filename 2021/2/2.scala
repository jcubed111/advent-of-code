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

    val end = input.foldLeft((0, 0, 0)) {
        case (x, y, aim) -> Words("forward", Int(n)) => (x + n, y + n * aim, aim)
        case (x, y, aim) -> Words("down", Int(n)) => (x, y, aim + n)
        case (x, y, aim) -> Words("up", Int(n)) => (x, y, aim - n)
    }

    end._1 * end._2
}
