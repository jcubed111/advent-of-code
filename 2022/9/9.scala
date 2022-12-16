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

    scala.io.Source.fromFile("example.txt").mkString
        .split("\n")
        .toList
        .flatMap{
            case Words(dir, Int(amount)) =>
                val move = dir match {
                    case "R" => (1, 0)
                    case "L" => (-1, 0)
                    case "D" => (0, -1)
                    case "U" => (0, 1)
                }
                Range(0, amount).map(_ => move).toList
        }
        .foldLeft(Set((0, 0)), (0, 0), (0, 0)) {
            case ((out, (hx, hy), (tx, ty)), (moveX, moveY)) =>
                val (nhx, nhy) = (hx + moveX, hy + moveY)
                val diff = (nhx - tx, nhy - ty)
                val newTail = diff match {
                    case (a, b) if a.abs < 2 && b.abs < 2 => (tx, ty)
                    case (2, 0) => (tx + 1, ty)
                    case (-2, 0) => (tx - 1, ty)
                    case (0, 2) => (tx, ty + 1)
                    case (0, -2) => (tx, ty - 1)
                    case (a, b) => (tx + a.sign, ty + b.sign)
                }
                (
                    Set(newTail) ++ out,
                    (nhx, nhy),
                    newTail
                )
        }
        ._1
        .size


}
