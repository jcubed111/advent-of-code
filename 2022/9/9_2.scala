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


case class Rope(positions: List[(Int, Int)]) {
    def move(dx: Int, dy: Int): Rope = {
        val (hx, hy) = positions.head
        var newOut = List((hx + dx, hy + dy))

        positions.drop(1).foreach{
            case (tx, ty) =>
                val (nhx, nhy) = newOut.last
                val diff = (nhx - tx, nhy - ty)
                val newTail = diff match {
                    case (a, b) if a.abs < 2 && b.abs < 2 => (tx, ty)
                    case (2, 0) => (tx + 1, ty)
                    case (-2, 0) => (tx - 1, ty)
                    case (0, 2) => (tx, ty + 1)
                    case (0, -2) => (tx, ty - 1)
                    case (a, b) => (tx + a.sign, ty + b.sign)
                }
                newOut = newOut ++ List(newTail)
        }

        Rope(newOut)
    }

    def end = positions.last
}

println{

    scala.io.Source.fromFile("input.txt").mkString
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
        .mapLeftAcc(
            Rope( Range(0, 10).map(_ => (0, 0)).toList )
        ) {
            case (rope, (moveX, moveY)) =>
                val newRope = rope.move(moveX, moveY)
                (newRope, newRope.end)
        }
        .toSet
        .size
}


// 2630
