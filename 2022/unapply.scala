// case class A(){
//     def unapply(s: String): Option[(Int, Int)] = {
//         s.split(divider) match {
//             case Array(a, b) => Some((a.toInt, b.toInt))
//             case _ => None
//         }
//     }
// }


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

object AsInt{
    def unapply(in: String) = scala.util.Try(in.toInt).toOption
}


println{
    "hello 1. 2" match {
        case Words(a, Chars(_, '.'), AsInt(c)) => (a, c + 5)
    }
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


println{
    List(1, 2, 3).mapLeftAcc(0) {
        case (acc, next) =>
            (acc + 1, next * acc)
    }
}
