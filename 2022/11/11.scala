object Numbers{
    val numR = raw"([\d.]+)".r
    def unapplySeq(in: String) =
        numR.findAllIn(in).toList.map(_.toInt) match {
            case Nil => None
            case a => Some(a)
        }
}

object Lines{
    def unapplySeq(in: String) =
        Some(in.split("\n").toList)
}

object Words{
    val numR = raw"(\S+)".r
    def unapplySeq(in: String) =
        numR.findAllIn(in.trim).toList match {
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

/****************************************************/

case class Monkey(
    val items: List[BigInt],
    val op: BigInt => BigInt,
    val divisor: BigInt,
    trueMonkey: Int,
    falseMonkey: Int,
) {
    def getNextMonkey(item: BigInt): Int =
        if(item % divisor == 0) {
            trueMonkey
        }else{
            falseMonkey
        }
}

val monkeys = scala.io.Source.fromFile("input.txt").mkString
    .trim
    .split("Monkey ")
    .toList
    .drop(1)
    .map{
        case Lines(
            _, // ignore monkey number
            Numbers(startingItems @ _*),
            Words("Operation:", "new", "=", "old", op, b),
            Words("Test:", "divisible", "by", Int(n)),
            Numbers(trueMonkey),
            Numbers(falseMonkey),
        ) =>
            Monkey(
                startingItems.toList.map(BigInt(_)),
                (op, b) match {
                    case ("+", "old") => ((n: BigInt) => n + n)
                    case ("*", "old") => ((n: BigInt) => n * n)
                    case ("+", Int(bi)) => ((n: BigInt) => n + BigInt(bi))
                    case ("*", Int(bi)) => ((n: BigInt) => n * BigInt(bi))
                },
                n,
                trueMonkey,
                falseMonkey,
            )
    }


println{
    val monkeyMod = monkeys.map(_.divisor).reduce(_ * _)

    val initialItems = monkeys.zipWithIndex.map{
        case (m, i) => i -> m.items
    }.toMap
    val initialInspections = monkeys.map(_ => BigInt(0))

    type Items = Map[Int, List[BigInt]]
    type Inspections = List[BigInt]

    def runMonkey(m: Monkey, items: List[BigInt]): Items = {
        items
            .map{
                case i: BigInt =>
                    val nextI: BigInt = m.op(i) % monkeyMod
                    m.getNextMonkey(nextI) -> nextI
            }
            .groupBy(_._1)
            .view.mapValues(_.map(_._2))
            .toMap
    }

    def runRound(items: Items, inspections: Inspections): (Items, Inspections) = {
        monkeys.zipWithIndex.foldLeft((items, inspections)) {
            case ((items, inspections: Inspections), (monkey, i)) =>
                val monkeyItems = items(i)
                val monkeyOut = runMonkey(monkey, monkeyItems)
                val mergedMap: Items = {
                    (items.toList ++ monkeyOut.toList)
                        .groupMapReduce(_._1)(_._2)(_ ++ _)
                }
                (
                    mergedMap ++ Map(i -> Nil),
                    inspections.updated(i, inspections(i) + BigInt(monkeyItems.size))
                )
        }
    }

    val end = Range(0, 10_000).foldLeft((initialItems, initialInspections)) {
        case ((items, inspections), _) =>
            runRound(items, inspections)
    }

    end._2.sorted.takeRight(2).reduce(_ * _)
}
