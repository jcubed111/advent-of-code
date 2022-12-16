import scala.annotation.tailrec


@tailrec
def tokenize(in: List[Char], out: List[Any] = List()): List[Any] = {
    in match {
        case '[' :: rest => tokenize(rest, out ++ List("["))
        case ']' :: rest => tokenize(rest, out ++ List("]"))
        case ',' :: rest => tokenize(rest, out)  // skip commas, we don't need them
        case ' ' :: rest => tokenize(rest, out)  // skip commas, we don't need them
        case Nil => out
        case _ =>
            val numChars = in.takeWhile(_.isDigit).toList
            val num = numChars.mkString.toInt
            tokenize(in.drop(numChars.size), out ++ List(num))
    }
}

case class ElfList(items: List[Any]) extends Ordered[ElfList] {
    override def toString = "[" + items.mkString(",") + "]"

    override def compare(b: ElfList): Int = {
        items.zip(b.items).view.map{
            case (i: ElfList, j: ElfList) => i compare j
            case (i: ElfList, j: Int) => i compare ElfList(List(j))
            case (i: Int, j: ElfList) => ElfList(List(i)) compare j
            case (i: Int, j: Int) => i - j
        }
        .filter(_ != 0)
        .headOption
        .getOrElse(items.size - b.items.size)
    }
}

def parseList(
    tokens: List[Any]
): (List[Any], ElfList) = {
    val (remaining, list) = parseItems(tokens.drop(1), Nil)  // eat a '['
    (
        remaining,
        ElfList(list)
    )
}

@tailrec
def parseItems(
    tokens: List[Any],
    out: List[Any] = Nil
): (List[Any], List[Any]) = {
    tokens match {
        case "[" :: rest =>
            val (remaining, headItem) = parseList(tokens)
            parseItems(remaining, out ++ List(headItem))
        case "]" :: rest =>
            (rest, out)
        case (x: Int) :: rest =>
            parseItems(rest, out ++ List(x))
        case _ :: rest =>
            (rest, out)
        case Nil =>
            (Nil, out)
    }
}

def parse(tokens: List[Any]): ElfList = parseList(tokens)._2


val ordered = scala.io.Source.fromFile("input.txt").mkString
    .split("\n")
    .filter(_ != "")
    .toList
    .++(List("[[2]]", "[[6]]"))
    .map(x => parse(tokenize(x.toList)))
    .sorted
    .map(_.toString)

val a = ordered.indexOf("[[2]]") + 1
val b = ordered.indexOf("[[6]]") + 1

println(a * b)
