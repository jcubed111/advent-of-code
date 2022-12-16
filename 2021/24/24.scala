import scala.collection.immutable.NumericRange

val input = scala.io.Source.fromFile("example.txt").mkString
    .split("\n")
    .toList


val instructions = input.map{_.split(" ") match {
    case Array(a, b) => (a, b)
    case Array(a, b, c) => (a, b, c)
}}

case class Registers(r: Map[String, Int], input: List[Int])

def applyInstruction(registers: Registers, instruction: Any): Registers = {
    instruction match {
        case ("inp", a: String) =>
            registers.input match {
                case head :: rest =>
                    Registers(
                        registers.r + (a -> head),
                        rest
                    )
                case Nil => ???
            }
        case (op, a: String, b: String) =>
            val aVal = registers.r.getOrElse(a, 0)
            val bVal = registers.r.getOrElse(b, b.toInt)
            val result = op match {
                case "add" => aVal + bVal
                case "mul" => aVal * bVal
                case "div" => aVal / bVal
                case "mod" => aVal % bVal
                case "eql" => if(a == b) 1 else 0
            }
            Registers(
                registers.r + (a -> result),
                registers.input
            )
    }
}

def runProg(input: Long): Map[String, Int] = {
    val r = Registers(
        Map("w" -> 0, "x" -> 0, "y" -> 0, "z" -> 0),
        input.toString.toList.map(_.toInt),
    )
    instructions.foldLeft(r)(applyInstruction _).r
}

println(runProg(7))

// def passes(input: Long): Boolean = {
//     if(input.toString.contains("0"))
//         false
//     else
//         (runProg(input)("z") == 0)
// }


// def stream(i: Long): LazyList[Long] = i #:: stream(i - 1L)


// val part1 = stream(99999999999999L).find(passes _)
// println(part1)
