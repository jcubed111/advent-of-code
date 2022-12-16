val input = scala.io.Source.fromFile("input.txt").mkString

val matches = input
    .trim
    .split("\n")
    .map(_.split(" "));

def getThing(letter: String) = letter match {
    case "A" => "r"
    case "B" => "p"
    case "C" => "s"
    case "X" => "r"
    case "Y" => "p"
    case "Z" => "s"
    case _ => throw new Exception("error")
}

def getOutcome(letter: String) = letter match {
    case "X" => "l"
    case "Y" => "d"
    case "Z" => "w"
}

val partA = matches.map({
    case Array(elf, me) =>
        val myThing = (getThing(elf), getOutcome(me)) match {
            case (a: String, "d") => a
            case ("r", "w") => "p"
            case ("r", "l") => "s"
            case ("p", "w") => "s"
            case ("p", "l") => "r"
            case ("s", "w") => "r"
            case ("s", "l") => "p"
        }
        val signScore = myThing match {
            case "r" => 1
            case "p" => 2
            case "s" => 3
            case _ => throw new Exception("error")
        }
        val outcomeScore = (getThing(elf), myThing) match {
            case ("r", "r") => 3
            case ("p", "p") => 3
            case ("s", "s") => 3
            case ("r", "p") => 6
            case ("p", "s") => 6
            case ("s", "r") => 6
            case ("r", "s") => 0
            case ("p", "r") => 0
            case ("s", "p") => 0
            case _ => throw new Exception("error")
        }
        // (elf, me, signScore + outcomeScore)
        signScore + outcomeScore
    case _ => throw new Exception("error")
}).sum


println(partA)
