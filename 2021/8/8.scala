val input = scala.io.Source.fromFile("input.txt").mkString
    .split("\n")
    .toList





case class Line(key: List[Set[Char]], output: List[Set[Char]]) {
    def countEasyDigits: Int = {
        output.filter{
            o => Set(2, 4, 3, 7).contains(o.size)
        }.size
    }

    def keyBySize(s: Int) = key.filter(_.size == s)

    val k1 = keyBySize(2).head
    val k7 = keyBySize(3).head
    val k4 = keyBySize(4).head
    val k8 = keyBySize(7).head

    val k3 = keyBySize(5).find(k => (k & k1).size == 2).get
    val k5 = keyBySize(5).find(
        k =>
            k != k3
            && (k & k4).size == 3
    ).get
    val k2 = keyBySize(5).find(k => k != k3 && k != k5).get

    val k0 = keyBySize(6).find(k => (k & k5).size == 4).get
    val k6 = keyBySize(6).find(k => k != k0 && (k & k7).size == 2).get
    val k9 = keyBySize(6).find(k => k != k0 && (k & k7).size == 3).get

    def keyMap = Map(
        k0 -> 0,
        k1 -> 1,
        k2 -> 2,
        k3 -> 3,
        k4 -> 4,
        k5 -> 5,
        k6 -> 6,
        k7 -> 7,
        k8 -> 8,
        k9 -> 9,
    )

    def solve: Int = {
        output.map(o => keyMap(o).toString).mkString.toInt
    }
}

val inputLines = input.map(_.split(" \\| ").toList).map{
    case List(key, output) =>
        Line(key.split(" ").toList.map(_.toSet), output.split(" ").toList.map(_.toSet))
    case _ => ???
}


val part1 = inputLines.map(_.countEasyDigits).sum
println(part1)

val part2 = inputLines.map(_.solve).sum
println(part2)
