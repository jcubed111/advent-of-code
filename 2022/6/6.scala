val ans =
    scala.io.Source.fromFile("input.txt")
    .mkString
    .toList
    .sliding(14, 1)
    .indexWhere(_.toSet.size == 14) + 14;

println(ans)
