val input = scala.io.Source.fromFile("input.txt").mkString

val List(keyString, mapString) = input
    .trim
    .split("\n\n")
    .toList

case class Image(data: Map[(Int, Int), Int], default: Int) {
    val startPx = data.keys.flatMap({case (x, y) => List(x, y)}).min - 1
    val endPx = data.keys.flatMap({case (x, y) => List(x, y)}).max + 2

    def get(x: Int, y: Int): Int = data.getOrElse((x, y), default)
    def sum = data.values.sum

    def map[T](cb: (Int, Int) => T): Iterable[T] =
        Range(startPx, endPx).flatMap(
            x => Range(startPx, endPx).map(
                y => cb(x, y)
            )
        )

    def print(): Unit =
        Range(startPx, endPx).foreach(y => {
            val row = Range(startPx, endPx)
                .map(x => get(x, y))
                .map({
                    case 1 => '#'
                    case 0 => '.'
                })
                .mkString
            println(row)
        })
}

val key = keyString
    .toList
    .map{
        case '#' => 1
        case '.' => 0
    };
val startMap: Image = Image(
        mapString
        .split("\n")
        .toList
        .zipWithIndex
        .flatMap{
            case (row, y) =>
                row.toList.zipWithIndex.map({
                    case (cell: Char, x: Int) =>
                        (x, y) -> (if(cell == '#') 1 else 0)
                })
        }
        .toMap,
        0
    )


def step(input: Image): Image = {
    val outputData = input.map{
        case (x, y) =>
            val index = (
                input.get(x - 1, y - 1) * 256
                + input.get(x    , y - 1) * 128
                + input.get(x + 1, y - 1) * 64
                + input.get(x - 1, y    ) * 32
                + input.get(x    , y    ) * 16
                + input.get(x + 1, y    ) * 8
                + input.get(x - 1, y + 1) * 4
                + input.get(x    , y + 1) * 2
                + input.get(x + 1, y + 1) * 1
            )
            (x, y) -> key(index)
    }.toMap

    val outputDefault = key(input.default * 511)

    Image(outputData, outputDefault)
}

def rStep(input: Image, times: Int): Image = times match {
    case 0 => input
    case x => rStep(step(input), times - 1)
}

// startMap.print()
// step(startMap).print()
// step(step(startMap)).print()

val part1 = step(step(startMap)).sum
println(part1)

println(rStep(startMap, 50).sum)
