println{
    List(-1, 5, 6, 7).sliding(2, 1)
                .takeWhile{
                    case List(a, b) => a < b
                    case _ => false
                }
                .size
}
