// Day 6
//
@main def run() =
  val input = """Time:      7  15   30
    |Distance:  9  40  200""".stripMargin

  val items =
    input.split("\n")
      .map(_.split(":"))
      .map(_.tail)
      .flatMap(_.map(_.trim))
      .map(_.split(" ")
      .filterNot(_ == ""))
      .map(_.toList.map(_.toInt))
      .toList

  val winners =
    items: @unchecked match
      case a :: b :: Nil =>
        val pairs = a.zip(b)
        pairs.map((t, d) =>
          for i <- 1 to t if i * (t -i) > d
          yield i * (t -i)
        )

  val res =
    winners.map(_.size).foldLeft(1) {
      (acc, i) => i * acc
    }

  println(res) // 288
