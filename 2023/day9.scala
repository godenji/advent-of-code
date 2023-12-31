// Day 9
//
@main def run() =
  val input = """0 3 6 9 12 15
    |1 3 6 10 15 21
    |10 13 16 21 30 45""".stripMargin

  def diffUntilZero(xs: List[Int]): List[List[Int]] =
    val res = xs.zip(xs.tail).map((a, b) =>
      b - a
    )
    if res.forall(_ == 0) then List(xs, res)
    else (List(res) ++ diffUntilZero(res)).distinct

  val items =
    input.split("\n").toList.map(line =>
      val xs = line.split(" ").toList.map(_.toInt)
      List(xs) ++ diffUntilZero(xs)
    )

  val nextValues = items.map(groups =>
    val zeroPadded =
      groups
        .map(_.reverse)
        .map(0 :: _)
        .map(_.reverse)
        .reverse

    zeroPadded.foldLeft(List.empty[List[Int]]) { case (acc, x) =>
      val v =
        x.reverse.tail.headOption.getOrElse(0) +
        acc.headOption.map(_.headOption.getOrElse(0)).getOrElse(0)
      List(v) :: acc
    }
  )

  val res = nextValues.flatMap(_.headOption).flatten.sum

  println(res) // 114
