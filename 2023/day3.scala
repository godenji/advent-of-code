// Day 3
//
@main def run() =
  val input = """467..114..
    |...*......
    |..35..633.
    |......#...
    |617*......
    |.....+.58.
    |..592.....
    |......755.
    |...$.*....
    |.664.598..""".stripMargin

  val symPattern = "[*#+$]".r.findFirstIn _
  val numPattern = "[0-9]+".r

  val lines = input.split("\n").toList

  def parse(maybePrev: Option[String], curr: String, maybeNext: Option[String]) =
    numPattern.findAllIn(curr).flatMap(n =>
      val start = curr.indexOf(n)
      val len = start + n.length
      val prevChars = maybePrev.map(_.toList).getOrElse(Nil)
      val currChars = curr.toList
      val nextChars = maybeNext.map(_.toList).getOrElse(Nil)

      val a = for (x <- start -1 to len if prevChars.isDefinedAt(x) && symPattern(prevChars(x).toString).isDefined) yield n.toInt
      val b = for (x <- start -1 to len if currChars.isDefinedAt(x) && symPattern(currChars(x).toString).isDefined) yield n.toInt
      val c = for (x <- start -1 to len if nextChars.isDefinedAt(x) && symPattern(nextChars(x).toString).isDefined) yield n.toInt
      a ++ b ++ c
    )

  val res = lines.zipWithIndex.flatMap((line, idx) =>
    val (maybePrev, maybeNext) = (
      if lines.isDefinedAt(idx -1) then Some(lines(idx -1)) else None,
      if lines.isDefinedAt(idx +1) then Some(lines(idx +1)) else None
    )
    parse(maybePrev, lines(idx), maybeNext)
  ).sum

  println(res) // 4361
