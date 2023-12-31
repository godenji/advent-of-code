// Day 1
//
@main def run() =
  val input = """1abc2
  |pqr3stu8vwx
  |a1b2c3d4e5f
  |treb7uchet""".stripMargin

  val digits = "\\d".r.findAllIn _

  val res =
    input.split("\n").map(line =>
      val xs = digits(line).toList
      (xs.headOption ++ xs.reverse.headOption).mkString.toInt
    ).sum

  println(res) // 142
