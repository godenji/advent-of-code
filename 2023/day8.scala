// Day 8
//
@main def run() =
  val input = """LLR

    |AAA = (BBB, BBB)
    |BBB = (AAA, ZZZ)
    |ZZZ = (ZZZ, ZZZ)""".stripMargin

  val lines = input.split("\n").filterNot(_ == "").toList
  val (directions, nodes) = (lines.head.split("").toList, lines.tail)
  val maps = nodes.map(n =>
    val node :: paths :: Nil = n.split(" = ").toList: @unchecked
    val left :: right :: Nil =
      paths.split(", ").toList.map(_.replaceAll("\\(", "").replaceAll("\\)", "")): @unchecked
    (node, (left, right))
  )

  val (startNode, endNode) = (
    maps.headOption.map(_._1).getOrElse(""),
    maps.reverse.headOption.map(_._1).getOrElse("")
  )

  def navigate(node: String, direction: String): Option[String] =
    maps
      .find(_._1 == node)
      .map(_._2)
      .map((left, right) =>
        if direction == "L" then left else right
      )

  def recurse(startNode: String): List[String] =
    val nodes = directions.foldLeft(List.empty[String]) { case (acc, d) =>
      if acc.isEmpty then navigate(startNode, d).toList
      else acc ++ acc.reverse.headOption.flatMap(navigate(_, d)).toList
    }
    if nodes.exists(_ == endNode) then nodes
    else nodes ++ nodes.reverse.headOption.flatMap(recurse)

  val res = recurse(startNode)

  println(res.size) // 6
