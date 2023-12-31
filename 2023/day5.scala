// Day 5
//
@main def run() =

  val input = """seed-to-soil map:
    |50 98 2
    |52 50 48

    |soil-to-fertilizer map:
    |0 15 37
    |37 52 2
    |39 0 15

    |fertilizer-to-water map:
    |49 53 8
    |0 11 42
    |42 0 7
    |57 7 4

    |water-to-light map:
    |88 18 7
    |18 25 70

    |light-to-temperature map:
    |45 77 23
    |81 45 19
    |68 64 13

    |temperature-to-humidity map:
    |0 69 1
    |1 0 69

    |humidity-to-location map:
    |60 56 37
    |56 93 4""".stripMargin

  val sources = "seeds: 79 14 55 13".split(": ")(1).split(" ").toList.map(_.toInt)
  val hasDigits = "\\d+".r.findAllIn _
  val initTargets = List.empty[List[(Int, Int)]]

  def reduce(candidates: List[List[(Int, Int)]]) =
    val xs = candidates.map(_.zipWithIndex)
    val partitioned = (xs.map(_.partition{ case ((source, slot), idx) => slot != -1}))
    val (matched, unmatched) = (
      partitioned.flatMap((a, b) => a),
      partitioned.flatMap((a, b) => b).distinct
    )
    val omits = for
      ((v, x), midx) <- matched
      ((v, x), uidx) <- unmatched if uidx == midx
    yield ((v, x), uidx)
    val sources: List[Int] = (unmatched.diff(omits) ++ matched).sortBy(_._2).reverse.foldLeft(List.empty) { case (acc, ((v, _), _)) => v :: acc }
    sources

  val mapResult = input.split("\n").toList.filterNot(_ == "").foldLeft((sources, initTargets)) {
    case ((sources, targets), line) =>
      if (line.contains(":"))
        if targets.isEmpty then (sources, targets)
        else (reduce(targets), initTargets)
      else
        hasDigits(line).toList: @unchecked match
          case dest :: source :: range :: Nil =>
            val (d, s, r) = (dest.toInt, source.toInt, range.toInt)
            val pairs = for (i <- 0 to r -1) yield (d + i, s + i)
            val targets2 = sources.zipWithIndex.map((source, slot) =>
              pairs.find(_._2 == source).map(x => (x._1, slot)).getOrElse((source, -1))
            )
            (sources, targets ++ List(targets2))
  }

  val finalSources = reduce(mapResult._2)

  println(finalSources.min) // 35
