// Day 2
//
@main def run() =
  val input = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green""".stripMargin

  val gamePattern = "Game ([0-9]+)".r
  val cubePattern = "([0-9]+) (blue|red|green)".r

  val res =
    input.split("\n").flatMap(line =>
      line.split(":").toList: @unchecked match
        case game :: items :: Nil =>
          val id = game match
            case gamePattern(id) => id.toInt
            case _ => 0
          val elems = items.split(";").flatMap(cubes =>
            cubes.split(", ").map(_.trim).map(cube =>
              cube match
                case cubePattern(number, color) =>
                  val num = number.toInt
                  color match
                    case "red" if num <= 12 => Some(id)
                    case "green" if num <= 13 => Some(id)
                    case "blue" if num <= 14 => Some(id)
                    case _ => None
                case x => None
            )
          ).toList
          if elems.forall(_.isDefined) then Some(id) else None
    ).sum

  println(res) // 8
