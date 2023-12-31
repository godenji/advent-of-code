// Day 7
//
@main def run() =
  enum Card(val rank: Int):
    case A extends Card(13)
    case K extends Card(12)
    case Q extends Card(11)
    case J extends Card(10)
    case T extends Card(9)
    case `9` extends Card(8)
    case `8` extends Card(7)
    case `7` extends Card(6)
    case `6` extends Card(5)
    case `5` extends Card(4)
    case `4` extends Card(3)
    case `3` extends Card(2)
    case `2` extends Card(1)

  object Card:
    def apply(x: String) = x match
      case "A" => Card.A
      case "K" => Card.K
      case "Q" => Card.Q
      case "J" => Card.J
      case "T" => Card.T
      case "9" => Card.`9`
      case "8" => Card.`8`
      case "7" => Card.`7`
      case "6" => Card.`6`
      case "5" => Card.`5`
      case "4" => Card.`4`
      case "3" => Card.`3`
      case "2" => Card.`2`

  enum Kind(val rank: Int):
    case FIVE extends Kind(6)
    case FOUR extends Kind(5)
    case FULL_HOUSE extends Kind(4)
    case THREE extends Kind(3)
    case TWO_PAIR extends Kind(2)
    case ONE_PAIR extends Kind(1)
    case HIGH_CARD extends Kind(0)

  val input = """32T3K 765
    |T55J5 684
    |KK677 28
    |KTJJT 220
    |QQQJA 483""".stripMargin

  val items = input.split("\n").flatMap(_.trim.split(" ")).toList
  val pairs = items.zip(items.tail).filter(_._1.size == 5).map((a, b) => (a, b.toInt))

  val ranked = pairs.map((hand, bid) =>
    val groups = hand.split("").groupBy(identity)
    val rank =
      groups
        .map((card, group) =>
          import Kind.*
          group.size match
            case 1 => HIGH_CARD
            case 2 => ONE_PAIR
            case 3 => THREE
            case 4 => FOUR
            case 5 => FIVE
        )
        .foldLeft(0) { case (acc, x) =>
          acc + x.rank
        }
    (rank, hand, bid)
  ).sortBy(_._1)

  val sorted = ranked.groupBy(_._1).zipWithIndex.flatMap { case ((rank, xs), idx) =>
    if (xs.size == 1)
      val (_, hand, bid) = xs(0)
      List((idx + 1, hand, bid))
    else
      xs.zip(xs.tail).flatMap((a, b) =>
        val (_, handA, bidA) = a
        val (_, handB, bidB) = b
        val (charsA, charsB) = (handA.split(""), handB.split(""))
        def compare(index: Int): Boolean =
          val (rankA, rankB) = (Card(charsA(index)).rank, Card(charsB(index)).rank)
          if (rankA == rankB) compare(index + 1)
          else rankA < rankB
        if compare(0) then List((idx + 1, handA, bidA), (idx + 2, handB, bidB))
        else List((idx + 1, handB, bidB), (idx + 2, handA, bidA))
      )
   }

  val res = sorted
    .zipWithIndex
    .map { case ((_, _, bid), idx) => (idx + 1) * bid}
    .foldLeft(0) { case (acc, x) => acc + x }

  println(res) // 6440
