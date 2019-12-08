import cats.effect.{ExitCode, IO, IOApp}
import Utils._
import cats.implicits._


object Day3 extends IOApp{
  case class Move(direction: Char, steps: Int)
  def manhattanDistance(p1: Point, p2: Point): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def intersection(w1 : List[Point], w2 : List[Point]) = w1.intersect(w2)

  object Move{
    def apply(s: String): Move = Move(s.head, s.tail.toInt)
  }
  case class Point(x: Int = 0, y: Int = 0)

  object Point{
    val center = Point()
  }

  def assembleWire(m: List[Move]): List[Point] =  m.foldLeft(List(Point()))((ps, move) =>{
    val moves = 0.to(move.steps).toList
    val currPos = ps.last
    val newPoints = move.direction match {
      case 'U' => moves.map(mv => Point(currPos.x, currPos.y + mv))
      case 'D' => moves.map(mv => Point(currPos.x, currPos.y - mv))
      case 'L' => moves.map(mv => Point(currPos.x - mv, currPos.y))
      case 'R' => moves.map(mv => Point(currPos.x + mv, currPos.y))
    }
    ps ++ newPoints
  })


  override def run(args: List[String]): IO[ExitCode] = {
    val inputs = readLines("day3.txt")
      .map(_.split(",").toList.map(Move.apply))
      .map(assembleWire)
      .compile.toList

    val testData: IO[List[List[Point]]] = readLines("day3.test.txt")
      .map(_.split(",").toList.map(Move.apply))
      .map(assembleWire)
      .compile.toList

    testData.map{ ws =>
      val intersections = ws.head.intersect(ws.last).toSet.excl(Point.center)
      val minDistance = intersections.map(manhattanDistance(Point.center,_)).min
      assert( minDistance == 159 )
    } *>
      inputs.map{ ws =>
        val intersections = ws.head.intersect(ws.last).toSet.excl(Point.center)
        val minDistance = intersections.map(manhattanDistance(Point.center,_)).min
        println(minDistance)
      } *> IO.unit.as(ExitCode.Success)
  }

}
