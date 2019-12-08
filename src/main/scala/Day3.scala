import Utils._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day3 extends IOApp {
  case class Move(direction: Char, steps: Int)
  object Move {
    def apply(s: String): Move = Move(s.head, s.tail.toInt)
  }

  case class Point(x: Int = 0, y: Int = 0)
  object Point {
    val center = Point()
  }

  case class Distance(distance: Int, steps: Int)

  def manhattanDistance(p1: Point, p2: Point): Int = Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def assembleWire(m: List[Move]): List[Point] =
    m.foldLeft(List(Point()))((ps, move) => {
      val moves   = 1.to(move.steps).toList
      val currPos = ps.last
      val newPoints = move.direction match {
        case 'U' => moves.map(mv => Point(currPos.x, currPos.y + mv))
        case 'D' => moves.map(mv => Point(currPos.x, currPos.y - mv))
        case 'L' => moves.map(mv => Point(currPos.x - mv, currPos.y))
        case 'R' => moves.map(mv => Point(currPos.x + mv, currPos.y))
      }
      ps ++ newPoints
    })

  def findMins(w1: List[Point], w2: List[Point]) = {
    val intersections = w1
      .intersect(w2)
      .toSet
      .excl(Point.center)

    val minDistance = intersections.map(manhattanDistance(Point.center, _)).min
    val minSteps = intersections.map { i =>
      {
        w1.takeWhile(p => p != i).size + w2.takeWhile(p => p != i).size
      }
    }.min

    Distance(minDistance, minSteps)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val inputs = readLines("day3.txt")
      .map(_.split(",").toList.map(Move.apply))
      .map(assembleWire)
      .compile
      .toList

    val testData: IO[List[List[Point]]] = readLines("day3.test.txt")
      .map(_.split(",").toList.map(Move.apply))
      .map(assembleWire)
      .compile
      .toList

    testData.map { ws =>
      val r = findMins(ws.head, ws.last)
      assert(r.distance == 159)
      assert(r.steps == 610)
    } *>
      inputs.map { ws =>
        println(findMins(ws.head, ws.last))
      } *> IO.unit.as(ExitCode.Success)
  }

}
