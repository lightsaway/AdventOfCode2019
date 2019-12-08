import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import scala.util.chaining._

object Day4 extends IOApp {

  @scala.annotation.tailrec
  def isSorted(l: List[Int]): Boolean = l match {
    case h :: m :: t =>
      if (h > m) false
      else isSorted(m +: t)
    case Nil | _ :: Nil => true
  }

  @scala.annotation.tailrec
  def hasTwinAdjacent(l: List[Int]): Boolean = l match {
    case h :: m :: t =>
      if (h == m) true
      else hasTwinAdjacent(m +: t)
    case Nil | _ :: Nil => false
  }

  def hasExactly2SameAdjacent(l: List[Int]): Boolean =
    l.pipe(x => x.sliding(2).exists { case List(a, b) => a == b && x.count(_ == a) == 2 })

  def part1(input: List[List[Int]]): Int = input.count(x => isSorted(x) && hasTwinAdjacent(x))

  def part2(input: List[List[Int]]): Int =
    input.count(x => isSorted(x) && hasExactly2SameAdjacent(x))

  def intToDigits(i: Int): List[Int] = i.toString.toList.map(_.asDigit)

  def convertInput(s: String) =
    s.split("-")
      .toList
      .map(_.toInt)
      .pipe { case List(from, to) => from.to(to).toList }
      .map(intToDigits)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      lines <- Utils.readLines("day4.txt").compile.toList
      input <- lines.head.pipe(convertInput).pure[IO]
      _ = part1(input).pipe(a => println(s"Part1 : $a"))
      _ = part2(input).pipe(a => println(s"Part2 : $a"))
    } yield ExitCode.Success
}
