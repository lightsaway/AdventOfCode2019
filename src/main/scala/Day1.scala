import cats.effect.{ExitCode, IO, IOApp}
import fs2.text
import cats.implicits._

object Day1 extends IOApp {
  def massToFuel(mass: Int): Int = Math.floor(mass / 3).toInt - 2

  @scala.annotation.tailrec
  def fuel(mass: Int, acc: Int = 0): Int = massToFuel(mass) match {
    case m if m <= 0 => acc
    case m           => fuel(m, acc + m)
  }

  override def run(args: List[String]): IO[ExitCode] =
    Utils
      .readLines("./day1.txt")
      .through(_.map(_.toInt))
      .map(fuel(_))
      .fold(0)(_ + _)
      .evalTap(x => IO(println(x)))
      .compile
      .drain
      .as(ExitCode.Success)

}
