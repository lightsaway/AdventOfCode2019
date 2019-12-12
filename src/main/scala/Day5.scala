import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day5 extends IOApp {
  type Program = List[Int]

  def readParam(idx: Int, l: Program, mode: Int): Int = {
    if (mode == 1) idx
    else l(idx)
  }

  private def params(idx: Int, l: Program, modes: (Int, Int, Int)) =
    (l.slice(idx + 1, idx + 4) match {
      case List(a, b, o) => (a, b, o)
    })

  def runProgram(idx: Int, prog: Program, input: Int = 0, outputs: List[Int]): List[Int] = {
    val instruction: List[Int] =
      prog(idx).toString.toList.reverse.map(_.asDigit).padTo(5, 0).reverse
    val op: Int                = instruction.takeRight(2).mkString.toInt
    val modes: (Int, Int, Int) = (instruction(2), instruction(1), instruction(0))
    op match {
      case 1 =>
        val (a, b, o) = params(idx, prog, modes)
        val first     = readParam(a, prog, modes._1)
        val second    = readParam(b, prog, modes._2)
        runProgram(idx + 4, prog.updated(o, first + second), input, outputs)
      case 2 =>
        val (a, b, o) = params(idx, prog, modes)
        val first     = readParam(a, prog, modes._1)
        val second    = readParam(b, prog, modes._2)
        runProgram(idx + 4, prog.updated(o, first * second), input, outputs)
      case 3 =>
        val pos: Int = prog(idx + 1)
        runProgram(idx + 2, prog.updated(pos, input), input, outputs)
      case 4 =>
        val out = prog(idx + 1)
        runProgram(idx + 2, prog, input, outputs :+ prog(out))
      case 5 => ???
      case 6 => ???
      case 7 => ???
      case 8 => ???
      case 99 => outputs
      case x =>
        throw new IllegalArgumentException(
          s"unknown operation => instruction ${instruction} => op $x "
        )
    }
  }

  def toProgram(s: String): Program = s.split(",").toList.map(_.toInt)

  override def run(args: List[String]): IO[ExitCode] = {
    val data = Utils
      .readStrings("day5.txt")
      .compile
      .toList
      .map(_.head)

    val programm = data.map(toProgram)

    programm
      .map(p => runProgram(0, p, 1, List.empty[Int]))
      .flatTap(x => println(s"Part1: ${x.last}").pure[IO]) *>
      IO.unit.as(ExitCode.Success)
  }

}
