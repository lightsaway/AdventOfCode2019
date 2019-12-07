import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import fs2.{text, Stream}

object Day2 extends IOApp {
  type Program = List[Int]

  private def read(idx: Int, l: Program) = l.slice(idx + 1, idx + 4) match {
    case List(a, b, o) => (a, b, o)
  }

  def runProgram(idx: Int, prog: Program): Program =
    prog(idx) match {
      case 1 =>
        val (a, b, o) = read(idx, prog)
        runProgram(idx + 4, prog.updated(o, prog(a) + prog(b)))
      case 2 =>
        val (a, b, o) = read(idx, prog)
        runProgram(idx + 4, prog.updated(o, prog(a) * prog(b)))
      case 99 => prog
      case _  => throw new IllegalArgumentException("unknown operation")
    }

  def restore(p: Program): Program = p.updated(1, 12).updated(2, 2)

  def toProgram(s: String): Program = s.split(",").toList.map(_.toInt)

  override def run(args: List[String]): IO[ExitCode] = {
    val records = Utils
      .readStrings("./day2.txt")
      .compile
      .toList

    records
      .map { strings =>
        strings
          .map(toProgram)
          .map(restore)
          .map(p => runProgram(0, p))
      }
      .map(progs => progs.map(_.head).map(println))
      .as(ExitCode.Success)
  }

}
