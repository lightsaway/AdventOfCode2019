import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

object Day2 extends IOApp {
  type Program = List[Int]

  private def read(idx: Int, l: Program) = l.slice(idx + 1, idx + 4) match {
    case List(a, b, o) => (a, b, o)
  }

  def runProgram(idx: Int, prog: Program): Int =
    prog(idx) match {
      case 1 =>
        val (a, b, o) = read(idx, prog)
        runProgram(idx + 4, prog.updated(o, prog(a) + prog(b)))
      case 2 =>
        val (a, b, o) = read(idx, prog)
        runProgram(idx + 4, prog.updated(o, prog(a) * prog(b)))
      case 99 => prog.head
      case _  => throw new IllegalArgumentException("unknown operation")
    }

  def restore(p: Program): Program = p.updated(1, 12).updated(2, 2)

  def gravityAssist(p: Program): Int =
    fs2.Stream
      .emits(0.until(99))
      .flatMap { noun =>
        fs2.Stream.emits(0.until(99)).map { verb =>
          {
            val memory = p.updated(1, noun).updated(2, verb)
            val out    = runProgram(0, memory)
            println(noun, verb, out)
            (noun, verb, out)
          }
        }
      }
      .dropWhile { case (_, _, o) => o != 19690720 }
      .take(1)
      .map { case (n, v, _) => 100 * n + v }
      .compile
      .toList
      .head


  def toProgram(s: String): Program = s.split(",").toList.map(_.toInt)

  override def run(args: List[String]): IO[ExitCode] = {
    val data = Utils
      .readStrings("./day2.txt")
      .compile
      .toList
      .map(_.head)

    val programm = data.map(toProgram)

    programm
      .map(restore)
      .map(p => runProgram(0, p))
      .map(println) *>
      programm
        .map(gravityAssist)
        .map(println) *>
      IO.unit.as(ExitCode.Success)
  }

}
