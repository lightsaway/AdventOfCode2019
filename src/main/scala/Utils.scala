import java.nio.file.Paths

import cats.effect.{Blocker, ContextShift, Sync}
import fs2.{Stream, io}

object Utils {
  def readFile[F[_]](f: String)(implicit CS: ContextShift[F], S: Sync[F]): Stream[F, Byte] = Stream.resource(Blocker[F]).flatMap { blocker =>
    io.file.readAll[F]( Paths.get(getClass.getResource(f).getPath), blocker, 4096)
  }
}
