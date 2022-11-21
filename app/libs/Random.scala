package libs

import cats.effect.{ContextShift, IO}

trait Random[F[_]] {
  def randomByteArray: F[Array[Byte]]
}

class RandomIO(implicit ec: ContextShift[IO]) extends Random[IO] {
  def randomByteArray(): IO[Array[Byte]] = {
    IO.pure(Array.fill(20)((scala.util.Random.nextInt(256) - 128).toByte))
  }
}
