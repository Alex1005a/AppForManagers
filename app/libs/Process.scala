package libs
import cats.effect.{Fiber, IO, ContextShift}
import scala.concurrent.ExecutionContext

trait Process[F[_]] {

  def fork[T](eff: F[T]): F[Task[F, T]]

  def await[T](task: Task[F, T]): F[T]

}

trait Task[F[_], T] {
    def getResult(): F[T]
}

class TaskIO[T](val fiber: Fiber[IO, T])(implicit ec: ContextShift[IO]) extends Task[IO, T] {
  override def getResult(): IO[T] =  {
   fiber.join
  } 
}

class ProcessIO(implicit ec: ContextShift[IO]) extends Process[IO] {
  override def fork[T](io: IO[T]): IO[TaskIO[T]] = for {
    fiber <- io.start
  } yield new TaskIO(fiber)

  override def await[T](task: Task[IO, T]): IO[T] = {
    task.getResult()
  }
}