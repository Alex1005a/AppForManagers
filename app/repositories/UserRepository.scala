package repositories

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import models.Id.Id
import models.Name.Name
import models.{Manager, UnverifiedManager, User, VerifiedManager, Worker}

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

trait UserRepository[F[_]] {
  def create(user: User): F[Id]

  def getManagerByName(name: Name): OptionT[F, VerifiedManager]

  def getWorkerByName(name: Name): OptionT[F, Worker]

  def getUnverifiedManagerByToken(token: String): OptionT[F, UnverifiedManager]

  def deleteUnverifiedManager(manager: UnverifiedManager): F[Unit]

}

class UserInMemoryRepository extends UserRepository[IO] {
  private val listUnverifiedManager = Ref.of[IO, ListBuffer[UnverifiedManager]](ListBuffer.empty[UnverifiedManager])
  private val listVerifiedManager = Ref.of[IO, ListBuffer[VerifiedManager]](ListBuffer.empty[VerifiedManager])
  private val listWorker = Ref.of[IO, ListBuffer[Worker]](ListBuffer.empty[Worker])

  override def create(user: User): IO[Id] = {
    user match {
      case w: Worker =>
        for {
          ref <- listWorker
          _ <- ref.update(_ += w)
        } yield w.id
      case m: Manager =>
        m match {
          case u: UnverifiedManager =>
            for {
              ref <- listUnverifiedManager
              _ <- ref.update(_ += u)
            } yield u.id
          case v: VerifiedManager =>
            for {
              ref <- listVerifiedManager
              _ <- ref.update(_ += v)
            } yield v.id
        }
    }
  }

  override def getManagerByName(name: Name): OptionT[IO, VerifiedManager] = OptionT {
    for {
      ref <- listVerifiedManager
      result <- ref.get.map(_.find(_.name == name))
    } yield result
  }

  override def getWorkerByName(name: Name): OptionT[IO, Worker] = OptionT {
    for {
      ref <- listWorker
      result <- ref.get.map(_.find(_.name == name))
    } yield result
  }

  override def getUnverifiedManagerByToken(token: String): OptionT[IO, UnverifiedManager] = OptionT {
    for {
      ref <- listUnverifiedManager
      result <- ref.get.map(_.find(_.confirmationToken == token))
    } yield result
  }

  override def deleteUnverifiedManager(manager: UnverifiedManager): IO[Unit] = {
    for {
      ref <- listUnverifiedManager
      _ <- ref.update(_ -= manager)
    } yield ()
  }
}
