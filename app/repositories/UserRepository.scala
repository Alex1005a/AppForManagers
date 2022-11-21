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

  def getManagerById(id: Id): OptionT[F, VerifiedManager]

  def getWorkerByName(name: Name): OptionT[F, Worker]

  def getWorkerById(id: Id): OptionT[F, Worker]

  def updateManager(manager: VerifiedManager): F[Boolean]

  def updateWorker(worker: Worker): F[Boolean]

  def getUnverifiedManagerByToken(token: String): OptionT[F, UnverifiedManager]

  def deleteUnverifiedManager(manager: UnverifiedManager): F[Unit]

}

object UserRepository {
  def apply[F[_]](implicit F: UserRepository[F]): UserRepository[F] = F
}

class UserInMemoryRepository extends UserRepository[IO] {

  private def update[T <: User](user: T, listUserIO: IO[Ref[IO, ListBuffer[T]]]): IO[Boolean] = for {
    ref <- listUserIO
    existResult <- ref.get.map(_.exists(_.id == user.id))
    result <- if(!existResult) IO(false)
    else ref.get.map(_.filterNot(_.id == user.id)).flatMap(list => ref.update(_ => list += user)).map(_ => true)
  } yield result

  private def getBy[T <: User](listUserIO: IO[Ref[IO, ListBuffer[T]]], predicate: T => Boolean): OptionT[IO, T] = OptionT {
    for {
      ref <- listUserIO
      result <- ref.get.map(_.find(predicate))
    } yield result
  }
  def createUserIOList[User]: IO[Ref[IO, ListBuffer[User]]] = Ref.of[IO, ListBuffer[User]](ListBuffer.empty[User])

  private val listUnverifiedManager = createUserIOList[UnverifiedManager]
  private val listVerifiedManager = createUserIOList[VerifiedManager]
  private val listWorker = createUserIOList[Worker]

  override def create(user: User): IO[Id] = {
    user match {
      case worker: Worker =>
        for {
          ref <- listWorker
          _ <- ref.update(_ += worker)
        } yield worker.id
      case manager: Manager =>
        manager match {
          case unverifiedManager: UnverifiedManager =>
            for {
              ref <- listUnverifiedManager
              _ <- ref.update(_ += unverifiedManager)
            } yield unverifiedManager.id
          case verifiedManager: VerifiedManager =>
            for {
              ref <- listVerifiedManager
              _ <- ref.update(_ += verifiedManager)
            } yield verifiedManager.id
        }
    }
  }

  override def getManagerByName(name: Name): OptionT[IO, VerifiedManager] = getBy(listVerifiedManager, _.name == name)

  override def getManagerById(id: Id): OptionT[IO, VerifiedManager] = getBy(listVerifiedManager, _.id == id)

  override def getWorkerByName(name: Name): OptionT[IO, Worker] = getBy(listWorker, _.name == name)

  override def getWorkerById(id: Id): OptionT[IO, Worker] = getBy(listWorker, _.id == id)

  override def getUnverifiedManagerByToken(token: String): OptionT[IO, UnverifiedManager] = getBy(listUnverifiedManager, _.confirmationToken == token)

  override def deleteUnverifiedManager(manager: UnverifiedManager): IO[Unit] = {
    for {
      ref <- listUnverifiedManager
      _ <- ref.update(_ -= manager)
    } yield ()
  }

  override def updateManager(manager: VerifiedManager): IO[Boolean] = update(manager, listVerifiedManager)

  override def updateWorker(worker: Worker): IO[Boolean] = update(worker, listWorker)

}
