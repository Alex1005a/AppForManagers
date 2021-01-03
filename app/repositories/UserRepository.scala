package repositories

import cats.data.OptionT
import cats.effect.IO
import models.Id.Id
import models.Name.Name
import models.{Manager, UnverifiedManager, User, VerifiedManager, Worker}

import scala.collection.mutable.ListBuffer

trait UserRepository {
  def create(user: User): IO[Id]

  def getManagerByName(name: Name): OptionT[IO, VerifiedManager]

  def getWorkerByName(name: Name): OptionT[IO, Worker]

  def getUnverifiedManagerByToken(token: String): OptionT[IO, UnverifiedManager]

  def deleteUnverifiedManager(manager: UnverifiedManager): IO[Unit]
}

class UserInMemoryRepository extends UserRepository {
  private val listUnverifiedManager = ListBuffer.empty[UnverifiedManager]
  private val listVerifiedManager = ListBuffer.empty[VerifiedManager]
  private val listWorker = ListBuffer.empty[Worker]

  override def create(user: User): IO[Id] = IO {
    user match {
      case w: Worker =>
        listWorker += w
        w.id
      case m: Manager =>
        m match {
          case u: UnverifiedManager =>
            listUnverifiedManager += u
            u.id
          case v: VerifiedManager =>
            listVerifiedManager += v
            v.id
        }
    }
  }

  override def getManagerByName(name: Name): OptionT[IO, VerifiedManager] = OptionT {
    IO(listVerifiedManager.find(_.name == name))
  }

  override def getWorkerByName(name: Name): OptionT[IO, Worker] = OptionT {
    IO(listWorker.find(_.name == name))
  }

  override def getUnverifiedManagerByToken(token: String): OptionT[IO, UnverifiedManager] = OptionT {
    IO(listUnverifiedManager.find(_.confirmationToken == token))
  }

  override def deleteUnverifiedManager(manager: UnverifiedManager): IO[Unit] = IO {
    listUnverifiedManager -= manager
  }
}
