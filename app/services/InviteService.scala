package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits._
import libs.{Process}
import models.Id.Id
import models.Invite
import models.User
import cats.Monad
import scala.concurrent.ExecutionContext
import scala.util.Random
import repositories._

object InviteService {


  def createInvite[F[_]: Monad](workerId: Id, managerId: Id)
  (implicit process: Process[F], userRepo: UserRepository[F], inviteRepo: InviteRepository[F], random: libs.Random[F]): F[Either[String, Invite]] = {
    val tuple = for {
        workerProcess <- process.fork(userRepo.getWorkerById(workerId).value)
        managerProcess <- process.fork(userRepo.getManagerById(managerId).value)
        worker <- process.await(workerProcess)
        manager <- process.await(managerProcess)
      } yield (manager, worker)

      tuple.flatMap {
        case (Some(_), Some(_)) =>
          val invite = random.randomByteArray.map(bytes => Invite(managerId, workerId, bytes))
          invite.flatMap(inviteRepo.create)
          invite.map(_.asRight)
        case _ => Monad[F].pure("not found manager or worker".asLeft[Invite])
      }
  }


  def getInvitesByManagerId[F[_] : Monad](id: Id)(implicit inviteRepo: InviteRepository[F]): F[Array[Invite]] = inviteRepo.getByManagerId(id)

  def getInvitesByWorkerId[F[_] : Monad](id: Id)(implicit inviteRepo: InviteRepository[F]): F[Array[Invite]] = inviteRepo.getByWorkerId(id)

  def confirmInvite[F[_] : Monad](inviteId: Id, workerId: Id)
  (implicit process: Process[F], inviteRepo: InviteRepository[F], userRepo: UserRepository[F]): F[Either[String, _]] =  {
    inviteRepo.getById(inviteId).value.flatMap {
        case Some(invite) => if (invite.workerId != workerId) Monad[F].pure(Left("It not your invite"))
        else {
          for {
            _ <- process.fork(inviteRepo.delete(invite))
            managerProcess <- process.fork(userRepo.getManagerById(invite.managerId).value)
            workerProcess <- process.fork(userRepo.getWorkerById(invite.workerId).value)
            manager <- process.await(managerProcess).map(_.get)
            worker <- process.await(workerProcess).map(_.get)
            _ <- process.fork(userRepo.updateManager(manager.copy(workers = manager.workers :+ invite.workerId)))
            _ <- process.fork(userRepo.updateWorker(worker.copy(managers = worker.managers :+ invite.managerId)))
            res <- Monad[F].pure(Right((): Unit))
          } yield res
        }
        case None => Monad[F].pure(Left("invite not find"))
      }
  }
}