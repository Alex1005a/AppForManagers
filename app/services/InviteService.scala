package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits._
import libs.{Env, Process}
import models.Id.Id
import models.Invite
import models.User
import cats.Monad
import scala.concurrent.ExecutionContext
import scala.util.Random

object InviteService {

  implicit class ExtendedRandom(ran: scala.util.Random) {
  def nextByte = (ran.nextInt(256) - 128).toByte
}


  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def createInvite[F[_]: Monad](workerId: Id, managerId: Id)(implicit process: Process[F]): Kleisli[F, Env[F], Either[String, Invite]] = Kleisli {
    (env: Env[F]) => {
      val tuple = for {
        workerProcess <- process.fork(env.userRepository.getWorkerById(workerId).value)
        managerProcess <- process.fork(env.userRepository.getManagerById(managerId).value)
        worker <- process.await(workerProcess)
        manager <- process.await(managerProcess)
      } yield (manager, worker)

      tuple.flatMap {
        case (Some(_), Some(_)) =>
          val invite = Monad[F].pure(Array.fill(20)(scala.util.Random.nextByte)).map(bytes => Invite(managerId, workerId, bytes))
          invite.flatMap(env.inviteRepository.create)
          invite.map(_.asRight)
        case _ => Monad[F].pure("not found manager or worker".asLeft[Invite])
      }
    }
  }

  def getInvitesByManagerId[F[_] : Monad](id: Id): Kleisli[F, Env[F], Array[Invite]] = Kleisli {
    (env: Env[F]) => {
      env.inviteRepository.getByManagerId(id)
    }
  }

  def getInvitesByWorkerId[F[_] : Monad](id: Id): Kleisli[F, Env[F], Array[Invite]] = Kleisli {
    (env: Env[F]) => {
      env.inviteRepository.getByWorkerId(id)
    }
  }

  def confirmInvite[F[_] : Monad](inviteId: Id, workerId: Id)(implicit process: Process[F]): Kleisli[F, Env[F], Either[String, _]] = Kleisli {
    (env: Env[F]) => {
      env.inviteRepository.getById(inviteId).value.flatMap {
        case Some(invite) => if (invite.workerId != workerId) Monad[F].pure(Left("It not your invite"))
        else {
          for {
            _ <- process.fork(env.inviteRepository.delete(invite))
            managerProcess <- process.fork(env.userRepository.getManagerById(invite.managerId).value)
            workerProcess <- process.fork(env.userRepository.getWorkerById(invite.workerId).value)
            manager <- process.await(managerProcess).map(_.get)
            worker <- process.await(workerProcess).map(_.get)
            _ <- process.fork(env.userRepository.updateManager(manager.copy(workers = manager.workers :+ invite.workerId)))
            _ <- process.fork(env.userRepository.updateWorker(worker.copy(managers = worker.managers :+ invite.managerId)))
            res <- Monad[F].pure(Right((): Unit))
          } yield res
        }
        case None => Monad[F].pure(Left("invite not find"))
      }
    }
  }
}