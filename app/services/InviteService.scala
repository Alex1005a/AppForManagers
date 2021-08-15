package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxEitherId
import libs.Env
import models.Id.Id
import models.Invite
import scala.concurrent.ExecutionContext

object InviteService {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def createInvite(workerId: Id, managerId: Id): Kleisli[IO, Env, Either[String, Invite]] = Kleisli {
    (env: Env) => {
      val tuple = for {
        workerFiber <- env.userRepository.getWorkerById(workerId).value.start
        managerFiber <- env.userRepository.getManagerById(managerId).value.start
        worker <- workerFiber.join
        manager <- managerFiber.join
      } yield (manager, worker)

      tuple.flatMap {
        case (Some(_), Some(_)) =>
          val invite = Invite(managerId, workerId)
          invite.flatMap(i => env.inviteRepository.create(i)).start
          invite.map(_.asRight)
        case _ => IO("not found manager or worker".asLeft[Invite])
      }
    }
  }

  def getInvitesByManagerId(id: Id): Kleisli[IO, Env, Array[Invite]] = Kleisli {
    (env: Env) => {
      env.inviteRepository.getByManagerId(id)
    }
  }

  def getInvitesByWorkerId(id: Id): Kleisli[IO, Env, Array[Invite]] = Kleisli {
    (env: Env) => {
      env.inviteRepository.getByWorkerId(id)
    }
  }

  def confirmInvite(inviteId: Id, workerId: Id): Kleisli[IO, Env, Either[String, _]] = Kleisli {
    (env: Env) => {
      env.inviteRepository.getById(inviteId).value.flatMap {
        case Some(invite) => if (invite.workerId != workerId) IO(Left("It not your invite"))
        else {
          for {
            _ <- env.inviteRepository.delete(invite).start
            fiberManager <- env.userRepository.getManagerById(invite.managerId).value.start
            fiberWorker <- env.userRepository.getWorkerById(invite.workerId).value.start
            manager <- fiberManager.join.map(_.get)
            worker <- fiberWorker.join.map(_.get)
            _ <- env.userRepository.updateManager(manager.copy(workers = manager.workers :+ invite.workerId)).start
            _ <- env.userRepository.updateWorker(worker.copy(managers = worker.managers :+ invite.managerId)).start
            res <- IO(Right())
          } yield res
        }
        case None => IO(Left("invite not find"))
      }
    }
  }
}