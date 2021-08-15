package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxEitherId
import controllers.{AuthDto, ManagerDto, WorkerDto}
import libs.Env
import models.Id.Id
import models.PasswordHash.PasswordHash
import models.{PasswordHash, UnverifiedManager, User, VerifiedManager}
import repositories.UserRepository
import scala.concurrent.ExecutionContext

object AccountService {

  implicit val cs: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  def createUser(user: User): Kleisli[IO, Env, Either[String, Id]] = Kleisli {
    (env: Env) =>
      user match {
        case unverifiedManager: UnverifiedManager =>
          for {
            j <- env.email.sendEmail(unverifiedManager.email, "http://localhost:9000/account/" + unverifiedManager.confirmationToken, "Subject").start
            id <- env.userRepository.create(user)
            res <- j.join.attempt
            _ <- if(res.isLeft) env.userRepository.deleteUnverifiedManager(unverifiedManager)
            else IO.unit
          } yield res.left.map(_.getMessage).map(_ =>id)

        case _ => env.userRepository.create(user).map(Right(_))
      }
  }

  def verifyManager(confirmationToken: String): Kleisli[IO, UserRepository[IO], Either[String, String]] = Kleisli {
    (repo: UserRepository[IO]) => {
      repo.getUnverifiedManagerByToken(confirmationToken).value.flatMap {
        case Some(v) =>
          val verifiedManagerIO = VerifiedManager(v.name, v.email, v.passwordHash).toOption.value.map(_.get)
          for {
            _ <- repo.deleteUnverifiedManager(v).start
            verifiedManager <- verifiedManagerIO
            id <- repo.create(verifiedManager)
          } yield s"Manager with $id verified successfully".asRight[String]

        case None => IO("Manager not find".asLeft[String])
      }
    }
  }

  def authorize(user: AuthDto): Kleisli[IO, Env, Either[String, String]] = Kleisli {
    (env: Env) => {
      user match {
        case managerDto: ManagerDto =>
          for {
            manager <- env.userRepository.getManagerByName(managerDto.name).value
          } yield manager match {
            case Some(manager) =>
              if(manager.email != managerDto.email) "Email not correct".asLeft[String]
              for {
                _ <- checkPassword(managerDto.password, manager.passwordHash)
              } yield env.auth.authorize(manager)
            case None => "Manager not find".asLeft[String]
          }
        case w: WorkerDto =>
          for {
            worker <- env.userRepository.getWorkerByName(w.name).value
          } yield worker match {
            case Some(worker) =>
              for {
                _ <- checkPassword(w.password, worker.passwordHash)
              } yield env.auth.authorize(worker)

            case None => "Worker not find".asLeft[String]
          }
      }
    }
  }

  private def checkPassword(password: String, hash: PasswordHash): Either[String, _] = {
    if(PasswordHash.checkPassword(password, hash)) Right()
    else "Password not correct".asLeft
  }
}
