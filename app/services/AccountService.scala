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

  def createUser(user: User): Kleisli[IO, Env, Id] = {
    Kleisli(
      (env: Env) => for {
        _ <- user match {
          case u: UnverifiedManager =>
            env.email.sendEmail(u.email, "http://localhost:9000/account/" + u.confirmationToken, "Subject").start
        }
        id <- env.repo.create(user)
      } yield id
    )
  }

  def verifyManager(confirmationToken: String): Kleisli[IO, UserRepository[IO], Either[String, String]] = {
    Kleisli(
      (repo: UserRepository[IO]) => {
        for {
          manager <- repo.getUnverifiedManagerByToken(confirmationToken).value
        } yield manager match {
          case Some(v) =>
            val verifiedManager = VerifiedManager(v.id, v.name, v.email, v.passwordHash)
            for {
              _ <- repo.deleteUnverifiedManager(v)
            } yield ()

            val id = repo.create(verifiedManager).unsafeRunSync()
            s"Manager with $id verified successfully".asRight[String]

          case None => "Manager not find".asLeft[String]
        }
      }
    )
  }

  def authorize(user: AuthDto): Kleisli[IO, Env, Either[String, String]] = {
    Kleisli(
      (env: Env) => {
        user match {
          case m: ManagerDto =>
            for {
              manager <- env.repo.getManagerByName(m.name).value
            } yield manager match {
              case Some(manager) =>
                if(manager.email != m.email) "Email not correct".asLeft[String]
                for {
                  _ <- checkPassword(m.password, manager.passwordHash)
                } yield env.auth.authorize(manager)
              case None => "Manager not find".asLeft[String]
            }
          case w: WorkerDto =>
            for {
              worker <- env.repo.getWorkerByName(w.name).value
            } yield worker match {
              case Some(worker) =>
                for {
                  _ <- checkPassword(w.password, worker.passwordHash)
                } yield env.auth.authorize(worker)

              case None => "Worker not find".asLeft[String]
            }
        }
      }
    )
  }

  private def checkPassword(password: String, hash: PasswordHash): Either[String, _] = {
    if(PasswordHash.checkPassword(password, hash)) Right()
    else "Password not correct".asLeft
  }

}
