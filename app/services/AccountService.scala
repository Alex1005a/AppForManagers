package services

import auth.Authentication
import cats.data.Kleisli
import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import controllers.{AuthDto, ManagerDto, WorkerDto}
import models.Id.Id
import models.PasswordHash.PasswordHash
import models.{PasswordHash, UnverifiedManager, User, VerifiedManager}
import repositories.UserRepository

case class AuthorizeConfig(repo: UserRepository[IO], auth: Authentication)
case class CreateUserConfig(repo: UserRepository[IO], email: EmailSender)

object AccountService {

  def createUser(user: User): Kleisli[IO, CreateUserConfig, Id] = {
    Kleisli(
      (conf: CreateUserConfig) => for {
        _ <- user match {
          case u: UnverifiedManager => conf.email.sendEmail(u.email, "http://localhost:9000/account/" + u.confirmationToken, "Subject")
        }
        id <- conf.repo.create(user)
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

  def authorize(user: AuthDto): Kleisli[IO, AuthorizeConfig, Either[String, String]] = {
    Kleisli(
      (conf: AuthorizeConfig) => {
        user match {
          case m: ManagerDto =>
            for {
              manager <- conf.repo.getManagerByName(m.name).value
            } yield manager match {
              case Some(manager) =>
                if(manager.email != m.email) "Email not correct".asLeft[String]
                for {
                  _ <- checkPassword(m.password, manager.passwordHash)
                } yield conf.auth.authorize(manager)
              case None => "Manager not find".asLeft[String]
            }
          case w: WorkerDto =>
            for {
              worker <- conf.repo.getWorkerByName(w.name).value
            } yield worker match {
              case Some(worker) =>
                for {
                  _ <- checkPassword(w.password, worker.passwordHash)
                } yield conf.auth.authorize(worker)

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
