package services

import auth.Authentication
import cats.data.Reader
import cats.implicits.catsSyntaxEitherId
import controllers.{AuthDto, ManagerDto, WorkerDto}
import models.Id.Id
import models.PasswordHash.PasswordHash
import models.{PasswordHash, UnverifiedManager, User, VerifiedManager}
import repositories.UserRepository

case class AuthorizeConfig(repo: UserRepository, auth: Authentication)

object AccountService {

  def createUser(user: User): Reader[UserRepository, Id] = {
    Reader(
      (repo: UserRepository) => {
        user match {
          case u: UnverifiedManager => EmailService.sendEmail(u.email, "http://localhost:9000/account/" + u.confirmationToken, "Subject").unsafeRunSync()
        }
        repo.create(user).unsafeRunSync()
      }
    )
  }

  def verifyManager(confirmationToken: String): Reader[UserRepository, Either[String, String]] = {
    Reader(
      (repo: UserRepository) => {
        repo.getUnverifiedManagerByToken(confirmationToken).value.unsafeRunSync() match {
          case Some(v) =>
            val verifiedManager = VerifiedManager(v.id, v.name, v.email, v.passwordHash)
            repo.deleteUnverifiedManager(v).unsafeRunSync()
            val id = repo.create(verifiedManager).unsafeRunSync()
            s"Manager with $id verified successfully".asRight[String]

          case None => "Manager not find".asLeft[String]
        }
      }
    )
  }

  def authorize(user: AuthDto): Reader[AuthorizeConfig, Either[String, String]] = {
    Reader(
      (conf: AuthorizeConfig) => {
        user match {
          case m: ManagerDto =>
            conf.repo.getManagerByName(m.name).value.unsafeRunSync() match {
              case Some(manager) =>
                if(manager.email != m.email) "Email not correct".asLeft[String]
                for {
                  _ <- checkPassword(m.password, manager.passwordHash)
                } yield conf.auth.authorize(manager)

              case None => "Manager not find".asLeft[String]
            }
          case w: WorkerDto =>
            conf.repo.getWorkerByName(w.name).value.unsafeRunSync() match {
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
