package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxEitherId
import controllers.{AuthDto, ManagerDto, WorkerDto}
import libs.{Process}
import models.Id.Id
import models.PasswordHash.PasswordHash
import models.{PasswordHash, UnverifiedManager, User, VerifiedManager}
import repositories.UserRepository
import scala.concurrent.ExecutionContext
import cats.Monad
import cats.effect.Concurrent
import cats.syntax.all._
import scala.util.Random
import auth._


object AccountService {

  def createUser[F[_] : Monad](user: User)
  (implicit userRepo: UserRepository[F], process: Process[F], email: EmailSender[F]): F[Either[String, Id]] =  {
    user match {
        case unverifiedManager: UnverifiedManager =>
          for {
            j <- process.fork(email.sendEmail(unverifiedManager.email, 
                  "http://localhost:9000/account/" + unverifiedManager.confirmationToken, "Subject"))
            id <- userRepo.create(user)
            res <- process.await(j)
          } yield Right(id)

        case _ => for {
          k <- userRepo.create(user)
        } yield Right(k)
      }
      
  }

  def verifyManager[F[_]:  Monad](confirmationToken: String)
  (implicit userRepo: UserRepository[F], process: Process[F], random: libs.Random[F]): F[Either[String, String]] =  {
    userRepo.getUnverifiedManagerByToken(confirmationToken).value.flatMap {
        case Some(v) =>
          for {
            _ <- process.fork(userRepo.deleteUnverifiedManager(v))
            randArr <- random.randomByteArray
            verifiedManager = VerifiedManager(v.name, v.email, v.passwordHash, randArr).map(userRepo.create)
            id <- verifiedManager.toEither match {
              case Left(nonEmptyStrings) => Monad[F].pure(nonEmptyStrings.head.asLeft[String])
              case Right(idF) => idF.map(_.asRight[String])
            }
          } yield id

        case None => Monad[F].pure("Manager not find".asLeft[String])
      }
  }

  def authorize[F[_] : Monad](user: AuthDto)(implicit userRepo: UserRepository[F], auth: Authentication): F[Either[String, String]] = {
    user match {
        case managerDto: ManagerDto =>
          for {
            manager <- userRepo.getManagerByName(managerDto.name).value
          } yield manager match {
            case Some(manager) =>
              if(manager.email != managerDto.email) "Email not correct".asLeft[String]
              for {
                _ <- checkPassword(managerDto.password, manager.passwordHash)
              } yield auth.authorize(manager)
            case None => "Manager not find".asLeft[String]
          }
        case w: WorkerDto =>
          for {
            worker <- userRepo.getWorkerByName(w.name).value
          } yield worker match {
            case Some(worker) =>
              for {
                _ <- checkPassword(w.password, worker.passwordHash)
              } yield auth.authorize(worker)

            case None => "Worker not find".asLeft[String]
          }
      }
  }

  private def checkPassword(password: String, hash: PasswordHash): Either[String, _] = {
    if(PasswordHash.checkPassword(password, hash)) Right((): Unit)
    else "Password not correct".asLeft
  }
}
