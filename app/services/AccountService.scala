package services

import cats.data.Kleisli
import cats.effect.{ContextShift, IO}
import cats.implicits.catsSyntaxEitherId
import controllers.{AuthDto, ManagerDto, WorkerDto}
import libs.{Env, Process}
import models.Id.Id
import models.PasswordHash.PasswordHash
import models.{PasswordHash, UnverifiedManager, User, VerifiedManager}
import repositories.UserRepository
import scala.concurrent.ExecutionContext
import cats.Monad
import cats.effect.Concurrent
import cats.syntax.all._
import scala.util.Random


object AccountService {

  implicit class ExtendedRandom(ran: scala.util.Random) {
  def nextByte = (ran.nextInt(256) - 128).toByte
}


  def createUser[F[_] : Monad](user: User)
  (implicit userRepo: UserRepository[F], process: Process[F]): Kleisli[F, Env[F], Either[String, Id]] = Kleisli {
    (env: Env[F]) =>
      user match {
        case unverifiedManager: UnverifiedManager =>
          for {
            j <- process.fork(env.email.sendEmail(unverifiedManager.email, 
                  "http://localhost:9000/account/" + unverifiedManager.confirmationToken, "Subject"))
            id <- env.userRepository.create(user)
            res <- process.await(j)
          } yield Right(id)

        case _ => for {
          k <- env.userRepository.create(user)
        } yield Right(k)
      }
  }

  def verifyManager[F[_]:  Monad](confirmationToken: String)
  (implicit userRepo: UserRepository[F], process: Process[F]): Kleisli[F, UserRepository[F], Either[String, String]] = Kleisli {
    (repo: UserRepository[F]) => {
      repo.getUnverifiedManagerByToken(confirmationToken).value.flatMap {
        case Some(v) =>
   
          for {
            _ <- process.fork(repo.deleteUnverifiedManager(v))
            randArr <- Monad[F].pure(Array.fill(20)(scala.util.Random.nextByte))
            idF <- Monad[F].pure(VerifiedManager(v.name, v.email, v.passwordHash, randArr).map(repo.create))
            id <- idF.toEither match {
              case Left(nonEmptyStrings) => Monad[F].pure(nonEmptyStrings.head.asLeft[String])
              case Right(idF) => idF.map(_.asRight[String])
            }
          } yield id

        case None => Monad[F].pure("Manager not find".asLeft[String])
      }
    }
  }

  def authorize[F[_] : Monad](user: AuthDto)(implicit U: UserRepository[F]): Kleisli[F, Env[F], Either[String, String]] = Kleisli {
    (env: Env[F]) => {
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
    if(PasswordHash.checkPassword(password, hash)) Right((): Unit)
    else "Password not correct".asLeft
  }
}
