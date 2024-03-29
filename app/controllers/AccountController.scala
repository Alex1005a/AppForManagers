package controllers

import Formats.JsonFormats.{ManagerDtoFormat, WorkerDtoFormat}
import cats.data.NonEmptyList
import cats.effect.{ContextShift, IO}
import libs.{Process, ProcessIO}
import libs.http.ActionBuilderOps
import models.{UnverifiedManager, User, Worker}
import play.api.http.Writeable
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Result}
import services.AccountService
import java.util.UUID
import javax.inject.Inject
import scala.concurrent.ExecutionContext
import repositories.{UserRepository, UserInMemoryRepository}
import repositories._
import libs._
import auth._
import services.{EmailService, EmailSender}

abstract class AuthDto
case class ManagerDto(name: String, email: String, password: String) extends AuthDto
case class WorkerDto(name: String, password: String) extends AuthDto

class AccountController @Inject()
(cc: ControllerComponents, inviteRepo: InviteRepository[IO], 
userRepo: UserRepository[IO], random: Random[IO], authentication: Authentication, 
process: Process[IO], emailSender: EmailSender[IO])(implicit ec: ExecutionContext) extends AbstractController(cc) {

  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val proc: Process[IO] = process
  implicit val userRepository: UserRepository[IO] = userRepo
  implicit val inviteRepository: InviteRepository[IO] = inviteRepo
  implicit val rand: Random[IO] = random
  implicit val email: EmailSender[IO] = emailSender
  implicit val auth: Authentication = authentication

  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(jsonBody) if jsonBody.validate[ManagerDto].isSuccess =>
        val m = jsonBody.validate[ManagerDto].get
        UnverifiedManager(m.name, m.email, m.password, UUID.randomUUID().toString, Array.fill(20)((scala.util.Random.nextInt(256) - 128).toByte)).toEither match {
          case Left(err) => IO.pure(serverError(NonEmptyList.fromReducible(err)))
          case Right(manager) => createUser(manager)
        }

      case Some(jsonBody) if jsonBody.validate[WorkerDto].isSuccess =>
        val w = jsonBody.validate[WorkerDto].get
        Worker(w.name, w.password, Array.fill(20)((scala.util.Random.nextInt(256) - 128).toByte)).toEither match {
          case Left(err) => IO.pure(serverError(NonEmptyList.fromReducible(err)))
          case Right(worker) => createUser(worker)
        }

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }

  def verifyManager(confirmationToken: String): Action[AnyContent] = Action.asyncF { implicit request =>
    AccountService.verifyManager[IO](confirmationToken).map {
      case Left(error) => InternalServerError(error)
      case Right(result) => Ok(result)
    }
  }

  def authorize: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(jsonBody) if jsonBody.validate[ManagerDto].isSuccess =>
        val managerDto = jsonBody.validate[ManagerDto].get
        toOk(serviceAuthorize(managerDto))

      case Some(jsonBody) if jsonBody.validate[WorkerDto].isSuccess =>
        val workerDto = jsonBody.validate[WorkerDto].get
        toOk(serviceAuthorize(workerDto))

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }

  private def serverError(errors: NonEmptyList[String]) = InternalServerError(errors.foldLeft("")((a, b) => a + ". " + b))
  private def createUser(user: User): IO[Result] =
    toOk(AccountService.createUser[IO](user).map(_.merge))
  private def toOk[T](io: IO[T])(implicit writeable: Writeable[T]): IO[Result] = io.map(Ok(_))
  private def serviceAuthorize(authDto: AuthDto)(implicit auth: Authentication): IO[String] =
     AccountService.authorize[IO](authDto).map(_.merge)
}