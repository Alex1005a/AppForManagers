package controllers

import Formats.JsonFormats.{ManagerDtoFormat, WorkerDtoFormat}
import auth.Authentication
import cats.effect.{ContextShift, IO}
import controllers.IOHttp.ActionBuilderOps
import models.{UnverifiedManager, Worker}
import play.api.http.Writeable
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Result}
import repositories.UserRepository
import services.{AccountService, AuthorizeConfig, CreateUserConfig, EmailSender}

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.ExecutionContext

abstract class AuthDto
case class ManagerDto(name: String, email: String, password: String) extends AuthDto
case class WorkerDto(name: String, password: String) extends AuthDto

class AccountController @Inject()(cc: ControllerComponents, auth: Authentication, repo: UserRepository[IO], email: EmailSender)(implicit ec: ExecutionContext) extends AbstractController(cc) {
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        UnverifiedManager(m.name, m.email, m.password, UUID.randomUUID().toString) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(manager) => toOk(AccountService.createUser(manager).run(CreateUserConfig(repo, email)))
        }

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        Worker(w.name, w.password) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(worker) => toOk(AccountService.createUser(worker).run(CreateUserConfig(repo, email)))
        }

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }

  def verifyManager(confirmationToken: String): Action[AnyContent] = Action.asyncF { implicit request =>
    AccountService.verifyManager(confirmationToken).run(repo).map {
      case Left(err) => InternalServerError(err)
      case Right(res) => Ok(res)
    }
  }

  def authorize: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        toOk(serviceAuthorize(m))

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        toOk(serviceAuthorize(w))

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }
  private def toOk[T](io: IO[T])(implicit writeable: Writeable[T]): IO[Result] = io.map(r => Ok(r))
  private def serviceAuthorize(dto: AuthDto): IO[String] = AccountService.authorize(dto).run(AuthorizeConfig(repo, auth)).map(_.fold(c => c, f => f))
}