package controllers

import Formats.JsonFormats.{ManagerDtoFormat, WorkerDtoFormat}
import auth.Authentication
import cats.effect.{ContextShift, IO}
import controllers.IOHttp.ActionBuilderOps
import models.{UnverifiedManager, Worker}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import repositories.UserRepository
import services.{AccountService, AuthorizeConfig}

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.ExecutionContext

abstract class AuthDto
case class ManagerDto(name: String, email: String, password: String) extends AuthDto
case class WorkerDto(name: String, password: String) extends AuthDto

class AccountController @Inject()(cc: ControllerComponents, auth: Authentication, repo: UserRepository) (implicit ec: ExecutionContext) extends AbstractController(cc) {
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        UnverifiedManager(m.name, m.email, m.password, UUID.randomUUID().toString) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(manager) => AccountService.createUser(manager).run(repo).map(r => Ok(r))
        }

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        Worker(w.name, w.password) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(worker) => AccountService.createUser(worker).run(repo).map(r => Ok(r))
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
        serviceAuthorize(m).map(r => Ok(r))

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        serviceAuthorize(w).map(r => Ok(r))

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }

  private def serviceAuthorize(dto: AuthDto) =  AccountService.authorize(dto).run(AuthorizeConfig(repo, auth)).map(_.fold(c => c, f => f))
}