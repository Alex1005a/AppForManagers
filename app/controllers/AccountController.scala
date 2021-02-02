package controllers

import Formats.JsonFormats.{ManagerDtoFormat, WorkerDtoFormat}
import cats.effect.{ContextShift, IO}
import libs.Env
import libs.http.ActionBuilderOps
import models.{UnverifiedManager, User, Worker}
import play.api.http.Writeable
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents, Result}
import services.AccountService

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.ExecutionContext

abstract class AuthDto
case class ManagerDto(name: String, email: String, password: String) extends AuthDto
case class WorkerDto(name: String, password: String) extends AuthDto

class AccountController @Inject()(cc: ControllerComponents, env: Env)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  implicit val cs: ContextShift[IO] = IO.contextShift(ec)

  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        UnverifiedManager(m.name, m.email, m.password, UUID.randomUUID().toString) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(manager) => createUser(env, manager)
        }

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        Worker(w.name, w.password) match {
          case Left(err) => IO.pure(InternalServerError(err))
          case Right(worker) => createUser(env, worker)
        }

      case Some(_) => IO.pure(InternalServerError("Parse json error"))

      case None => IO.pure(Ok("Missing or incorrect body"))
    }
  }

  def verifyManager(confirmationToken: String): Action[AnyContent] = Action.asyncF { implicit request =>
    AccountService.verifyManager(confirmationToken).run(env.repo).map {
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
  private def createUser(env: Env, user: User): IO[Result] = toOk(AccountService.createUser(user).run(env).map(
    f => f.fold(err => err, ok => ok)
  ))
  private def toOk[T](io: IO[T])(implicit writeable: Writeable[T]): IO[Result] = io.map(r => Ok(r))
  private def serviceAuthorize(dto: AuthDto): IO[String] = AccountService.authorize(dto).run(env).map(_.fold(c => c, f => f))
}