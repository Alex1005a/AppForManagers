package controllers

import auth.{ManagerAuth, WorkerAuth}
import cats.effect.{ContextShift, IO}
import libs.{Env, Process, ProcessIO}
import libs.http.ActionBuilderOps
import Formats.JsonFormats.InviteFormat
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import services.InviteService
import javax.inject.Inject
import scala.concurrent.ExecutionContext


class InviteController @Inject()(cc: ControllerComponents, env: Env[IO])(implicit ec: ExecutionContext) extends AbstractController(cc) {
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val process: Process[IO] = new ProcessIO()
  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(manager: ManagerAuth) =>
        request.body.asText match {
          case Some(workerId) =>  InviteService.createInvite[IO](workerId, manager.id).run(env)
            .map(
              _.fold(
                error => InternalServerError(error),
                invite => Ok(Json.toJson(invite))
              )
            )
          case None => IO(InternalServerError("Not find worker id"))
        }
      case Left(error) => IO(InternalServerError(error))
      case _ => IO(InternalServerError("You are worker, but not manager"))
    }
  }

  def getInvites: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(v) => v match {
        case w: WorkerAuth => InviteService.getInvitesByWorkerId[IO](w.id).run(env).map(res => Ok(Json.toJson(res)))
        case m: ManagerAuth => InviteService.getInvitesByManagerId[IO](m.id).run(env).map(res => Ok(Json.toJson(res)))
      }
      case Left(error) => IO(InternalServerError(error))
    }
  }

  def confirmInvite: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(worker: WorkerAuth) =>
        request.body.asText match {
          case Some(inviteId) => InviteService.confirmInvite[IO](inviteId, worker.id).run(env)
            .map(_.fold(InternalServerError(_), _ => Ok("Invite confirmed!!")))
          case None => IO(InternalServerError("Not find invite id"))
        }
      case Left(error) => IO(InternalServerError(error))
      case _ => IO(InternalServerError("You are manager, but not worker"))
    }
  }
}
