package controllers

import auth.{ManagerAuth, WorkerAuth}
import cats.effect.IO
import libs.Env
import libs.http.ActionBuilderOps
import Formats.JsonFormats.InviteFormat
import play.api.libs.json.Json
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import services.InviteService
import javax.inject.Inject
import scala.concurrent.ExecutionContext


class InviteController @Inject()(cc: ControllerComponents, env: Env)(implicit ec: ExecutionContext) extends AbstractController(cc) {
  def create: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(manager: ManagerAuth) =>
        request.body.asText match {
          case Some(workerId) =>  InviteService.createInvite(workerId, manager.id).run(env)
            .map(
              _.fold(
                err => InternalServerError(err),
                i => Ok(Json.toJson(i))
              )
            )
          case None => IO(InternalServerError("Not find worker id"))
        }
      case Left(v) => IO(InternalServerError(v))
      case _ => IO(InternalServerError("You are worker, but not manager"))
    }
  }

  def getInvites: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(v) => v match {
        case w: WorkerAuth => InviteService.getInvitesByWorkerId(w.id).run(env).map(res => Ok(Json.toJson(res)))
        case m: ManagerAuth => InviteService.getInvitesByManagerId(m.id).run(env).map(res => Ok(Json.toJson(res)))
      }
      case Left(v) => IO(InternalServerError(v))
    }
  }

  def confirmInvite: Action[AnyContent] = Action.asyncF { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(worker: WorkerAuth) =>
        request.body.asText match {
          case Some(inviteId) => InviteService.confirmInvite(inviteId, worker.id).run(env)
            .map(_.fold(InternalServerError(_), _ => Ok("Invite confirmed!!")))
          case None => IO(InternalServerError("Not find invite id"))
        }
      case Left(v) => IO(InternalServerError(v))
      case _ => IO(InternalServerError("You are manager, but not worker"))
    }
  }
}