package controllers

import Formats.JsonFormats.{ManagerAuthFormat, WorkerAuthFormat}
import auth.{ManagerAuth, WorkerAuth}
import libs.Env
import play.api.libs.json.Json
import javax.inject._
import play.api.mvc._

@Singleton
class HomeController @Inject()(cc: ControllerComponents, env: Env) extends AbstractController(cc) {

  def index: Action[AnyContent] = Action { implicit request =>
    val user = env.auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(v) => v match {
        case w: WorkerAuth => Ok(Json.toJson(w))
        case m: ManagerAuth => Ok(Json.toJson(m))
      }
      case Left(v) => InternalServerError(v)
    }
  }
}

