package controllers

import Formats.JsonFormats.{ManagerAuthFormat, WorkerAuthFormat}
import auth._
import play.api.libs.json.Json
import javax.inject._
import play.api.mvc._
import cats.effect.{IO}

@Singleton
class HomeController @Inject()(cc: ControllerComponents,auth: Authentication) extends AbstractController(cc) {

  def index: Action[AnyContent] = Action { implicit request =>
    val user = auth.authenticate(request.headers.get("Authorization").getOrElse(""))
    user match {
      case Right(auth) => auth match {
        case workerAuth: WorkerAuth => Ok(Json.toJson(workerAuth))
        case managerAuth: ManagerAuth => Ok(Json.toJson(managerAuth))
      }
      case Left(error) => InternalServerError(error)
    }
  }
}

