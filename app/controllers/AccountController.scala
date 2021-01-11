package controllers

import Formats.JsonFormats.{ManagerDtoFormat, WorkerDtoFormat}
import auth.Authentication
import models.{UnverifiedManager, Worker}
import play.api.mvc.{AbstractController, Action, AnyContent, ControllerComponents}
import repositories.UserRepository
import services.{AccountService, AuthorizeConfig}
import java.util.UUID
import javax.inject.Inject

abstract class AuthDto
case class ManagerDto(name: String, email: String, password: String) extends AuthDto
case class WorkerDto(name: String, password: String) extends AuthDto

class AccountController @Inject()(cc: ControllerComponents, auth: Authentication, repo: UserRepository) extends AbstractController(cc) {
  def create: Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        UnverifiedManager(m.name, m.email, m.password, UUID.randomUUID().toString).fold(
          err => InternalServerError(err),
          manager => Ok(AccountService.createUser(manager).run(repo).unsafeRunSync())
        )

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        Worker(w.name, w.password).fold(
          err => InternalServerError(err),
          worker => Ok(AccountService.createUser(worker).run(repo).unsafeRunSync())
        )

      case Some(_) => InternalServerError("Parse json error")

      case None => Ok("Missing or incorrect body")
    }
  }

  def verifyManager(confirmationToken: String): Action[AnyContent] = Action {
    AccountService.verifyManager(confirmationToken).run(repo).map { result =>
      result.fold(
        err => InternalServerError(err),
        res => Ok(res)
      )
    }.unsafeRunSync()
  }

  def authorize: Action[AnyContent] = Action { implicit request =>
    request.body.asJson match {
      case Some(v) if v.validate[ManagerDto].isSuccess =>
        val m = v.validate[ManagerDto].get
        Ok(serviceAuthorize(m).unsafeRunSync())

      case Some(v) if v.validate[WorkerDto].isSuccess =>
        val w = v.validate[WorkerDto].get
        Ok(serviceAuthorize(w).unsafeRunSync())

      case Some(_) => InternalServerError("Parse json error")

      case None => Ok("Missing or incorrect body")
    }
  }

  private def serviceAuthorize(dto: AuthDto) =  AccountService.authorize(dto).run(AuthorizeConfig(repo, auth)).map(_.fold(c => c, f => f))
}

/*
request.body.asJson match {
      case Some(v) => v.validate[WorkerDto].fold(
        _ => v.validate[ManagerDto].fold(
          _ => InternalServerError("Parse json error"),
          m => Manager(m.name, m.email, m.password).fold(
            err => InternalServerError(err),
            manager => Ok(AccountService.createUser(manager).run(repo))
          )),
        w => Worker(w.name, w.password).fold(
          err => InternalServerError(err),
          worker => Ok(AccountService.createUser(worker).run(repo))
        ))
        InternalServerError("Parse json error")
      case None => Ok("Missing or incorrect body")
    }
 */
