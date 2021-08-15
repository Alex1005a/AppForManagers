package Formats

import auth.{ManagerAuth, WorkerAuth}
import controllers.{ManagerDto, WorkerDto}
import models.Invite
import play.api.libs.json.{Json, OFormat}

object JsonFormats {
  implicit val ManagerAuthFormat: OFormat[ManagerAuth] = Json.format[ManagerAuth]
  implicit val WorkerAuthFormat: OFormat[WorkerAuth] = Json.format[WorkerAuth]
  implicit val ManagerDtoFormat: OFormat[ManagerDto] = Json.format[ManagerDto]
  implicit val WorkerDtoFormat: OFormat[WorkerDto] = Json.format[WorkerDto]
  implicit val InviteFormat: OFormat[Invite] = Json.format[Invite]
}
