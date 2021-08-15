package models

import cats.effect.IO
import models.Id.Id

case class Invite(id: Id, managerId: Id, workerId: Id)

object Invite {
  def apply(managerId: Id, workerId: Id): IO[Invite] = {
    Id().map { id =>
      Invite(id, managerId, workerId)
    }
  }
}
