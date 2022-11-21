package models

import cats.effect.IO
import models.Id.Id

case class Invite(id: Id, managerId: Id, workerId: Id)

object Invite {
  //private def apply(id: Id, managerId: Id, workerId: Id): Iv
  def apply(managerId: Id, workerId: Id, seed: Array[Byte]): Invite = {
    Invite(Id(seed), managerId, workerId)
  }
}
