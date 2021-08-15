package repositories

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import models.Id.Id
import models.Invite

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

trait InviteRepository[F[_]] {

  def create(invite: Invite): F[Id]

  def getById(id: Id): OptionT[F, Invite]

  def getByWorkerId(id: Id): F[Array[Invite]]

  def getByManagerId(id: Id): F[Array[Invite]]

  def delete(invite: Invite): F[Unit]
}

class InviteInMemoryRepository extends InviteRepository[IO] {
  private val invites = Ref.of[IO, ListBuffer[Invite]](ListBuffer.empty[Invite])

  override def create(invite: Invite): IO[Id] = for {
      ref <- invites
      _ <- ref.update(_ += invite)
  } yield invite.id

  override def getById(id: Id): OptionT[IO, Invite] = OptionT {
    for {
      ref <- invites
      result <- ref.get.map(_.find(_.id == id))
    } yield result
  }

  override def getByWorkerId(id: Id): IO[Array[Invite]] = for {
    ref <- invites
    result <- ref.get.map(_.filter(_.workerId == id).toArray)
  } yield result

  override def getByManagerId(id: Id): IO[Array[Invite]] = for {
    ref <- invites
    result <- ref.get.map(_.filter(_.managerId == id).toArray)
  } yield result

  override def delete(invite: Invite): IO[Unit] = {
    for {
      ref <- invites
      _ <- ref.update(_ -= invite)
    } yield ()
  }
}