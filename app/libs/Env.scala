package libs

import auth.{Authentication, JWTAuthentication}
import cats.effect.IO
import repositories.{InviteInMemoryRepository, InviteRepository, UserInMemoryRepository, UserRepository}
import services.{EmailSender, EmailService}

trait Env {
  def email: EmailSender
  def auth: Authentication
  def userRepository: UserRepository[IO]
  def inviteRepository: InviteRepository[IO]
}

class ProdEnv extends Env {
  override def email: EmailSender = new EmailService()

  override def auth: Authentication = new JWTAuthentication()

  override val userRepository = new UserInMemoryRepository()

  override val inviteRepository = new InviteInMemoryRepository()
}
