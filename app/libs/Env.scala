package libs

import auth.{Authentication, JWTAuthentication}
import cats.effect.IO
import repositories.{UserInMemoryRepository, UserRepository}
import services.{EmailSender, EmailService}

trait Env {
  def email: EmailSender
  def auth: Authentication
  def repo: UserRepository[IO]
}

class ProdEnv extends Env {
  override def email: EmailSender = new EmailService()

  override def auth: Authentication = new JWTAuthentication()

  override val repo = new UserInMemoryRepository()
}
