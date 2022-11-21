package libs

import auth.{Authentication, JWTAuthentication}
import cats.effect.{IO, ContextShift}
import repositories.{InviteInMemoryRepository, InviteRepository, UserInMemoryRepository, UserRepository}
import services.{EmailSender, EmailService}

trait Env[F[_]] {
  def email: EmailSender[F]
  def auth: Authentication
  def userRepository: UserRepository[F]
  def inviteRepository: InviteRepository[F]
  def random: Random[F]
}

class ProdEnv(implicit ec: ContextShift[IO]) extends Env[IO] {
  override def email: EmailSender[IO] = new EmailService()

  override def auth: Authentication = new JWTAuthentication()

  override val userRepository = new UserInMemoryRepository()

  override val inviteRepository = new InviteInMemoryRepository()

  override val random = new RandomIO()
}
