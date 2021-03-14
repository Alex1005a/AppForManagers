import auth.{Authentication, JWTAuthentication}
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import libs.Env
import models.UnverifiedManager
import org.scalatest.freespec.AsyncFreeSpec
import repositories.{UserInMemoryRepository, UserRepository}
import services.{AccountService, EmailSender}

class CreateUserTest extends AsyncFreeSpec with AsyncIOSpec {
  "CreateUserTest" in {
    val env = new TestEnv()
    val token = "Token"
    UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.value
      .map(_.get).flatMap { unverifiedManager =>
      AccountService.createUser(unverifiedManager).run(env)
    }.asserting(_ => assert(env.repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isDefined))
  }

  "CreateUserFailTest" in {
    val env = new TestEnvEmailFail()
    val token = "Token"
    UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.value
      .map(_.get).flatMap { unverifiedManager =>
      AccountService.createUser(unverifiedManager).run(env)
    }.asserting { result =>
      assert(result.isLeft)
      assert(env.repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isEmpty)
    }
  }
}

class TestEnv extends Env {

  override def email: EmailSender = new TestEmail()

  override def auth: Authentication = new JWTAuthentication()

  override val repo: UserRepository[IO] = new UserInMemoryRepository()
}

class TestEnvEmailFail extends Env {

  override def email: EmailSender = new TestEmailFail()

  override def auth: Authentication = new JWTAuthentication()

  override val repo: UserRepository[IO] = new UserInMemoryRepository()
}

class TestEmail extends EmailSender {
  override def sendEmail(toEmail: String, text: String, subject: String): IO[Unit] = IO.unit
}
class TestEmailFail extends EmailSender {
  override def sendEmail(toEmail: String, text: String, subject: String): IO[Unit] = IO.raiseError(new Exception("boom"))
}
