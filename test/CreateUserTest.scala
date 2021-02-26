import auth.{Authentication, JWTAuthentication}
import cats.effect.IO
import libs.Env
import models.UnverifiedManager
import org.scalatest.funsuite.AnyFunSuite
import repositories.{UserInMemoryRepository, UserRepository}
import services.{AccountService, EmailSender}

class CreateUserTest extends AnyFunSuite {
  test("CreateUserTest") {
    val env = new TestEnv()
    val token = "Token"
    val manager = UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.get
    AccountService.createUser(manager).run(env).unsafeRunSync()
    assert(env.repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isDefined)
  }

  test("CreateUserFailTest") {
    val env = new TestEnvEmailFail()
    val token = "Token"
    val manager = UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.get
    val res = AccountService.createUser(manager).run(env).unsafeRunSync()

    assert(res.isLeft)
    assert(env.repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isEmpty)
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
