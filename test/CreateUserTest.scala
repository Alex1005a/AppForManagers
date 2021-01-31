import auth.{Authentication, JWTAuthentication}
import cats.effect.IO
import libs.Env
import models.Worker
import org.scalatest.funsuite.AnyFunSuite
import repositories.{UserInMemoryRepository, UserRepository}
import services.{AccountService, EmailSender}

class CreateUserTest extends AnyFunSuite {
  test("CreateUserTest") {
    val env = new TestEnv()
    val name = "Name"
    val worker = Worker(name, "password").toOption.get
    AccountService.createUser(worker).run(env).unsafeRunSync()
    assert(env.repo.getWorkerByName(name).value.unsafeRunSync().isDefined)
  }
}

class TestEnv extends Env {

  override def email: EmailSender = new TestEmail()

  override def auth: Authentication = new JWTAuthentication()

  override val repo: UserRepository[IO] = new UserInMemoryRepository()
}

class TestEmail extends EmailSender {
  override def sendEmail(toEmail: String, text: String, subject: String): IO[Unit] = IO.unit
}
