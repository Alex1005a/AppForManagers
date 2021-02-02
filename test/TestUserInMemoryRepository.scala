import models.UnverifiedManager
import org.scalatest.funsuite.AnyFunSuite
import repositories.UserInMemoryRepository

class TestUserInMemoryRepository extends AnyFunSuite {
  test("TestUserInMemoryRepository") {
    val token = "Token"
    val manager = UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.get

    val repo = new UserInMemoryRepository()
    repo.create(manager).unsafeRunSync()

    assert(repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isDefined)

    repo.deleteUnverifiedManager(manager).unsafeRunSync()

    assert(repo.getUnverifiedManagerByToken(token).value.unsafeRunSync().isEmpty)
  }
}
