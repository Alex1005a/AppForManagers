import models.VerifiedManager
import org.scalatest.funsuite.AnyFunSuite
import repositories.UserInMemoryRepository

class TestUserInMemoryRepository extends AnyFunSuite {
  test("TestUserInMemoryRepository") {
    val manager = VerifiedManager("Name", "test@gmail.com", "password").toOption.get

    val repo = new UserInMemoryRepository()
    repo.create(manager).unsafeRunSync()

    assert(repo.getManagerByName(manager.name).value.unsafeRunSync().isDefined)
  }
}
