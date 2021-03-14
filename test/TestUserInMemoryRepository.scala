import cats.effect.testing.scalatest.AsyncIOSpec
import models.UnverifiedManager
import org.scalatest.freespec.AsyncFreeSpec
import repositories.UserInMemoryRepository

class TestUserInMemoryRepository extends AsyncFreeSpec with AsyncIOSpec {
  "TestUserInMemoryRepository" in {
    val token = "Token"
    val repo = new UserInMemoryRepository()
    UnverifiedManager("Name", "test@gmail.com", "password", token).toOption.value
      .map(_.get).map { unverifiedManager =>
      repo.create(unverifiedManager)
      repo.getUnverifiedManagerByToken(token).value.asserting(option => assert(option.isDefined))
      unverifiedManager
    }.flatMap { unverifiedManager =>
      repo.deleteUnverifiedManager(unverifiedManager)
      repo.getUnverifiedManagerByToken(token).value.asserting(option => assert(option.isEmpty))
    }
  }
}
