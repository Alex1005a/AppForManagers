import models.PasswordHash
import org.scalatest.freespec.AsyncFreeSpec

class TestPasswordHash extends AsyncFreeSpec {
  "TestPasswordHash" in {
    val password = "password"
    val passwordHash = PasswordHash(password).toOption.get

    val checkResult = PasswordHash.checkPassword(password, passwordHash)

    assert(checkResult)
  }
}
