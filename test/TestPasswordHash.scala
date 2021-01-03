import models.PasswordHash
import org.scalatest.funsuite.AnyFunSuite

class TestPasswordHash extends AnyFunSuite {
  test("TestPasswordHash") {
    val password = "password"
    val passwordHash = PasswordHash(password).toOption.get

    val checkResult = PasswordHash.checkPassword(password, passwordHash)

    assert(checkResult)
  }
}
