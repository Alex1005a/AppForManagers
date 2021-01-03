import models.Email
import org.scalatest.funsuite.AnyFunSuite

class EmailTest extends AnyFunSuite {
  test("EmailTestNotValid") {
    assert(Email("test").isLeft)
    assert(Email("t es t@gmailcom").isLeft)
  }

  test("EmailTestValid") {
    assert(Email("test@gmail.com").isRight)
  }
}
