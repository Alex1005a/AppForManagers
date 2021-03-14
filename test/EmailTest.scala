import models.Email
import org.scalatest.freespec.AsyncFreeSpec

class EmailTest extends AsyncFreeSpec {
  "EmailTestNotValid" in {
    assert(Email("test").isLeft)
    assert(Email("t es t@gmailcom").isLeft)
  }

  "EmailTestValid" in {
    assert(Email("test@gmail.com").isRight)
  }
}
