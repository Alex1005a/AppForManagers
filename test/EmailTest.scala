import models.Email
import org.scalatest.freespec.AsyncFreeSpec

class EmailTest extends AsyncFreeSpec {
  "EmailTestInvalid" in {
    assert(Email("test").isInvalid)
    assert(Email("t es t@gmailcom").isInvalid)
  }

  "EmailTestValid" in {
    assert(Email("test@gmail.com").isValid)
  }
}
