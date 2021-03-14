import cats.effect.testing.scalatest.AsyncIOSpec
import models.Worker
import org.scalatest.freespec.AsyncFreeSpec

class NameTest extends AsyncFreeSpec with AsyncIOSpec {
  "NameTest" in {
    val name = "Name"
    Worker(name, "password").value.map { createResult =>
      assert(createResult.isRight)
      assert(createResult.toOption.get.name == name)
    }
  }
}
