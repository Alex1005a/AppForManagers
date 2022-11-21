import cats.effect.testing.scalatest.AsyncIOSpec
import models.Worker
import org.scalatest.freespec.AsyncFreeSpec

class NameTest extends AsyncFreeSpec with AsyncIOSpec {
  "NameTest" in  {
    val unsafeRandomByteArray = Array.fill(20)((scala.util.Random.nextInt(256) - 128).toByte)
    val name = "Name"
    val worker =  Worker(name, "password", unsafeRandomByteArray)
    assert(worker.isValid)
    assert(worker.toOption.get.name == name)
  }
}
