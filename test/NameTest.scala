import models.Worker
import org.scalatest.funsuite.AnyFunSuite

class NameTest extends AnyFunSuite {
  test("NameTest") {
    val name = "Name"
    val w = Worker(name, "password").value.unsafeRunSync
    assert(w.isRight)
    assert(w.toOption.get.name == name)
  }
}
