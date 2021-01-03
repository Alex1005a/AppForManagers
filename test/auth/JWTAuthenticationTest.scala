package auth

import models.VerifiedManager
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class JWTAuthenticationTest extends AnyFunSuite {
  test("JWTAuthenticationSuccessTest") {
    val user = VerifiedManager("Name", "test@gmail.com", "password").toOption.get
    val jwt = new JWTAuthentication().authorize(user)
    val userRequest = new JWTAuthentication().authenticate(jwt)

    assert(userRequest.isRight)
    userRequest match {
      case Right(user1) => assert(user1.asInstanceOf[ManagerAuth] == ManagerAuth(user.id, user.name, user.email))
    }
  }

  test("JWTAuthenticationFailureTest") {
    val userRequest = new JWTAuthentication().authenticate(Random.nextString(30))

    assert(userRequest.isLeft)
  }
}
