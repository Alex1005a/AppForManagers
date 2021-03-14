package auth

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import models.VerifiedManager
import org.scalatest.freespec.AsyncFreeSpec

import scala.util.Random

class JWTAuthenticationTest extends AsyncFreeSpec with AsyncIOSpec {
  "JWTAuthenticationSuccessTest" in {
    val jwtAuthentication = new JWTAuthentication()

    VerifiedManager("Name", "test@gmail.com", "password").toOption.value.map(_.get).map { user =>
      val jwt = jwtAuthentication.authorize(user)
      val result = jwtAuthentication.authenticate(jwt)
      assert(result.isRight)
      assert(result.toOption.get.asInstanceOf[ManagerAuth] == ManagerAuth(user.id, user.name, user.email))
    }
  }

  "JWTAuthenticationFailureTest" in {
    def randomString: IO[String] = IO(Random.nextString(30))

    val jwtAuthentication = new JWTAuthentication()
    randomString.map(jwtAuthentication.authenticate).asserting { res =>
      assert(res.isLeft)
    }
  }
}
