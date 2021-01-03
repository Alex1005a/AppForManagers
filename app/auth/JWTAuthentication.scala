package auth

import authentikat.jwt._
import play.api.libs.json.Json
import Formats.JsonFormats.{ManagerAuthFormat, WorkerAuthFormat}
import cats.effect.IO
import cats.implicits.catsSyntaxEitherId
import com.typesafe.config.ConfigFactory
import models.{Manager, User, Worker}

trait UserAuth
case class ManagerAuth(id: String, name: String, email: String) extends UserAuth
case class WorkerAuth(id: String, name: String) extends UserAuth

private object JwtUtility {

  private val config = IO { ConfigFactory.load() } .unsafeRunSync()
  val JwtSecretKey: String = config.getString("myJwtSecretKey")
  val JwtSecretAlgo: String = config.getString("myJwtSecretAlgo")

  def createToken(payload: String): String = {
    val header = JwtHeader(JwtSecretAlgo)
    val claimsSet = JwtClaimsSet(payload)

    JsonWebToken(header, claimsSet, JwtSecretKey)
  }

  def isValidToken(jwtToken: String): Boolean =
    JsonWebToken.validate(jwtToken, JwtSecretKey)

  def decodePayload(jwtToken: String): Option[String] =
    jwtToken match {
      case JsonWebToken(_, claimsSet, _) => Option(claimsSet.asJsonString)
      case _                                          => None
    }
}

class JWTAuthentication extends Authentication {

  def authenticate(token: String): Either[String, UserAuth] = {
    val jwtToken = token

    if (JwtUtility.isValidToken(jwtToken)) {
      JwtUtility.decodePayload(jwtToken).fold {
        "Invalid credential".asLeft[UserAuth]
      }
      {
        payload => {
          payload match {
            case p if Json.parse(p).validate[ManagerAuth].isSuccess =>
              Json.parse(payload).validate[ManagerAuth].get.asRight[String]

            case p if Json.parse(p).validate[WorkerAuth].isSuccess =>
              Json.parse(payload).validate[WorkerAuth].get.asRight[String]

            case _ => "Error json parsing".asLeft[UserAuth]
          }
      }
      }
    }
    else {
      "Invalid credential".asLeft[UserAuth]
    }
  }

  def authorize(user: User): String = {
    user match {
      case m: Manager => JwtUtility.createToken(Json.toJson(ManagerAuth(m.id, m.name, m.email)).toString())
      case w: Worker => JwtUtility.createToken(Json.toJson(WorkerAuth(w.id, w.name)).toString())
    }
  }
}

trait Authentication {

  def authenticate(token: String): Either[String, UserAuth]

  def authorize(user: User): String
}
