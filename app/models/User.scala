package models


import cats.data.{EitherNel, EitherT}
import cats.effect.IO
import io.chrisdavenport.fuuid.FUUID
import models.Email.Email
import models.Id.Id
import models.Name.Name
import models.PasswordHash.PasswordHash
import org.mindrot.jbcrypt.BCrypt
import cats.implicits.{catsSyntaxEitherId, catsSyntaxTuple2Parallel, catsSyntaxTuple3Parallel}
import libs.EitherNelT.EitherNelT

trait User
trait Manager extends User {
  def id: Id
  def name: Name
  def email: Email
  def passwordHash: PasswordHash
}

case class Worker(id: Id, name: Name, passwordHash: PasswordHash) extends User

case class UnverifiedManager(id: Id, name: Name, email: Email, passwordHash: PasswordHash, confirmationToken: String)
  extends Manager

case class VerifiedManager(id: Id, name: Name, email: Email, passwordHash: PasswordHash)
  extends Manager


object Worker {
  def apply(name: String, password: String): EitherNelT[IO, String, Worker] = EitherT {
    Id().map { id =>
      (Name(name), PasswordHash(password))
        .parMapN(Worker(id, _, _))
    }
  }
}

object UnverifiedManager {
  def apply(name: String, email: String, password: String, confirmationToken: String): EitherNelT[IO, String, UnverifiedManager] = EitherT {
    Id().map { id =>
      (Name(name), Email(email), PasswordHash(password))
        .parMapN(UnverifiedManager(id, _, _, _, confirmationToken))
    }
  }
}

object VerifiedManager {
  def apply(name: String, email: String, password: String): EitherNelT[IO, String, VerifiedManager] = EitherT {
    Id().map { id =>
      (Name(name), Email(email), PasswordHash(password))
        .parMapN(VerifiedManager(id, _, _, _))
    }
  }
}

object Id {

  type Id = String

  def apply(): IO[Id] = {
    FUUID.randomFUUID[IO].map(_.show)
  }
}

object Name {

  type Name = String

  def apply(x: String): EitherNel[String, Name] = {
    x match {
      case c if c.length > 50 | c.length < 2 => "Name length must be less 50 and more 1".leftNel
      case _ =>  x.rightNel
    }
  }
}

object PasswordHash {

  type PasswordHash = String

  def apply(password: String): EitherNel[String, PasswordHash] = {
    password match {
      case c if c.length > 50 | c.length < 5 => "Password length must be less 50 and more 4".leftNel
      case _ =>  BCrypt.hashpw(password, BCrypt.gensalt()).rightNel
    }
  }

  def checkPassword(password: String, hash: PasswordHash): Boolean = {
    BCrypt.checkpw(password, hash)
  }
}

object Email {

  type Email = String

  def apply(email: String): EitherNel[String, Email] = {
    email match {
      case c if """(\w+)@([\w.]+)""".r.unapplySeq(c).isEmpty => "Email not valid".leftNel
      case _ =>  email.rightNel
    }
  }
}