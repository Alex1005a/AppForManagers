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

trait User {
  def id: Id
  def name: Name
  def passwordHash: PasswordHash
}
trait Manager extends User {
  def id: Id
  def name: Name
  def email: Email
  def passwordHash: PasswordHash
}

case class Worker private (id: Id, name: Name, passwordHash: PasswordHash, managers: Array[Id]) extends User

case class UnverifiedManager private (id: Id, name: Name, email: Email, passwordHash: PasswordHash, confirmationToken: String)
  extends Manager

case class VerifiedManager private (id: Id, name: Name, email: Email, passwordHash: PasswordHash, workers: Array[Id])
  extends Manager


object Worker {
  private def apply(id: Id, name: Name, passwordHash: PasswordHash, managers: Array[Id]): Worker = {
    new Worker(id, name, passwordHash, Array.empty)
  }

  def apply(name: String, password: String): EitherNelT[IO, String, Worker] = EitherT {
    Id().map { id =>
      (Name(name), PasswordHash(password))
        .parMapN(new Worker(id, _, _, Array.empty))
    }
  }
}

object UnverifiedManager {
  private def apply(id: Id, name: Name, email: Email, passwordHash: PasswordHash, confirmationToken: String): UnverifiedManager = {
    new UnverifiedManager(id, name, email, passwordHash, confirmationToken)
  }

  def apply(name: String, email: String, password: String, confirmationToken: String): EitherNelT[IO, String, UnverifiedManager] = EitherT {
    Id().map { id =>
      (Name(name), Email(email), PasswordHash(password))
        .parMapN(new UnverifiedManager(id, _, _, _, confirmationToken))
    }
  }
}

object VerifiedManager {
   private def apply(id: Id, name: Name, email: Email, passwordHash: PasswordHash, workers: Array[Id]): VerifiedManager = {
    new VerifiedManager(id, name, email, passwordHash, workers)
  }

  def apply(name: String, email: String, password: String): EitherNelT[IO, String, VerifiedManager] = EitherT {
    Id().map { id =>
      (Name(name), Email(email), PasswordHash(password))
        .parMapN(new VerifiedManager(id, _, _, _, Array.empty))
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