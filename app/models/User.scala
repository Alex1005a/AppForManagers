package models


import cats.data.{EitherNel, EitherT}
import cats.effect.IO
import java.util.UUID
import models.Email.Email
import models.Id.Id
import models.Name.Name
import models.PasswordHash.PasswordHash
import org.mindrot.jbcrypt.BCrypt
import cats.implicits._
import cats.data.ValidatedNec

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

  def apply(name: String, password: String, seed: Array[Byte]): ValidatedNec[String, Worker] =  {
    (Name(name), PasswordHash(password))
        .mapN(Worker(Id(seed), _, _, Array.empty))
  }
}

object UnverifiedManager {
  private def apply(id: Id, name: Name, email: Email, passwordHash: PasswordHash, confirmationToken: String): UnverifiedManager = {
    new UnverifiedManager(id, name, email, passwordHash, confirmationToken)
  }

  def apply(name: String, email: String, password: String, confirmationToken: String, seed: Array[Byte]): ValidatedNec[String, UnverifiedManager] = {
    (Name(name), Email(email), PasswordHash(password))
        .mapN(UnverifiedManager(Id(seed), _, _, _, confirmationToken))
  }
}

object VerifiedManager {
   private def apply(id: Id, name: Name, email: Email, passwordHash: PasswordHash, workers: Array[Id]): VerifiedManager = {
    new VerifiedManager(id, name, email, passwordHash, workers)
  }

  def apply(name: String, email: String, password: String, seed: Array[Byte]): ValidatedNec[String, VerifiedManager] = {
    (Name(name), Email(email), PasswordHash(password))
        .mapN(VerifiedManager(Id(seed), _, _, _, Array.empty))
  }
}

object Id {

  type Id = String

  def apply(seed: Array[Byte]): Id = {
    UUID.nameUUIDFromBytes(seed).toString()
  }
}

object Name {

  type Name = String

  def apply(x: String): ValidatedNec[String, Name] = {
    x match {
      case c if c.length > 50 | c.length < 2 => "Name length must be less 50 and more 1".invalidNec 
      case _ =>  x.validNec 
    }
  }
}

object PasswordHash {

  type PasswordHash = String

  def apply(password: String): ValidatedNec[String, PasswordHash] = {
    password match {
      case c if c.length > 50 | c.length < 5 => "Password length must be less 50 and more 4".invalidNec
      case _ =>  BCrypt.hashpw(password, BCrypt.gensalt()).validNec
    }
  }

  def checkPassword(password: String, hash: PasswordHash): Boolean = {
    BCrypt.checkpw(password, hash)
  }
}

object Email {

  type Email = String

  def apply(email: String): ValidatedNec[String, Email] = {
    email match {
      case c if """(\w+)@([\w.]+)""".r.unapplySeq(c).isEmpty => "Email not valid".invalidNec
      case _ =>  email.validNec
    }
  }
}