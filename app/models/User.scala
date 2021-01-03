package models

import models.Email.Email
import models.Id.Id
import models.Name.Name
import models.PasswordHash.PasswordHash
import org.mindrot.jbcrypt.BCrypt
import java.util.UUID

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
  def apply(name: String, password: String): Either[String, Worker] = {
    for {
      name <- Name(name)
      hash <- PasswordHash(password)
    } yield Worker(Id(), name, hash)
  }
}

object UnverifiedManager {
  def apply(name: String, email: String, password: String, confirmationToken: String): Either[String, UnverifiedManager] = {
    for {
      name <- Name(name)
      email <- Email(email)
      hash <- PasswordHash(password)
    } yield UnverifiedManager(Id(), name, email, hash, confirmationToken)
  }
}

object VerifiedManager {
  def apply(name: String, email: String, password: String): Either[String, VerifiedManager] = {
    for {
      name <- Name(name)
      email <- Email(email)
      hash <- PasswordHash(password)
    } yield VerifiedManager(Id(), name, email, hash)
  }
}

object Id {

  type Id = String

  def apply(): Id = {
    UUID.randomUUID().toString
  }
}

object Name {

  type Name = String

  def apply(x: String): Either[String, Name] = {
    x match{
      case c if c.length > 50 | c.length < 2 => Left("Name length must be less 50 and more 1")
      case _ =>  Right(x.asInstanceOf[Name])
    }
  }
}

object PasswordHash {

  type PasswordHash = String

  def apply(password: String): Either[String, PasswordHash] = {
    password match{
      case c if c.length > 50 | c.length < 5 => Left("Password length must be less 50 and more 4")
      case _ =>  Right(BCrypt.hashpw(password, BCrypt.gensalt()))
    }
  }

  def checkPassword(password: String, hash: PasswordHash): Boolean ={
    BCrypt.checkpw(password, hash)
  }
}

object Email {

  type Email = String

  def apply(email: String): Either[String, Email] = {
    email match{
      case c if """(\w+)@([\w.]+)""".r.unapplySeq(c).isEmpty => Left("Email not valid")
      case _ =>  Right(email)
    }
  }
}