package services

import cats.effect.IO
import com.typesafe.config.ConfigFactory

import java.util.Properties
import javax.mail.{Message, Session, Transport}
import javax.mail.internet.{InternetAddress, MimeMessage}

trait EmailSender[F[_]] {
  def sendEmail(toEmail: String, text: String, subject: String): F[Unit]
}

class EmailService extends EmailSender[IO] {
  private val config = ConfigFactory.load()
  private val username: String = config.getString("myEmail")
  private val password: String = config.getString("myEmailPassword")

  def sendEmail(toEmail: String, text: String, subject: String): IO[Unit] = IO {
    val prop = new Properties
    prop.put("mail.smtp.host", "smtp.gmail.com")
    prop.put("mail.smtp.port", "587")
    prop.put("mail.smtp.auth", "true")
    prop.put("mail.smtp.starttls.enable", "true")

    val session = Session.getDefaultInstance(prop)

    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(username))
    message.setRecipient(
      Message.RecipientType.TO,
      new InternetAddress(toEmail)
    )
    message.setSubject(subject)
    message.setText(text)

    Transport.send(message, username, password)
  }
}
