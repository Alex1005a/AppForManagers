import com.google.inject.AbstractModule
import cats.effect.IO
import java.time.Clock
import repositories._
import libs._
import auth._
import services.{EmailService, EmailSender}

class Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    bind(classOf[InviteRepository[IO]]).to(classOf[InviteInMemoryRepository])
    bind(classOf[UserRepository[IO]]).to(classOf[UserInMemoryRepository])

    bind(classOf[Random[IO]]).to(classOf[RandomIO])

    bind(classOf[Process[IO]]).to(classOf[ProcessIO])
    
    bind(classOf[Authentication]).to(classOf[JWTAuthentication])

    bind(classOf[EmailSender[IO]]).to(classOf[EmailService])
  }
}


