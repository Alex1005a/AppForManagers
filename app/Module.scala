import auth.{Authentication, JWTAuthentication}
import com.google.inject.AbstractModule
import repositories.{UserInMemoryRepository, UserRepository}

import java.time.Clock

class Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    bind(classOf[Authentication]).to(classOf[JWTAuthentication])

    bind(classOf[UserRepository]).to(classOf[UserInMemoryRepository])
  }

}
