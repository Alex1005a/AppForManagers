import com.google.inject.AbstractModule
import libs.{Env, ProdEnv}
import cats.effect.IO
import java.time.Clock

class Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    bind(classOf[Env[IO]]).to(classOf[ProdEnv]).asEagerSingleton()
  }
}


