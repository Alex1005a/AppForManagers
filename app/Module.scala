import com.google.inject.AbstractModule
import libs.{Env, ProdEnv}

import java.time.Clock

class Module extends AbstractModule {
  override def configure(): Unit = {
    bind(classOf[Clock]).toInstance(Clock.systemDefaultZone)

    bind(classOf[Env]).to(classOf[ProdEnv]).asEagerSingleton()
  }
}


