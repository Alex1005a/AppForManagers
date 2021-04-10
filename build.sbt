name := "AppForManagers"
 
version := "1.0" 
      
lazy val `appformanagers` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "https://repo.akka.io/snapshots/"

scalacOptions += "-Ypartial-unification"
scalaVersion := "2.12.2"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice )
libraryDependencies += "org.scalamock" %% "scalamock" % "4.4.0" % Test
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
libraryDependencies ++= Seq( "com.jason-goodwin" %% "authentikat-jwt" % "0.4.5" , "org.typelevel" %% "cats-effect-laws" % "2.3.1" , "org.scalatestplus.play" %% "scalatestplus-play" % "5.1.0" % "test", "org.mindrot" % "jbcrypt" % "0.4" )
libraryDependencies += "javax.mail" % "mail" % "1.5.0-b01"
libraryDependencies += "io.chrisdavenport" %% "fuuid" % "0.5.0"
libraryDependencies += "com.codecommit" %% "cats-effect-testing-scalatest" % "0.5.2" % Test
unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )  

      