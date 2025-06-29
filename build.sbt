name := "smartdox"

organization := "org.smartdox"

version := "2.2.8"

scalaVersion := "2.12.18"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

javacOptions ++= Seq("--release", "21")

// incOptions := incOptions.value.withNameHashing(true)

// scalaz-stream
// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

// goldenport-java-lib-0.1.4
resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

// resolvers += "GitHab releases 2021" at "https://raw.github.com/asami/maven-repository/2021/releases"

// resolvers += "GitHab releases 2022" at "https://raw.github.com/asami/maven-repository/2022/releases"

// resolvers += "GitHab releases 2023" at "https://raw.github.com/asami/maven-repository/2023/releases"

// resolvers += "GitHab releases 2024" at "https://raw.github.com/asami/maven-repository/2024/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2025/releases"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// =======
// // libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// // libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.2.8"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "2.0.0"

// libraryDependencies += "io.circe" %% "circe-generic-extras" % "0.14.3"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.4" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0"

// // libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"
// >>>>>>> origin/master

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "5.10.0.202012080955-r"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.14"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.1.1" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

lazy val exportClasspath = taskKey[Unit]("Export full classpath to a file")

exportClasspath := {
  val cp = (Compile / fullClasspath).value.files
  val out = (Compile / target).value / "classpath.txt"
  IO.write(out, cp.mkString(":"))
  println(s"Classpath written to: $out")
}

Compile / mainClass := Some("org.smartdox.service.SmartDoxService")

// publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
