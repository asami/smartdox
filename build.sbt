name := "smartdox"

organization := "org.smartdox"

version := "2.4.16"

scalaVersion := "2.12.18"

lazy val Master = config("master").extend(Compile)

ivyConfigurations += Master

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

resolvers += "SimpleModeling.org" at "https://www.simplemodeling.org/repository/maven"

// resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// =======
// // libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// // libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.3.30"

libraryDependencies += "org.goldenport" %% "goldenport-record" % "2.2.5"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "2.0.0"

// libraryDependencies += "io.circe" %% "circe-generic-extras" % "0.14.3"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.4" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0"

libraryDependencies += "org.apache.poi" % "poi-ooxml" % "3.12"

// // libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"
// >>>>>>> origin/master

libraryDependencies += "org.jsoup" % "jsoup" % "1.18.1"

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "5.10.0.202012080955-r"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.14"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test"

libraryDependencies += "org.scalatestplus" %% "junit-4-13" % "3.2.19.0" % Test

libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0" % Test // ScalaCheck 1.17 用

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0" % Test

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

publishTo := {
  val repo = sys.env.get("SIMPLEMODELING_MAVEN_LOCAL")
    .map(file)
    .getOrElse(baseDirectory.value / "maven-local")
  Some(Resolver.file("local-simplemodeling-maven", repo))
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true

Compile / packageDoc / publishArtifact := false
