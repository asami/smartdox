name := "smartdox"

organization := "org.smartdox"

version := "2.2.0"

scalaVersion := "2.12.18"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// incOptions := incOptions.value.withNameHashing(true)

// scalaz-stream
// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases 2021" at "https://raw.github.com/asami/maven-repository/2021/releases"

resolvers += "GitHab releases 2022" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2024/releases"

resolvers += "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// =======
// // libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// // libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.2.0"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "2.0.0"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" exclude("org.scala-stm", "scala-stm_2.10.0")

libraryDependencies += "com.atilika.kuromoji" % "kuromoji-ipadic" % "0.9.0"

// // libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"
// >>>>>>> origin/master

libraryDependencies += "org.eclipse.jgit" % "org.eclipse.jgit" % "5.10.0.202012080955-r"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.4.14"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

// publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))

publishTo := Some(
  "GitHub Packages" at "https://maven.pkg.github.com/asami/maven-repository"
)

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

publishMavenStyle := true
