name := "smartdox"

organization := "org.smartdox"

version := "1.3.13-SNAPSHOT"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

// scalaz-stream
// resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "GitHab releases 2020" at "https://raw.github.com/asami/maven-repository/2020/releases"

resolvers += "GitHab releases 2021" at "https://raw.github.com/asami/maven-repository/2021/releases"

resolvers += "GitHab releases 2022" at "https://raw.github.com/asami/maven-repository/2022/releases"

resolvers += "GitHab releases" at "https://raw.github.com/asami/maven-repository/2024/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.4.0-SNAPSHOT"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "1.1.0"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" exclude("org.scala-stm", "scala-stm_2.10.0")

// libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

// publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))

val mavenrepo = settingKey[String]("mavenrepo")

mavenrepo := sys.env.getOrElse("PUBLISH_MAVEN_REPO", default = "target/maven-repository")

publishTo <<= mavenrepo { v: String =>
  Some(Resolver.file("file", file(v)))
}
