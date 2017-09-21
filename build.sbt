name := "smartdox"

organization := "org.smartdox"

version := "1.2.0"

scalaVersion := "2.10.3"
// crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

// libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.0"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "1.1.0"

// libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.0" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "1.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
