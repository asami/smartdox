name := "smartdox"

organization := "org.smartdox"

version := "0.3.0-SNAPSHOT"

// scalaVersion := "2.9.1"

crossScalaVersions := Seq("2.9.2", "2.9.1")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.4"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "0.2.0"

// libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "0.2.0" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
