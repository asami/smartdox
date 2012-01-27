name := "smartdox"

organization := "org.smartdox"

version := "0.2.1"

scalaVersion := "2.9.1"

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "6.0.3"

libraryDependencies += "org.goldenport" %% "scalazlib" % "0.1.1"

// libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1" % "test"

libraryDependencies += "org.goldenport" %% "scalatestlib" % "0.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.8" % "test"

publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
