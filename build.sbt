name := "smartdox"

organization := "org.smartdox"

version := "2.0.0"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.0.0"

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "2.0.0"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.0.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
