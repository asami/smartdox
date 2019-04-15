name := "smartdox"

organization := "org.smartdox"

version := "2.1.1-SNAPSHOT"

scalaVersion := "2.12.7"

// crossScalaVersions := Seq("2.11.6", "2.10.5")

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"

scalacOptions += "-feature"

incOptions := incOptions.value.withNameHashing(true)

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

resolvers += "Asami Maven Repository" at "http://www.asamioffice.com/maven"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"

libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "2.1.1-SNAPSHOT"
// =======
// // libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.0.6"

// // libraryDependencies += "org.goldenport" % "goldenport-java-lib" % "0.1.2"

// libraryDependencies += "org.goldenport" %% "goldenport-scala-lib" % "1.2.33"

// libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "1.1.0"

// libraryDependencies += "com.typesafe.play" %% "play-json" % "2.2.2" % "provided" exclude("org.scala-stm", "scala-stm_2.10.0")

// // libraryDependencies += "net.sourceforge.nekohtml" % "nekohtml" % "1.9.15"
// >>>>>>> origin/master

libraryDependencies += "org.goldenport" %% "goldenport-scalaz-lib" % "2.1.0"

libraryDependencies += "org.goldenport" %% "goldenport-scalatest-lib" % "2.1.0" % "test"

libraryDependencies += "junit" % "junit" % "4.12" % "test"

publishTo := Some(Resolver.file("asamioffice", file("target/maven-repository")))
