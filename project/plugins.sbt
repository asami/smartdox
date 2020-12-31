resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/groups/scala-tools/"

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.4.0")

addSbtPlugin("org.ensime" % "ensime-sbt-cmd" % "0.1.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.16")

addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.5.0")
