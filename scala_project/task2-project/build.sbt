libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

resolvers += "Central" at "http://central.maven.org/maven2/"

libraryDependencies ++= Seq(
    "com.typesafe.akka" % "akka-actor_2.12" % "2.4.20"
)
