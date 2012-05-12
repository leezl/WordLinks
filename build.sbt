name := "NGramSearch"

version := "0.1"

scalaVersion := "2.9.1"

// Twitter's repository
resolvers += "twitter.com" at "http://maven.twttr.com/"

// Add Twitter dependencies
libraryDependencies ++= Seq(
  "com.twitter" % "finagle-core_2.9.1" % "1.9.12",
  "com.twitter" % "finagle-http_2.9.1" % "1.9.12"
)

libraryDependencies += "org.jboss.netty" % "netty" % "3.2.1.Final"