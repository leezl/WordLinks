name := "NGramSearch"

version := "0.1"

scalaVersion := "2.9.1"

// Twitter's repository
resolvers += "twitter.com" at "http://maven.twttr.com/",
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"

// Add Twitter dependencies
libraryDependencies ++= Seq(
  "com.twitter" % "finagle-core_2.9.1" % "1.9.12",
  "com.twitter" % "finagle-http_2.9.1" % "1.9.12",
  "org.scalala" %% "scalala" % "1.0.0.RC3-SNAPSHOT"
)

libraryDependencies += "org.jboss.netty" % "netty" % "3.2.1.Final"