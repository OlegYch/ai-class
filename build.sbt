name := "ai-class"

resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo"
)

libraryDependencies += "commons-lang" % "commons-lang" % "2.5"

libraryDependencies += "org.scalala" %% "scalala" % "1.0.0.RC2-SNAPSHOT"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"

libraryDependencies += "junit" % "junit" % "4.8.1" % "compile"