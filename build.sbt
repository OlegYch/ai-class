name := "ai-class"

sbtVersion := "0.11.2"

resolvers ++= Seq(
  "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/",
  "ScalaNLP Maven2" at "http://repo.scalanlp.org/repo",
  "Virtual-Void repository" at "http://mvn.virtual-void.net"
)

libraryDependencies += "commons-lang" % "commons-lang" % "2.5"

libraryDependencies += "org.scalala" %% "scalala" % "1.0.0.RC2"

libraryDependencies += "org.scalatest" %% "scalatest" % "1.6.1"

libraryDependencies += "junit" % "junit" % "4.8.1" % "compile"

autoCompilerPlugins := true

libraryDependencies += "net.virtualvoid" %% "scala-enhanced-strings" % "0.5.2" % "compile;plugin->default(compile)"

libraryDependencies += "org.apache.poi" % "poi" % "3.8-beta5"

libraryDependencies += "org.springframework" % "spring-core" % "3.1.0.RELEASE"
