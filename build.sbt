name := "discrete-math"

version := "1.0"

scalaVersion := "2.11.7"

assemblyJarName in assembly := "app.jar"
mainClass in assembly := Some("by.bsuir.dm.App")

libraryDependencies += "net.sourceforge.jeval" % "jeval" % "0.9.4"
libraryDependencies += "com.github.wookietreiber" % "scala-chart_2.11" % "0.5.0"

