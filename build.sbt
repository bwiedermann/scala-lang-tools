name := "langtools"

version := "1.0"

scalaVersion := "2.11.1"

libraryDependencies ++= 
  Seq( "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1",
       "org.scalatest" % "scalatest_2.11" % "2.1.7" % "test" )