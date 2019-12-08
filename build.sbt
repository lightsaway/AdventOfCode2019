name := "AdventOfCode2019"
version := "0.1"
scalaVersion := "2.13.1"

scalafixDependencies in ThisBuild += "com.nequissimus" %% "sort-imports" % "0.2.1"

addCompilerPlugin(scalafixSemanticdb)
scalacOptions ++= Seq(
  "-Yrangepos", // required by SemanticDB compiler plugin
  "-Ywarn-unused" // required for RemoveUnused
)



libraryDependencies += "org.typelevel" %% "cats-effect" % "2.0.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"
libraryDependencies += "co.fs2" %% "fs2-core" % "2.1.0"
libraryDependencies += "co.fs2" %% "fs2-io" % "2.1.0"

def addCommandsAlias(name: String, values: List[String]) =
  addCommandAlias(name, values.mkString(";", ";", ""))

addCommandsAlias(
  "fix",
  List(
    "scalafix RemoveUnused",
    "scalafix SortImports"
  )
)