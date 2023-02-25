
val sharedSettings = Seq(
  scalaVersion := "3.2.2"
)
val agent = project
  .settings(
    sharedSettings,
    packageOptions in (Compile, packageBin) += 
     Package.ManifestAttributes( "Premain-Class" -> "agent.Agent" )
  )

val bench = project
  .dependsOn(agent)
  .settings(
    sharedSettings,
    fork in run := true,

    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.0",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0-M1",
    javaOptions in run += ("-javaagent:" + (packageBin in (agent, Compile)).value)
)
