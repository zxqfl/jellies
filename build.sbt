name := "jellies"
// scalaVersion in ThisBuild := "2.12.2"

lazy val commonSettings = Seq(
	name := "jellies",
	version := "0.1.0",
	// libraryDependencies += "com.lihaoyi" %%% "fastparse" % "0.4.1",
	libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1",
	libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test",
	scalacOptions += "-feature"
	)

lazy val projJVM = project.in(file(".")).
	settings(
		EclipseKeys.skipProject := true,
		isScalaJSProject := false,
		scalaVersion := "2.12.0"
	).
	settings(commonSettings: _*)

lazy val projJS = project.in(file(".")).
	enablePlugins(ScalaJSPlugin).
	settings(commonSettings: _*).
	settings(
		unmanagedSourceDirectories in Compile += baseDirectory.value / "js/src/main/scala",
		unmanagedResourceDirectories in Compile += baseDirectory.value / "js/src/main/resources",
		libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1",
		scalaVersion := "2.12.0"
	)
