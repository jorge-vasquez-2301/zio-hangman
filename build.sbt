val scalaVer = "2.13.4"

val zioVersion = "1.0.4"

lazy val compileDependencies = Seq(
  "dev.zio" %% "zio" % zioVersion
) map (_ % Compile)

lazy val settings = Seq(
  name := "zio-hangman",
  version := "1.0.0",
  scalaVersion := scalaVer,
  libraryDependencies ++= compileDependencies
)

lazy val root = (project in file("."))
  .settings(settings)
