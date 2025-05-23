ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.3"

lazy val root = (project in file("."))
  .settings(
    name := "projetoppm"
  )

// Garante que os recursos sejam copiados para o classpath
Compile / unmanagedResourceDirectories += baseDirectory.value / "src" / "main" / "resources"

// Para projetos JavaFX (se aplicável)
libraryDependencies += "org.openjfx" % "javafx-controls" % "17" classifier "win" // ajuste para seu OS
