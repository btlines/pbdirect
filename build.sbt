lazy val pbdirect = project
  .in(file("."))
  .settings(name := "pbdirect")

// check for library updates whenever the project is [re]load
// format: OFF
onLoad in Global := { s => "dependencyUpdates" :: s }
// format: ON
