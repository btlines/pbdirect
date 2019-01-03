pgpPassphrase := Some(getEnvVar("PGP_PASSPHRASE").getOrElse("").toCharArray)
pgpPublicRing := file(s"$gpgFolder/pubring.gpg")
pgpSecretRing := file(s"$gpgFolder/secring.gpg")

lazy val pbdirect = project
  .in(file("."))
  .settings(name := "pbdirect")

// check for library updates whenever the project is [re]load
// format: OFF
onLoad in Global := { s => "dependencyUpdates" :: s }
// format: ON
