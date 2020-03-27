pgpPassphrase := sys.env.get("PGP_PASSPHRASE").map(_.toCharArray)

lazy val pbdirect = project
  .in(file("."))
  .settings(name := "pbdirect")

addCommandAlias("ci-test", "scalafmtCheck; scalafmtSbtCheck; +test")
