name := "parchment"

version := "0.0.17-SNAPSHOT"

organization := "me.yuhuan"

scalaVersion := "2.11.7"

publishMavenStyle := true

isSnapshot := true

scalacOptions in (Compile, doc) += "-diagrams"

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

//region Custom Dependencies
libraryDependencies += "edu.stanford.nlp" % "stanford-parser" % "3.5.2"
libraryDependencies += "edu.stanford.nlp" % "stanford-parser" % "3.5.2" classifier "models"
libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2"
libraryDependencies += "edu.stanford.nlp" % "stanford-corenlp" % "3.5.2" classifier "models"

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "me.yuhuan" %% "marauder"               % "0.0.6-SNAPSHOT"
libraryDependencies += "me.yuhuan" %% "reparo"                 % "0.0.0-SNAPSHOT"
libraryDependencies += "me.yuhuan" %% "debugview-client-scala" % "0.0.1-SNAPSHOT"
//endregion


pomExtra :=
  <url>https://github.com/jyuhuan/parchment</url>
    <licenses>
      <license>
        <name>MIT</name>
        <url>http://opensource.org/licenses/MIT</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:jyuhuan/parchment.git</url>
      <connection>scm:git:git@github.com:jyuhuan/parchment.git</connection>
    </scm>
    <developers>
      <developer>
        <id>yuhuan</id>
        <name>Yuhuan Jiang</name>
        <url>http://yuhuan.me/</url>
      </developer>
    </developers>


