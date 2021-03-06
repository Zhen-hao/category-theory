name := "category-theory"

version := "0.1"

scalaVersion := "2.12.4"


scalacOptions ++= Seq(
    "-Ypartial-unification",
    "-feature"
)


libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "1.0.0-RC1"
)