import org.openurp.parent.Settings.*

ThisBuild / organization := "org.openurp.edu"
ThisBuild / version := "0.0.2-SNAPSHOT"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://gitee.com/xnurp/edu-extern"),
    "scm:git@gitee.com:xurp/edu-extern.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id = "chaostone",
    name = "Tihua Duan",
    email = "duantihua@gmail.com",
    url = url("http://github.com/duantihua")
  )
)

ThisBuild / description := "OpenURP Edu Cert"
ThisBuild / homepage := Some(url("http://openurp.github.io/edu-extern/index.html"))


val commons = "org.beangle.commons" % "beangle-commons" % "6.0.15"
lazy val webapp = (project in file("."))
  .settings(
    name := "openurp-edu-cert",
    common,
    libraryDependencies ++= Seq(commons)
  )
