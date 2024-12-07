import mill._
import mill.scalalib._
import $ivy.`com.goyeau::mill-scalafix::0.4.2`
import com.goyeau.mill.scalafix.ScalafixModule
import scalafmt._

object aoc2024 extends ScalaModule with ScalafixModule with ScalafmtModule {
  def scalaVersion  = "3.5.2"
  def scalacOptions = Seq("-Wunused:all")

  def ivyDeps = Agg(
    ivy"dev.zio::zio:2.1.13",
    ivy"dev.zio::zio-streams:2.1.13",
    ivy"com.lihaoyi::fastparse:3.1.1",
    ivy"com.lihaoyi::os-lib:0.11.3",
    ivy"org.scala-graph::graph-core:2.0.2"
  )

  object test extends ScalaTests with TestModule.ZioTest {
    override def ivyDeps = Agg(
      ivy"dev.zio::zio-test:2.1.13",
      ivy"dev.zio::zio-test-sbt:2.1.13",
      ivy"dev.zio::zio-test-magnolia:2.1.13"
    )
  }
}
