package weaver.docs

sealed trait CatsEffect
case object CE2 extends CatsEffect
case object CE3 extends CatsEffect

case class Artifact(
    name: String,
    jvm: Boolean,
    js: Boolean,
    scalaVersion: String,
    catsEffect: CatsEffect,
    version: String
)

case class Cell(
    jvm: Seq[String],
    js: Seq[String],
    version: String
)

case class Row(
    name: String,
    ce2: Option[Cell],
    ce3: Option[Cell]
)

case class Table(
    name: String,
    rows: Vector[Row]
) {
  def _row(l: Seq[String], header: Boolean = false): String = {
    val base = l.mkString("|", "|", "|")

    if (header) base + "\n" + _row(l.map(_ => "---"), false)
    else base + "\n"
  }

  def _cell(c: Option[Cell]): String = {
    c match {
      case Some(c) =>
        val allVersions = (c.jvm.toSet ++ c.js.toSet).map {
          case ver if ver.startsWith("2.") =>
            ver.split("\\.").take(2).mkString(".")
          case other => other
        }.toList.sorted

        if (c.jvm.nonEmpty && c.js.nonEmpty)
          s"✅ Scala ${allVersions.mkString(", ")}"
        else
          "❌"
      case None => "❌"
    }
  }

  def render(
      catsEffect3Version: String,
      ce2ArtifactsVersion: String,
      ce3ArtifactsVersion: String) = {
    val sb = new StringBuilder
    sb.append(_row(
      Seq(
        name,
        s"Cats Effect 2 <br/><br/> Weaver version: `$ce2ArtifactsVersion`",
        s"Cats Effect $catsEffect3Version <br/><br/> Weaver version: `$ce3ArtifactsVersion`"
      ),
      header = true
    ))

    rows.map { case Row(name, ce2, ce3) =>
      sb.append(_row(Seq(name, _cell(ce2), _cell(ce3))))
    }
    sb.result()
  }
}

object Table {
  def row_name(artif: String) = artif match {
    case "cats"       => "Cats-Effect"
    case "zio"        => "ZIO"
    case "monix"      => "Monix"
    case "monix-bio"  => "Monix BIO"
    case "scalacheck" => "ScalaCheck"
    case "specs2"     => "Specs2 matchers"
    case "discipline" => "Discipline law testing"
    case _            => throw new RuntimeException("Not another effect type!")
  }

  def artifactsToCell(artifacts: List[Artifact]): Option[Cell] = {
    artifacts.map(_.version).distinct.headOption.map { version =>
      val jvmVersions = artifacts.filter(_.jvm).map(_.scalaVersion)
      val jsVersions  = artifacts.filter(_.js).map(_.scalaVersion)

      Cell(
        jvm = jvmVersions,
        js = jsVersions,
        version = version
      )
    }
  }

  def create(name: String, artifacts: List[Artifact]): Table = {

    val grouped = artifacts.groupBy(_.name)

    val rows = grouped.map {
      case (name, artifacts) =>
        val rowName = row_name(name)

        val ce2Artifacts = artifacts.filter(_.catsEffect == CE2)
        val ce3Artifacts = artifacts.filter(_.catsEffect == CE3)

        Row(rowName,
            artifactsToCell(ce2Artifacts),
            artifactsToCell(ce3Artifacts))
    }

    Table(
      name,
      rows.toVector.sortBy(_.name)
    )
  }
}
