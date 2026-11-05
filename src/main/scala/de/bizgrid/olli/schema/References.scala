package de.bizgrid.olli.schema

import java.net.URI
import scala.util.Try

/** Ein `$ref` mit seiner Fundstelle (JSON Pointer) und dem aufgeloesten absoluten Ziel. */
final case class Reference(pointer: String, raw: String, documentUrl: String, fragment: Option[String])

/**
 * Extraktion der Referenzziele eines Schemas (ADR-0062). Grundlage fuer Aufloesbarkeit, Reverse-Index
 * (`schema:refby`) und die Released-Pruefung.
 *
 * Verfolgt werden `$ref` und `$dynamicRef` relativ zur jeweils geltenden Basis-URI; eingebettete
 * Ressourcen mit eigenem `$id` verschieben die Basis. Werte unter `const`, `enum`, `default` und
 * `examples` sind Daten und werden nicht durchsucht.
 */
object References {
  private val RefKeywords = Set("$ref", "$dynamicRef")
  private val DataKeywords = Set("const", "enum", "default", "examples")

  def extract(document: ujson.Value, baseUrl: String): Either[Seq[Problem], Seq[Reference]] = {
    val found = Seq.newBuilder[Reference]
    val problems = Seq.newBuilder[Problem]

    def walk(value: ujson.Value, base: String, path: Vector[String]): Unit = value match {
      case obj: ujson.Obj =>
        val localBase = obj.value.get("$id").flatMap(_.strOpt).flatMap(id => resolve(base, id)).map(stripFragment).getOrElse(base)
        obj.value.foreach {
          case (key, ujson.Str(raw)) if RefKeywords(key) =>
            resolve(localBase, raw) match {
              case Some(absolute) =>
                val (document, fragment) = split(absolute)
                found += Reference(Json.pointer(path :+ key), raw, document, fragment)
              case None =>
                problems += Problem("invalid-reference", s"$key '$raw' is not a valid URI reference", Some(Json.pointer(path :+ key)))
            }
          case (key, _) if DataKeywords(key) => ()
          case (key, child) => walk(child, localBase, path :+ key)
        }
      case arr: ujson.Arr => arr.value.zipWithIndex.foreach((child, index) => walk(child, base, path :+ index.toString))
      case _ => ()
    }

    walk(document, stripFragment(baseUrl), Vector.empty)
    val result = problems.result()
    if (result.nonEmpty) Left(result) else Right(found.result())
  }

  /** `$id`-Werte eingebetteter Ressourcen; Referenzen auf sie bleiben im Dokument. */
  def embeddedIds(document: ujson.Value, baseUrl: String): Set[String] = {
    val ids = Set.newBuilder[String]
    def walk(value: ujson.Value, base: String): Unit = value match {
      case obj: ujson.Obj =>
        val localBase = obj.value.get("$id").flatMap(_.strOpt).flatMap(id => resolve(base, id)).map(stripFragment).getOrElse(base)
        ids += localBase
        obj.value.foreach {
          case (key, _) if DataKeywords(key) => ()
          case (_, child) => walk(child, localBase)
        }
      case arr: ujson.Arr => arr.value.foreach(walk(_, base))
      case _ => ()
    }
    walk(document, stripFragment(baseUrl))
    ids.result()
  }

  /** Absolute Dokument-URLs ausserhalb des Dokuments selbst. */
  def externalTargets(document: ujson.Value, baseUrl: String): Either[Seq[Problem], Seq[String]] = {
    val own = embeddedIds(document, baseUrl) + stripFragment(baseUrl)
    extract(document, baseUrl).map(_.map(_.documentUrl).filterNot(own).distinct)
  }

  def resolve(base: String, reference: String): Option[String] =
    Try(URI.create(base).resolve(URI.create(reference.trim)).normalize().toString).toOption

  def stripFragment(value: String): String = value.indexOf('#') match {
    case -1 => value
    case index => value.substring(0, index)
  }

  private def split(value: String): (String, Option[String]) = value.indexOf('#') match {
    case -1 => (value, None)
    case index => (value.substring(0, index), Some(value.substring(index + 1)).filter(_.nonEmpty))
  }

  /** Setzt `$id` auf die abgeleitete URL (ADR-0059); das `schema`-Property der Entity hat Vorrang. */
  def withId(document: ujson.Value, url: String): ujson.Value = document match {
    case obj: ujson.Obj =>
      val copy = ujson.Obj("$id" -> url)
      obj.value.foreach { case (key, value) => if (key != "$id") copy(key) = value }
      copy
    case other => other
  }
}
