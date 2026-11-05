package de.bizgrid.olli.schema

import org.apache.pekko.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import org.apache.pekko.http.scaladsl.model.{ContentType, ContentTypes, HttpCharsets, HttpEntity, MediaType}

import java.time.Instant

object Json {
  implicit val jsonMarshaller: ToEntityMarshaller[ujson.Value] =
    Marshaller.withFixedContentType(ContentTypes.`application/json`)(v => HttpEntity(ContentTypes.`application/json`, ujson.write(v)))

  val SchemaJson: MediaType.WithFixedCharset =
    MediaType.applicationWithFixedCharset("schema+json", HttpCharsets.`UTF-8`, "json")

  val SchemaJsonContentType: ContentType.WithFixedCharset = ContentType(SchemaJson)

  def error(message: String): ujson.Value = ujson.Obj("error" -> message)

  def instant(epochMillis: Long): ujson.Value = ujson.Str(Instant.ofEpochMilli(epochMillis).toString)

  /** JSON Merge Patch (RFC 7396), wie ihn `EntityPatched` im EntityIndex anwendet. */
  def mergePatch(target: ujson.Value, patch: ujson.Value): ujson.Value = patch match {
    case patchObj: ujson.Obj =>
      val result = target match {
        case obj: ujson.Obj => ujson.Obj.from(obj.value)
        case _ => ujson.Obj()
      }
      patchObj.value.foreach {
        case (key, ujson.Null) => result.value.remove(key)
        case (key, value) => result.value(key) = mergePatch(result.value.getOrElse(key, ujson.Null), value)
      }
      result
    case other => other
  }

  /** JSON Pointer (RFC 6901) aus Segmenten. */
  def pointer(segments: Seq[String]): String =
    segments.map(segment => "/" + segment.replace("~", "~0").replace("/", "~1")).mkString

  /** Wert an einem JSON Pointer innerhalb eines Dokuments. */
  def at(document: ujson.Value, pointer: String): Option[ujson.Value] =
    if (pointer.isEmpty) Some(document)
    else if (!pointer.startsWith("/")) None
    else pointer.drop(1).split("/", -1).map(_.replace("~1", "/").replace("~0", "~")).foldLeft(Option(document)) {
      case (Some(obj: ujson.Obj), segment) => obj.value.get(segment)
      case (Some(arr: ujson.Arr), segment) => segment.toIntOption.filter(index => index >= 0 && index < arr.value.size).map(arr.value(_))
      case _ => None
    }
}
