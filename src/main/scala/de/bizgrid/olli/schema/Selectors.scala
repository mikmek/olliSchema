package de.bizgrid.olli.schema

import scala.util.Try

/** Ein Selektor eines Meta-Schemas mit seiner Fundstelle im Meta-Schema-Dokument. */
final case class Selector(expression: String, location: String, kind: Selector.Kind)

object Selector {
  enum Kind { case JsonPath, JsonPointer }
}

/** Ein Schritt eines Selektors, auf das Instanzmodell bezogen. */
private[schema] enum Step {
  case Name(name: String)
  case Index(index: Int)
  /** `*`, Filter `[?…]` und Slices: irgendein Kind. */
  case AnyChild
  /** `..name`: irgendein Nachfahre mit diesem Namen. */
  case Descendant(name: Option[String])
  /** JSON-Pointer-Segment, das Name oder Index sein kann. */
  case NameOrIndex(segment: String)
}

/**
 * Selektoren der Meta-Schemas (ADR-0065): JSONPath nach RFC 9535 fuer Selektionen mit Wildcards und
 * Filtern, JSON Pointer nach RFC 6901 fuer konkrete Orte.
 *
 * Geprueft wird nur, ob ein Selektor im Zielschema existieren kann: jeder benannte Schritt muss in
 * `properties` (oder passend in `patternProperties` bzw. einem Schema unter `additionalProperties`)
 * deklariert sein. Filterausdruecke werden nicht ausgewertet, sie selektieren Kinder.
 */
object Selectors {
  private val WholePath = "^\\$(?:[.\\[].*)?$".r
  private val Template = "\\{(\\$[^}]*)\\}".r
  private val MaxNodes = 10000

  /** Alle Selektoren eines Meta-Schema-Dokuments. */
  def extract(document: ujson.Value): Seq[Selector] = {
    val found = Seq.newBuilder[Selector]
    def walk(value: ujson.Value, path: Vector[String]): Unit = value match {
      case obj: ujson.Obj => obj.value.foreach {
        case ("pointer", ujson.Str(pointer)) if pointer.isEmpty || pointer.startsWith("/") =>
          found += Selector(pointer, Json.pointer(path :+ "pointer"), Selector.Kind.JsonPointer)
        case (key, child) => walk(child, path :+ key)
      }
      case arr: ujson.Arr => arr.value.zipWithIndex.foreach((child, index) => walk(child, path :+ index.toString))
      case ujson.Str(text) =>
        if (WholePath.matches(text.trim)) found += Selector(text.trim, Json.pointer(path), Selector.Kind.JsonPath)
        else Template.findAllMatchIn(text).foreach(m => found += Selector(m.group(1).trim, Json.pointer(path), Selector.Kind.JsonPath))
      case _ => ()
    }
    walk(document, Vector.empty)
    found.result()
  }

  /** Zerlegt einen JSONPath (RFC 9535) in Schritte. */
  def parseJsonPath(expression: String): Either[String, Seq[Step]] = {
    val text = expression.trim
    if (!text.startsWith("$")) return Left("JSONPath must start with '$'")
    val steps = Seq.newBuilder[Step]
    var index = 1
    def name(start: Int): (String, Int) = {
      var end = start
      while (end < text.length && (text(end).isLetterOrDigit || text(end) == '_' || text(end) > 0x7f)) end += 1
      (text.substring(start, end), end)
    }
    while (index < text.length) {
      text(index) match {
        case '.' if text.startsWith("..", index) =>
          if (text.startsWith("..*", index)) { steps += Step.Descendant(None); index += 3 }
          else if (text.startsWith("..[", index)) {
            bracket(text, index + 2) match {
              case Left(problem) => return Left(problem)
              case Right((selectors, next)) =>
                steps += Step.Descendant(selectors.collectFirst { case Step.Name(value) => value })
                index = next
            }
          } else {
            val (value, next) = name(index + 2)
            if (value.isEmpty) return Left(s"Expected a member name at position ${index + 2}")
            steps += Step.Descendant(Some(value)); index = next
          }
        case '.' =>
          if (text.startsWith(".*", index)) { steps += Step.AnyChild; index += 2 }
          else {
            val (value, next) = name(index + 1)
            if (value.isEmpty) return Left(s"Expected a member name at position ${index + 1}")
            steps += Step.Name(value); index = next
          }
        case '[' =>
          bracket(text, index) match {
            case Left(problem) => return Left(problem)
            case Right((selectors, next)) =>
              steps += (if (selectors.size == 1) selectors.head else Step.AnyChild)
              index = next
          }
        case other => return Left(s"Unexpected '$other' at position $index")
      }
    }
    Right(steps.result())
  }

  /** Liest einen Klammerselektor ab `start` (zeigt auf '['). */
  private def bracket(text: String, start: Int): Either[String, (Seq[Step], Int)] = {
    var depth = 0
    var quote: Option[Char] = None
    var end = start
    var done = false
    while (!done && end < text.length) {
      val c = text(end)
      quote match {
        case Some(q) => if (c == '\\') end += 1 else if (c == q) quote = None
        case None =>
          if (c == '\'' || c == '"') quote = Some(c)
          else if (c == '[') depth += 1
          else if (c == ']') { depth -= 1; if (depth == 0) done = true }
      }
      if (!done) end += 1
    }
    if (!done) Left(s"Unclosed '[' at position $start")
    else {
      val inner = text.substring(start + 1, end).trim
      val parsed: Seq[Either[String, Step]] =
        if (inner.startsWith("?")) Seq(Right(Step.AnyChild))
        else splitTopLevel(inner).map(part => selector(part.trim))
      parsed.collectFirst { case Left(problem) => problem } match {
        case Some(problem) => Left(problem)
        case None => Right((parsed.collect { case Right(step) => step }, end + 1))
      }
    }
  }

  private def selector(value: String): Either[String, Step] = {
    def quoted(q: Char) = value.length >= 2 && value.head == q && value.last == q
    if (value == "*") Right(Step.AnyChild)
    else if (quoted('\'') || quoted('"'))
      Right(Step.Name(value.substring(1, value.length - 1).replace("\\'", "'").replace("\\\"", "\"")))
    else value.toIntOption match {
      case Some(number) => Right(Step.Index(number))
      case None if value.contains(':') => Right(Step.AnyChild)
      case None => Left(s"Unsupported selector '$value'")
    }
  }

  private def splitTopLevel(text: String): Seq[String] = {
    val parts = Seq.newBuilder[String]
    var quote: Option[Char] = None
    var from = 0
    text.indices.foreach { index =>
      val c = text(index)
      quote match {
        case Some(q) => if (c == q) quote = None
        case None =>
          if (c == '\'' || c == '"') quote = Some(c)
          else if (c == ',') { parts += text.substring(from, index); from = index + 1 }
      }
    }
    parts += text.substring(from)
    parts.result()
  }

  def parseJsonPointer(pointer: String): Either[String, Seq[Step]] =
    if (pointer.isEmpty) Right(Nil)
    else if (!pointer.startsWith("/")) Left("JSON Pointer must be empty or start with '/'")
    else Right(pointer.drop(1).split("/", -1).toSeq.map(segment => Step.NameOrIndex(segment.replace("~1", "/").replace("~0", "~"))))

  /**
   * Prueft alle Selektoren gegen das Zielschema. `resolve` liefert referenzierte Dokumente aus der
   * lokalen Registry.
   */
  def check(selectors: Seq[Selector], targetUrl: String, target: ujson.Value, resolve: String => Option[ujson.Value]): Seq[Problem] =
    selectors.flatMap { selector =>
      val steps = selector.kind match {
        case Selector.Kind.JsonPath => parseJsonPath(selector.expression)
        case Selector.Kind.JsonPointer => parseJsonPointer(selector.expression)
      }
      steps match {
        case Left(problem) => Seq(Problem("invalid-selector", s"${selector.expression}: $problem", Some(selector.location)))
        case Right(parsed) =>
          new Navigator(resolve).missing(SchemaNode(targetUrl, target, target), parsed) match {
            case None => Nil
            case Some(reason) => Seq(Problem("unknown-selector", s"${selector.expression}: $reason", Some(selector.location)))
          }
      }
    }

  private final case class SchemaNode(documentUrl: String, root: ujson.Value, node: ujson.Value)

  private final class Navigator(resolve: String => Option[ujson.Value]) {
    private var visited = 0

    /** Liefert den Grund, falls der Pfad im Schema nicht existieren kann. */
    def missing(start: SchemaNode, steps: Seq[Step]): Option[String] = {
      var current = Seq(start)
      var problem: Option[String] = None
      val remaining = steps.iterator
      while (problem.isEmpty && remaining.hasNext) {
        val step = remaining.next()
        val next = current.flatMap(expand).flatMap(node => children(node, step)).distinct
        if (next.isEmpty) problem = Some(s"${describe(step)} is not declared in the target schema")
        current = next
      }
      problem
    }

    private def describe(step: Step): String = step match {
      case Step.Name(name) => s"property '$name'"
      case Step.Index(index) => s"item [$index]"
      case Step.AnyChild => "child selection"
      case Step.Descendant(name) => name.fold("descendant selection")(value => s"descendant '$value'")
      case Step.NameOrIndex(segment) => s"location '$segment'"
    }

    /** Ein Knoten zusammen mit allem, was ueber $ref und Kombinatoren zu ihm gehoert. */
    private def expand(start: SchemaNode): Seq[SchemaNode] = {
      val result = Seq.newBuilder[SchemaNode]
      def visit(node: SchemaNode, depth: Int): Unit = if (depth < 32 && visited < MaxNodes) {
        visited += 1
        result += node
        node.node.objOpt.foreach { obj =>
          obj.get("$ref").flatMap(_.strOpt).flatMap(ref => follow(node, ref)).foreach(visit(_, depth + 1))
          Seq("allOf", "anyOf", "oneOf").foreach(key => obj.get(key).flatMap(_.arrOpt).foreach(_.foreach(child => visit(node.copy(node = child), depth + 1))))
          Seq("then", "else").foreach(key => obj.get(key).foreach(child => visit(node.copy(node = child), depth + 1)))
        }
      }
      visit(start, 0)
      result.result()
    }

    private def follow(node: SchemaNode, ref: String): Option[SchemaNode] = {
      val base = node.root.objOpt.flatMap(_.get("$id")).flatMap(_.strOpt).getOrElse(node.documentUrl)
      References.resolve(base, ref).flatMap { absolute =>
        val document = References.stripFragment(absolute)
        val fragment = absolute.drop(document.length).stripPrefix("#")
        val root = if (document == References.stripFragment(base) || document == node.documentUrl) Some(node.root) else resolve(document)
        root.flatMap { rootDoc =>
          val decoded = Try(java.net.URLDecoder.decode(fragment, "UTF-8")).getOrElse(fragment)
          val target =
            if (decoded.isEmpty || decoded.startsWith("/")) Json.at(rootDoc, decoded)
            else findAnchor(rootDoc, decoded)
          target.map(value => SchemaNode(document, rootDoc, value))
        }
      }
    }

    private def findAnchor(root: ujson.Value, anchor: String): Option[ujson.Value] = root match {
      case obj: ujson.Obj =>
        if (obj.value.get("$anchor").flatMap(_.strOpt).contains(anchor)) Some(obj)
        else obj.value.valuesIterator.flatMap(findAnchor(_, anchor)).nextOption()
      case arr: ujson.Arr => arr.value.iterator.flatMap(findAnchor(_, anchor)).nextOption()
      case _ => None
    }

    private def children(node: SchemaNode, step: Step): Seq[SchemaNode] = {
      val obj = node.node.objOpt
      def child(value: ujson.Value) = node.copy(node = value)
      def properties = obj.flatMap(_.get("properties")).flatMap(_.objOpt).fold(Seq.empty[(String, ujson.Value)])(_.toSeq)
      def additional = obj.flatMap(_.get("additionalProperties")).filter(_.objOpt.nonEmpty).toSeq
      def items = obj.toSeq.flatMap(o => o.get("items").toSeq ++ o.get("prefixItems").flatMap(_.arrOpt).toSeq.flatten)
      def named(name: String): Seq[SchemaNode] = {
        val declared = properties.collect { case (key, value) if key == name => value }
        val patterned = obj.flatMap(_.get("patternProperties")).flatMap(_.objOpt).toSeq.flatMap(_.toSeq).collect {
          case (pattern, value) if Try(pattern.r.findFirstIn(name).nonEmpty).getOrElse(false) => value
        }
        (declared ++ patterned ++ (if (declared.isEmpty && patterned.isEmpty) additional else Nil)).map(child)
      }
      def indexed(index: Int): Seq[SchemaNode] = obj.toSeq.flatMap { o =>
        val prefix = o.get("prefixItems").flatMap(_.arrOpt).flatMap(arr => arr.lift(index)).toSeq
        if (prefix.nonEmpty) prefix else o.get("items").toSeq
      }.map(child)
      step match {
        case Step.Name(name) => named(name)
        case Step.Index(index) => indexed(index)
        case Step.NameOrIndex(segment) => named(segment) ++ segment.toIntOption.toSeq.flatMap(indexed)
        case Step.AnyChild => (properties.map(_._2) ++ additional ++ items).map(child)
        case Step.Descendant(name) =>
          val all = Seq.newBuilder[SchemaNode]
          def descend(current: SchemaNode, depth: Int): Unit = if (depth < 16 && visited < MaxNodes) {
            expand(current).foreach { expanded =>
              val direct = name.fold(children(expanded, Step.AnyChild))(value => children(expanded, Step.Name(value)).filter(_ => hasDeclared(expanded, value)))
              all ++= direct
              children(expanded, Step.AnyChild).foreach(descend(_, depth + 1))
            }
          }
          descend(node, 0)
          all.result()
      }
    }

    private def hasDeclared(node: SchemaNode, name: String): Boolean =
      node.node.objOpt.flatMap(_.get("properties")).flatMap(_.objOpt).exists(_.contains(name))
  }
}
