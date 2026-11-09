package de.bizgrid.olli.schema

import java.util.regex.{Pattern, PatternSyntaxException}
import scala.collection.mutable
import scala.concurrent.duration.FiniteDuration
import scala.util.Try

/** Ein Selektor eines Meta-Schemas mit seiner Fundstelle im Meta-Schema-Dokument. */
final case class Selector(expression: String, location: String, kind: Selector.Kind)

object Selector {
  enum Kind { case JsonPath, JsonPointer }
}

/** In Filterausdruecken eingebettete Abfragen: relativ zum gefilterten Kind (`@`) oder zur Wurzel (`$`). */
private[schema] final case class FilterQueries(relative: Seq[Seq[Step]], absolute: Seq[Seq[Step]])

/** Ein Schritt eines Selektors, auf das Instanzmodell bezogen. */
private[schema] enum Step {
  case Name(name: String)
  /** Index eines Array-Elements; nur auf Arrays zulaessig. */
  case Index(index: Int)
  /** Slice `start:end:step`; nur auf Arrays zulaessig. */
  case Slice
  /** `*`: jedes Kind (Properties und Array-Elemente). */
  case Wildcard
  /** `[?…]`: Kinder, deren eingebettete Abfragen im Schema des Kindes bzw. der Wurzel existieren muessen. */
  case Filter(queries: FilterQueries)
  /** Mehrfachauswahl `[a,b]`: jedes Element muss fuer sich existieren. */
  case Union(steps: Seq[Step])
  /** `..x`: der Selektor `x` auf den Knoten und alle Nachfahren. */
  case Descendant(inner: Step)
  /** JSON-Pointer-Segment, das Name oder Index sein kann. */
  case NameOrIndex(segment: String)
}

/**
 * Parser fuer JSONPath nach RFC 9535. Unzulaessige Konstrukte werden abgelehnt und nie zu einer
 * Wildcard verbreitert (SCH-A05). Filterausdruecke werden vollstaendig geparst: logische und
 * Vergleichsoperatoren, Literale, eingebettete Abfragen und die Funktionen der Spezifikation (`length`,
 * `count`, `match`, `search`, `value`) mit Stelligkeit und Grundtypen.
 */
private[schema] final class JsonPathParser(text: String) {
  private var pos = 0

  final class ParseError(message: String) extends RuntimeException(message, null, false, false)

  private def fail(message: String): Nothing = throw new ParseError(s"$message at position $pos")
  private def peek: Char = if (pos < text.length) text(pos) else '\u0000'
  private def atEnd: Boolean = pos >= text.length
  private def startsWith(token: String): Boolean = text.startsWith(token, pos)
  private def expect(token: String): Unit = if (startsWith(token)) pos += token.length else fail(s"Expected '$token'")
  private def blanks(): Unit = while (!atEnd && " \t\n\r".contains(peek)) pos += 1

  def parseQuery(): Either[String, Seq[Step]] =
    try {
      expect("$")
      val steps = segments()
      if (!atEnd) fail(s"Unexpected '$peek'")
      Right(steps)
    } catch { case error: ParseError => Left(error.getMessage) }

  /** Segmente bis zu einem Zeichen, das kein Segment beginnt; Leerraum zwischen Segmenten ist erlaubt. */
  private def segments(): Seq[Step] = {
    val steps = Seq.newBuilder[Step]
    var continue = true
    while (continue) {
      val mark = pos
      blanks()
      if (startsWith("..")) {
        pos += 2
        steps += Step.Descendant(
          if (peek == '[') bracketed()
          else if (peek == '*') { pos += 1; Step.Wildcard }
          else Step.Name(memberName()))
      } else if (peek == '.') {
        pos += 1
        steps += (if (peek == '*') { pos += 1; Step.Wildcard } else Step.Name(memberName()))
      } else if (peek == '[') steps += bracketed()
      else { pos = mark; continue = false }
    }
    steps.result()
  }

  private def isNameFirst(c: Char): Boolean = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_' || c >= 0x80
  private def isNameChar(c: Char): Boolean = isNameFirst(c) || (c >= '0' && c <= '9')

  private def memberName(): String = {
    if (atEnd || !isNameFirst(peek)) fail("Expected a member name")
    val start = pos
    while (!atEnd && isNameChar(peek)) pos += 1
    text.substring(start, pos)
  }

  private def bracketed(): Step = {
    expect("[")
    blanks()
    val selectors = mutable.ArrayBuffer(selector())
    blanks()
    while (peek == ',') {
      pos += 1
      blanks()
      selectors += selector()
      blanks()
    }
    expect("]")
    if (selectors.size == 1) selectors.head else Step.Union(selectors.toSeq)
  }

  private def selector(): Step = peek match {
    case '\'' | '"' => Step.Name(stringLiteral())
    case '*' => pos += 1; Step.Wildcard
    case '?' =>
      pos += 1
      blanks()
      val queries = mutable.ArrayBuffer.empty[FilterQueries]
      logicalExpr(queries)
      Step.Filter(FilterQueries(queries.flatMap(_.relative).toSeq, queries.flatMap(_.absolute).toSeq))
    case c if c == ':' || c == '-' || c.isDigit => indexOrSlice()
    case other => fail(s"Unsupported selector starting with '$other'")
  }

  private def integer(): Option[Long] = {
    val start = pos
    if (peek == '-') pos += 1
    if (peek == '0') {
      pos += 1
      if (text.substring(start, pos) == "-0") fail("'-0' is not a valid index")
    } else if (peek >= '1' && peek <= '9') while (peek.isDigit) pos += 1
    else { pos = start; return None }
    text.substring(start, pos).toLongOption.orElse(fail("Index out of range"))
  }

  private def indexOrSlice(): Step = {
    val start = integer()
    blanks()
    if (peek != ':') start match {
      case Some(value) if value.isValidInt => Step.Index(value.toInt)
      case Some(_) => fail("Index out of range")
      case None => fail("Expected an index or a slice")
    } else {
      pos += 1
      blanks()
      integer()
      blanks()
      if (peek == ':') {
        pos += 1
        blanks()
        integer()
      }
      Step.Slice
    }
  }

  private def stringLiteral(): String = {
    val quote = peek
    pos += 1
    val out = new StringBuilder
    while (peek != quote) {
      if (atEnd) fail("Unterminated string literal")
      val c = peek
      if (c < 0x20) fail("Control character in string literal")
      if (c == '\\') {
        pos += 1
        peek match {
          case 'b' => out += '\b'; pos += 1
          case 'f' => out += '\f'; pos += 1
          case 'n' => out += '\n'; pos += 1
          case 'r' => out += '\r'; pos += 1
          case 't' => out += '\t'; pos += 1
          case '/' => out += '/'; pos += 1
          case '\\' => out += '\\'; pos += 1
          case 'u' =>
            pos += 1
            val hex = if (pos + 4 <= text.length) text.substring(pos, pos + 4) else ""
            if (!hex.matches("[0-9a-fA-F]{4}")) fail("Invalid unicode escape")
            out += Integer.parseInt(hex, 16).toChar
            pos += 4
          case q if q == quote => out += q; pos += 1
          case other => fail(s"Invalid escape '\\$other'")
        }
      } else { out += c; pos += 1 }
    }
    pos += 1
    out.result()
  }

  // ---- Filterausdruecke -------------------------------------------------------------------------

  /** Grundtypen nach RFC 9535 Abschnitt 2.4.1. */
  private enum Kind { case Value, Logical, Nodes, SingularNodes, Literal }

  private def logicalExpr(queries: mutable.ArrayBuffer[FilterQueries]): Unit = {
    andExpr(queries)
    blanks()
    while (startsWith("||")) { pos += 2; blanks(); andExpr(queries); blanks() }
  }

  private def andExpr(queries: mutable.ArrayBuffer[FilterQueries]): Unit = {
    basicExpr(queries)
    blanks()
    while (startsWith("&&")) { pos += 2; blanks(); basicExpr(queries); blanks() }
  }

  private val ComparisonOps = Seq("==", "!=", "<=", ">=", "<", ">")

  private def basicExpr(queries: mutable.ArrayBuffer[FilterQueries]): Unit = {
    val negated = peek == '!' && !startsWith("!=")
    if (negated) { pos += 1; blanks() }
    if (peek == '(') {
      pos += 1
      blanks()
      logicalExpr(queries)
      blanks()
      expect(")")
    } else {
      val left = operand(queries)
      blanks()
      ComparisonOps.find(startsWith) match {
        case Some(op) =>
          if (negated) fail("'!' cannot negate a comparison without parentheses")
          requireComparable(left)
          pos += op.length
          blanks()
          requireComparable(operand(queries))
        case None =>
          left match {
            case Kind.Nodes | Kind.SingularNodes | Kind.Logical => ()
            case _ => fail("Expected a comparison or a test expression")
          }
      }
    }
  }

  private def requireComparable(kind: Kind): Unit = kind match {
    case Kind.Literal | Kind.SingularNodes | Kind.Value => ()
    case _ => fail("Operand is not comparable (use a literal, a singular query or a value function)")
  }

  private def operand(queries: mutable.ArrayBuffer[FilterQueries]): Kind = peek match {
    case '@' =>
      pos += 1
      val steps = segments()
      queries += FilterQueries(Seq(steps), Nil)
      if (singular(steps)) Kind.SingularNodes else Kind.Nodes
    case '$' =>
      pos += 1
      val steps = segments()
      queries += FilterQueries(Nil, Seq(steps))
      if (singular(steps)) Kind.SingularNodes else Kind.Nodes
    case '\'' | '"' => stringLiteral(); Kind.Literal
    case c if c == '-' || c.isDigit => number(); Kind.Literal
    case _ if startsWith("true") && !nameContinues(4) => pos += 4; Kind.Literal
    case _ if startsWith("false") && !nameContinues(5) => pos += 5; Kind.Literal
    case _ if startsWith("null") && !nameContinues(4) => pos += 4; Kind.Literal
    case c if c >= 'a' && c <= 'z' => function(queries)
    case other => fail(s"Unexpected '$other' in filter expression")
  }

  private def nameContinues(offset: Int): Boolean = pos + offset < text.length && {
    val c = text(pos + offset)
    c.isLetterOrDigit || c == '_'
  }

  private def singular(steps: Seq[Step]): Boolean = steps.forall {
    case Step.Name(_) | Step.Index(_) => true
    case _ => false
  }

  private def number(): Unit = {
    val start = pos
    if (peek == '-') pos += 1
    if (peek == '0') pos += 1
    else if (peek >= '1' && peek <= '9') while (peek.isDigit) pos += 1
    else fail("Invalid number")
    if (peek == '.') {
      pos += 1
      if (!peek.isDigit) fail("Invalid fraction")
      while (peek.isDigit) pos += 1
    }
    if (peek == 'e' || peek == 'E') {
      pos += 1
      if (peek == '+' || peek == '-') pos += 1
      if (!peek.isDigit) fail("Invalid exponent")
      while (peek.isDigit) pos += 1
    }
    if (pos == start) fail("Invalid number")
  }

  /** Funktionen der Spezifikation: Name -> (Argumenttypen, Ergebnistyp). */
  private val Functions: Map[String, (Seq[Kind], Kind)] = Map(
    "length" -> (Seq(Kind.Value), Kind.Value),
    "count" -> (Seq(Kind.Nodes), Kind.Value),
    "value" -> (Seq(Kind.Nodes), Kind.Value),
    "match" -> (Seq(Kind.Value, Kind.Value), Kind.Logical),
    "search" -> (Seq(Kind.Value, Kind.Value), Kind.Logical))

  private def function(queries: mutable.ArrayBuffer[FilterQueries]): Kind = {
    val start = pos
    while (!atEnd && ((peek >= 'a' && peek <= 'z') || peek.isDigit || peek == '_')) pos += 1
    val name = text.substring(start, pos)
    val (parameters, result) = Functions.getOrElse(name, fail(s"Unknown function '$name'"))
    expect("(")
    blanks()
    val arguments = mutable.ArrayBuffer.empty[Kind]
    if (peek != ')') {
      arguments += argument(queries)
      blanks()
      while (peek == ',') { pos += 1; blanks(); arguments += argument(queries); blanks() }
    }
    expect(")")
    if (arguments.size != parameters.size) fail(s"Function '$name' expects ${parameters.size} argument(s)")
    parameters.zip(arguments).foreach { (expected, actual) =>
      val ok = expected match {
        case Kind.Value => actual == Kind.Value || actual == Kind.Literal || actual == Kind.SingularNodes
        case Kind.Nodes => actual == Kind.Nodes || actual == Kind.SingularNodes
        case _ => true
      }
      if (!ok) fail(s"Argument of '$name' has the wrong type")
    }
    result
  }

  private def argument(queries: mutable.ArrayBuffer[FilterQueries]): Kind = operand(queries)
}

/**
 * Selektoren der Meta-Schemas (ADR-0065): JSONPath nach RFC 9535 fuer Selektionen mit Wildcards und
 * Filtern, JSON Pointer nach RFC 6901 fuer konkrete Orte.
 *
 * Geprueft wird, ob ein Selektor im Zielschema existieren kann: jeder benannte Schritt muss in
 * `properties` (oder passend in `patternProperties` bzw. einem Schema unter `additionalProperties`)
 * deklariert sein, Index und Slice verlangen ein Array, jedes Element einer Mehrfachauswahl wird einzeln
 * geprueft, und die in Filtern eingebetteten Abfragen muessen im Schema der gefilterten Kinder bzw. der
 * Wurzel existieren. `patternProperties` werden mit Zeitgrenze ausgewertet (SCH-A06).
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
  def parseJsonPath(expression: String): Either[String, Seq[Step]] = new JsonPathParser(expression.trim).parseQuery()

  def parseJsonPointer(pointer: String): Either[String, Seq[Step]] =
    if (pointer.isEmpty) Right(Nil)
    else if (!pointer.startsWith("/")) Left("JSON Pointer must be empty or start with '/'")
    else {
      val segments = pointer.drop(1).split("/", -1).toSeq
      segments.find(segment => "~(?![01])".r.findFirstIn(segment).nonEmpty) match {
        case Some(segment) => Left(s"Invalid escape in segment '$segment'")
        case None => Right(segments.map(segment => Step.NameOrIndex(segment.replace("~1", "/").replace("~0", "~"))))
      }
    }

  /**
   * Prueft alle Selektoren gegen das Zielschema. `resolve` liefert referenzierte Dokumente aus der
   * lokalen Registry.
   */
  def check(selectors: Seq[Selector], targetUrl: String, target: ujson.Value, resolve: String => Option[ujson.Value],
            regexTimeout: FiniteDuration): Seq[Problem] =
    selectors.flatMap { selector =>
      val steps = selector.kind match {
        case Selector.Kind.JsonPath => parseJsonPath(selector.expression)
        case Selector.Kind.JsonPointer => parseJsonPointer(selector.expression)
      }
      steps match {
        case Left(problem) => Seq(Problem("invalid-selector", s"${selector.expression}: $problem", Some(selector.location)))
        case Right(parsed) =>
          val navigator = new Navigator(resolve, regexTimeout)
          val root = SchemaNode(targetUrl, target, target)
          val missing = navigator.missing(root, root, parsed).map(reason =>
            Problem("unknown-selector", s"${selector.expression}: $reason", Some(selector.location)))
          val timeouts = navigator.regexTimeouts.distinct.map(pattern =>
            Problem("selector-regex-timeout", s"${selector.expression}: patternProperties '$pattern' exceeded its time limit", Some(selector.location)))
          missing.toSeq ++ timeouts
      }
    }

  private final case class SchemaNode(documentUrl: String, root: ujson.Value, node: ujson.Value)

  private final class Navigator(resolve: String => Option[ujson.Value], regexTimeout: FiniteDuration) {
    private var visited = 0
    val regexTimeouts = mutable.ArrayBuffer.empty[String]

    /** Liefert den Grund, falls der Pfad im Schema nicht existieren kann. */
    def missing(root: SchemaNode, start: SchemaNode, steps: Seq[Step]): Option[String] =
      walk(root, Seq(start), steps).left.toOption

    private def walk(root: SchemaNode, start: Seq[SchemaNode], steps: Seq[Step]): Either[String, Seq[SchemaNode]] =
      steps.foldLeft[Either[String, Seq[SchemaNode]]](Right(start)) { (current, step) =>
        current.flatMap(nodes => apply(root, nodes.flatMap(expand), step))
      }

    private def apply(root: SchemaNode, nodes: Seq[SchemaNode], step: Step): Either[String, Seq[SchemaNode]] = step match {
      case Step.Union(members) =>
        members.foldLeft[Either[String, Seq[SchemaNode]]](Right(Nil)) { (acc, member) =>
          acc.flatMap(found => apply(root, nodes, member).map(found ++ _))
        }.map(_.distinct)
      case Step.Descendant(inner) =>
        val all = descendants(nodes)
        inner match {
          case Step.Union(members) =>
            members.foldLeft[Either[String, Seq[SchemaNode]]](Right(Nil)) { (acc, member) =>
              acc.flatMap(found => apply(root, all, member).map(found ++ _))
            }.left.map(reason => s"descendant: $reason")
          case other => apply(root, all, other).left.map(reason => s"descendant: $reason")
        }
      case Step.Filter(queries) =>
        val candidates = nodes.flatMap(children(_, Step.Wildcard)).distinct
        if (candidates.isEmpty) Left("filter selection has no children in the target schema")
        else {
          val relative = queries.relative.map(path => (path, walk(root, candidates, path)))
          val absolute = queries.absolute.map(path => (path, walk(root, Seq(root), path)))
          (relative ++ absolute).collectFirst { case (_, Left(reason)) => s"filter query: $reason" } match {
            case Some(reason) => Left(reason)
            case None => Right(candidates)
          }
        }
      case simple =>
        val next = nodes.flatMap(children(_, simple)).distinct
        if (next.isEmpty) Left(s"${describe(simple)} is not declared in the target schema") else Right(next)
    }

    private def describe(step: Step): String = step match {
      case Step.Name(name) => s"property '$name'"
      case Step.Index(index) => s"array item [$index]"
      case Step.Slice => "array slice"
      case Step.Wildcard => "child selection"
      case Step.NameOrIndex(segment) => s"location '$segment'"
      case other => other.toString
    }

    private def descendants(nodes: Seq[SchemaNode]): Seq[SchemaNode] = {
      val result = mutable.LinkedHashSet.empty[SchemaNode]
      var frontier = nodes
      var depth = 0
      while (frontier.nonEmpty && depth < 16 && visited < MaxNodes) {
        val fresh = frontier.flatMap(expand).filterNot(result.contains)
        result ++= fresh
        frontier = fresh.flatMap(children(_, Step.Wildcard))
        depth += 1
      }
      result.toSeq
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

    private def matches(pattern: String, name: String): Boolean =
      try SafeRegex.find(Pattern.compile(pattern), name, regexTimeout)
      catch {
        case _: RegexTimeoutException => regexTimeouts += pattern; false
        case _: PatternSyntaxException => false
      }

    private def children(node: SchemaNode, step: Step): Seq[SchemaNode] = {
      val obj = node.node.objOpt
      def child(value: ujson.Value) = node.copy(node = value)
      def properties = obj.flatMap(_.get("properties")).flatMap(_.objOpt).fold(Seq.empty[(String, ujson.Value)])(_.toSeq)
      def patternProperties = obj.flatMap(_.get("patternProperties")).flatMap(_.objOpt).fold(Seq.empty[(String, ujson.Value)])(_.toSeq)
      def additional = obj.flatMap(_.get("additionalProperties")).filter(_.objOpt.nonEmpty).toSeq
      def prefixItems = obj.flatMap(_.get("prefixItems")).flatMap(_.arrOpt).fold(Seq.empty[ujson.Value])(_.toSeq)
      def items = obj.flatMap(_.get("items")).filter(value => value.objOpt.nonEmpty || value == ujson.True).toSeq
      def named(name: String): Seq[SchemaNode] = {
        val declared = properties.collect { case (key, value) if key == name => value }
        val patterned = patternProperties.collect { case (pattern, value) if matches(pattern, name) => value }
        (declared ++ patterned ++ (if (declared.isEmpty && patterned.isEmpty) additional else Nil)).map(child)
      }
      def indexed(index: Int): Seq[SchemaNode] =
        (if (index >= 0 && index < prefixItems.size) Seq(prefixItems(index))
         else if (index < 0) prefixItems ++ items
         else items).map(child)
      step match {
        case Step.Name(name) => named(name)
        case Step.Index(index) => indexed(index)
        case Step.Slice => (prefixItems ++ items).map(child)
        case Step.NameOrIndex(segment) => named(segment) ++ segment.toIntOption.filter(_ >= 0).toSeq.flatMap(indexed)
        case Step.Wildcard => (properties.map(_._2) ++ patternProperties.map(_._2) ++ additional ++ prefixItems ++ items).map(child)
        case _ => Nil
      }
    }
  }
}
