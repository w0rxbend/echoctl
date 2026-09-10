package com.worxbend.echoctl.render

import com.worxbend.echoctl.api.Rgb
import os.Path

import scala.util.Try

final case class ParsedFrame(
  rows: Seq[Seq[Rgb]],
  palette: Map[String, Rgb]
)

object FramePreview:
  private val hexColor = "#[0-9a-fA-F]{6}".r
  private val rgbValue = "(?i)rgb\\((\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\)".r
  private val arrayRow = "^\\[(.*)\\]$".r

  private val namedColors: Map[String, Rgb] = Map(
    "red" -> Rgb(255, 0, 0),
    "green" -> Rgb(0, 255, 0),
    "blue" -> Rgb(0, 0, 255),
    "white" -> Rgb(255, 255, 255),
    "off" -> Rgb(0, 0, 0)
  )

  def parseColor(raw: String): Option[Rgb] =
    val trimmed = stripQuotes(raw.trim).toLowerCase
    if trimmed.isEmpty then None
    else if isHexColor(trimmed) then
      Try {
        val hex = trimmed.drop(1)
        Rgb(
          Integer.parseInt(hex.substring(0, 2), 16),
          Integer.parseInt(hex.substring(2, 4), 16),
          Integer.parseInt(hex.substring(4, 6), 16)
        )
      }.toOption
    else if namedColors.contains(trimmed) then
      namedColors.get(trimmed)
    else trimmed match
      case rgbValue(r, g, b) =>
        Try((r.toInt, g.toInt, b.toInt)).toOption.flatMap { case (red, green, blue) =>
          if inRange(red) && inRange(green) && inRange(blue) then Some(Rgb(red, green, blue)) else None
        }
      case _ => None

  def load(path: String): Either[String, ParsedFrame] =
    try
      val lines = os.read.lines(Path(path, os.pwd)).toSeq
      if lines.forall(_.trim.isEmpty) then
        Left("frame file is empty")
      else
        val normalized = lines.map(_.trim).filter(_.nonEmpty)
        val (palette, rowsSource) = splitSections(normalized)
        val hasRowsSection = normalized.exists(_.equalsIgnoreCase("rows:"))
        val parsedRows = if !hasRowsSection then
          parseLegacyRows(normalized.filterNot(inPaletteHeader), palette)
        else
          parseRows(rowsSource, palette)

        parsedRows match
          case Left(message) => Left(message)
          case Right(rows) =>
            Right(ParsedFrame(rows, palette))
    catch
      case ex: Exception => Left(ex.toString)

  def ansiGrid(rows: Seq[Seq[Rgb]]): String =
    val cell = (c: Rgb) =>
      s"\u001b[48;2;${c.r};${c.g};${c.b}m \u001b[0m"
    rows.map { row =>
      row.map(cell).mkString(" ", "", " ")
    }.mkString("\n")

  private def splitSections(lines: Seq[String]): (Map[String, Rgb], Seq[String]) =
    var palette = Map.empty[String, Rgb]
    var inPalette = false
    var inRows = false
    var inUnknown = false
    val rows = collection.mutable.ListBuffer.empty[String]

    for line <- lines do
      val trimmed = line.trim
      if trimmed.equalsIgnoreCase("palette:") then
        inPalette = true
        inRows = false
        inUnknown = false
      else if trimmed.equalsIgnoreCase("rows:") then
        inPalette = false
        inRows = true
        inUnknown = false
      else if inPalette && parsePaletteLine(trimmed).isDefined then
        val (name, color) = parsePaletteLine(trimmed).get
        palette = palette + (name -> color)
      else if inRows then
        rows += trimmed
      else if !inUnknown then
        rows += trimmed

    (palette, rows.toSeq.filterNot(isTopComment))

  private def inPaletteHeader(line: String): Boolean =
    line.equalsIgnoreCase("palette:") || line.equalsIgnoreCase("rows:")

  private def parsePaletteLine(line: String): Option[(String, Rgb)] =
    val normalized = line.trim
    if !normalized.contains(":") then None
    else if normalized.endsWith(":") then None
    else
      val parts = normalized.split(":", 2)
      val name = parts(0).trim.toLowerCase.stripPrefix("-").trim
      parseColor(parts(1).trim).map(color => (name, color))

  private def parseLegacyRows(lines: Seq[String], palette: Map[String, Rgb]): Either[String, Seq[Seq[Rgb]]] =
    val tokens = lines
      .filterNot(inPaletteHeader)
      .filterNot(isTopComment)
      .flatMap(rowTokens)
    resolveTokens(tokens, palette).flatMap { values =>
      if values.isEmpty then Left("frame file must contain color values")
      else if values.length != 64 then Left("frame file must contain exactly 64 colors (8x8)")
      else Right(values.grouped(8).toSeq)
    }

  private def parseRows(lines: Seq[String], palette: Map[String, Rgb]): Either[String, Seq[Seq[Rgb]]] =
    val rowTokenLists = lines.map(rowTokens).filter(_.nonEmpty)
    if rowTokenLists.isEmpty then Left("frame file must contain row data")
    else if rowTokenLists.size != 8 then Left("frame file rows must contain 8 rows")
    else if !rowTokenLists.forall(_.size == 8) then Left("frame file rows must contain 8 columns")
    else
      val resolved = rowTokenLists.map(resolveTokens(_, palette))
      resolved.collectFirst { case Left(message) => message } match
        case Some(message) => Left(message)
        case None => Right(resolved.map(_.toOption.get))

  private def rowTokens(raw: String): Seq[String] =
    val line = raw.stripPrefix("-").trim
    line match
      case arrayRow(inner) => splitArrayTokens(inner)
      case _ => splitLegacyRow(line)

  /** Resolves row tokens to colours, consulting the file's own palette first.
    *
    * The palette section is the whole point of naming colours, so a named symbol
    * must win over the built-in names — and a token that resolves to nothing is an
    * error, not a pixel to quietly drop. Dropping it used to shift every later
    * pixel in the row and produce a silently wrong picture.
    */
  private def resolveTokens(tokens: Seq[String], palette: Map[String, Rgb]): Either[String, Seq[Rgb]] =
    val resolved = tokens.map { token =>
      val key = stripQuotes(token.trim).toLowerCase
      palette.get(key).orElse(parseColor(token)).toRight(token.trim)
    }
    resolved.collectFirst { case Left(token) => token } match
      case Some(token) =>
        val known = (palette.keys.toSeq.sorted ++ namedColors.keys.toSeq.sorted).mkString(", ")
        Left(s"unknown color '$token'; use a hex value, rgb(r,g,b), or one of: $known")
      case None => Right(resolved.map(_.toOption.get))

  private def splitArrayTokens(raw: String): Seq[String] =
    val tokens = collection.mutable.ListBuffer.empty[String]
    val current = new StringBuilder
    var depth = 0

    for char <- raw do
      char match
        case '(' =>
          depth += 1
          current.append(char)
        case ')' =>
          depth = math.max(0, depth - 1)
          current.append(char)
        case ',' if depth == 0 =>
          val token = current.toString.trim
          if token.nonEmpty then tokens += token
          current.clear()
        case other =>
          current.append(other)

    val tail = current.toString.trim
    if tail.nonEmpty then tokens += tail
    tokens.toSeq

  private def splitLegacyRow(line: String): Seq[String] =
    line
      .replace("[", " ")
      .replace("]", " ")
      .split("\\s+")
      .toSeq
      .map(_.trim)
      .filter(_.nonEmpty)
      .filter(token => !token.startsWith("#") || isHexColor(token.toLowerCase))

  private def isTopComment(line: String): Boolean =
    val trimmed = line.trim
    trimmed.startsWith("#") && hexColor.findFirstIn(trimmed).isEmpty

  private def stripQuotes(value: String): String =
    value.stripPrefix("\"").stripSuffix("\"").stripPrefix("'").stripSuffix("'")

  private def isHexColor(value: String): Boolean =
    hexColor.pattern.matcher(value).matches()

  private def inRange(value: Int): Boolean = 0 <= value && value <= 255
