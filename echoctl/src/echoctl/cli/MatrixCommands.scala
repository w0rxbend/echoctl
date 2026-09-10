package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{
  AnimationFrameRequest,
  AnimationUploadRequest,
  BrightnessRequest,
  ColorRequest,
  PanelRequest,
  PixelRequest,
  Rgb
}
import com.worxbend.echoctl.render.{FramePreview, ParsedFrame}

final class MatrixCommands(val ctx: CliContext) extends CommandSupport:
  import MatrixCommands.*

  def fill(color: String): Unit =
    val (r, g, b) = resolveFillColor(color)
    printResponse(ctx.client.matrixFill(requireDevice(), ColorRequest(r, g, b)), _.status)

  def clear(): Unit =
    printResponse(ctx.client.matrixClear(requireDevice()), _.status)

  def brightness(value: Int): Unit =
    if value < 0 || value > 255 then
      failValidation("brightness must be in 0..255")
    printResponse(ctx.client.matrixBrightness(requireDevice(), BrightnessRequest(value)), _.status)

  def pixel(x: Int, y: Int, color: String): Unit =
    if x < 0 || x >= MatrixSize || y < 0 || y >= MatrixSize then
      failValidation(s"x and y must be in 0..${MatrixSize - 1}")
    val (r, g, b) = resolveFillColor(color)
    printResponse(ctx.client.matrixPixel(requireDevice(), PixelRequest(x, y, r, g, b)), _.status)

  def panel(state: String): Unit =
    val enabled = state.trim.toLowerCase match
      case "on" | "true" | "1" | "enable" | "enabled" => true
      case "off" | "false" | "0" | "disable" | "disabled" => false
      case other => failValidation(s"expected 'on' or 'off', got '$other'")
    printResponse(ctx.client.matrixPanel(requireDevice(), PanelRequest(enabled)), _.status)

  def static(color: String): Unit =
    val (r, g, b) = resolveFillColor(color)
    printResponse(ctx.client.matrixStatic(requireDevice(), ColorRequest(r, g, b)), _.status)

  /** Previews a frame file, and uploads it as a single-frame animation unless the
    * caller only wants the preview.
    */
  def frame(path: String, delay: String, previewOnly: Boolean): Unit =
    val parsed = loadFrame(path)
    ctx.output.verbose(FramePreview.ansiGrid(parsed.rows))
    if previewOnly then
      ctx.output.short(s"frame loaded and previewed from $path")
    else
      upload(Seq(parsed), delay)

  /** Uploads up to 8 frames for the device to loop locally. */
  def animation(paths: Seq[String], delay: String): Unit =
    if paths.isEmpty then failValidation("at least one frame file is required")
    if paths.length > MaxAnimationFrames then
      failValidation(s"the firmware animation slot holds at most $MaxAnimationFrames frames; got ${paths.length}")
    val frames = paths.map(loadFrame)
    frames.foreach(frame => ctx.output.verbose(FramePreview.ansiGrid(frame.rows)))
    upload(frames, delay)

  private def upload(frames: Seq[ParsedFrame], delay: String): Unit =
    val validatedDelay = validateDuration(delay)
    val request = buildUpload(frames, validatedDelay) match
      case Left(message) => failValidation(message)
      case Right(value) => value
    // Deliberately does not promise the loop keeps running. Any direct matrix
    // command marks the scheduler's desired background dirty, so when a background
    // is configured with restore_on_idle the panel reconverges to it as soon as the
    // queue drains — the uploaded animation is replaced, not looped indefinitely.
    printResponse(
      ctx.client.matrixAnimation(requireDevice(), request),
      _ => s"uploaded ${frames.length} frame(s) to the device animation slot"
    )

  private def loadFrame(path: String): ParsedFrame =
    FramePreview.load(path) match
      case Left(message) => failValidation(s"$path: $message")
      case Right(frame) => frame

  private def resolveFillColor(raw: String): (Int, Int, Int) =
    parseColor(raw) match
      case Some((r, g, b)) => (r, g, b)
      case None => failValidation(s"invalid color '$raw'")

object MatrixCommands:
  private val MatrixSize = 8

  /** Mirrors AppConfig::kMaxCustomFrames in the firmware. */
  val MaxAnimationFrames = 8

  /** Symbols used to name distinct colours when converting a pixel grid back into
    * the palette-and-rows form the API accepts. Deliberately excludes characters
    * that are awkward in JSON or shell quoting.
    */
  private val PaletteSymbols: Seq[Char] =
    ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9') ++ Seq('#', '@', '%', '&')

  private def hex(color: Rgb): String = f"#${color.r}%02X${color.g}%02X${color.b}%02X"

  /** Builds the upload request from parsed pixel grids.
    *
    * The API speaks palette-and-rows — the same vocabulary as config-authored
    * animations — so a grid of raw colours is folded back into a shared palette
    * across every frame. Black is pinned to "." to keep uploaded art readable in
    * logs and error messages.
    */
  def buildUpload(frames: Seq[ParsedFrame], delay: String): Either[String, AnimationUploadRequest] =
    for
      _ <- checkFrameCount(frames)
      _ <- checkFrameShapes(frames)
      symbolFor <- buildPalette(frames)
    yield AnimationUploadRequest(
      palette = symbolFor.map { case (color, symbol) => symbol.toString -> hex(color) },
      frames = frames.map { frame =>
        AnimationFrameRequest(
          delay = delay,
          rows = frame.rows.map(row => row.map(symbolFor).mkString)
        )
      }
    )

  private def checkFrameCount(frames: Seq[ParsedFrame]): Either[String, Unit] =
    if frames.isEmpty then Left("at least one frame is required")
    else if frames.length > MaxAnimationFrames then
      Left(s"the firmware animation slot holds at most $MaxAnimationFrames frames; got ${frames.length}")
    else Right(())

  private def checkFrameShapes(frames: Seq[ParsedFrame]): Either[String, Unit] =
    frames.zipWithIndex
      .collectFirst {
        case (frame, index) if frame.rows.length != MatrixSize || frame.rows.exists(_.length != MatrixSize) =>
          s"frame $index must be ${MatrixSize}x$MatrixSize"
      }
      .toLeft(())

  /** Folds every distinct colour across all frames into one shared palette, with
    * black pinned to "." so uploaded art stays readable in logs.
    */
  private def buildPalette(frames: Seq[ParsedFrame]): Either[String, Map[Rgb, Char]] =
    val black = Rgb(0, 0, 0)
    val distinct = frames.flatMap(_.rows.flatten).distinct.filterNot(_ == black)
    if distinct.length + 1 > PaletteSymbols.length then
      Left(s"frames use ${distinct.length + 1} distinct colours; at most ${PaletteSymbols.length} are supported")
    else
      Right(Map(black -> '.') ++ distinct.zip(PaletteSymbols).toMap)
