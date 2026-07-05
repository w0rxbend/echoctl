package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{BrightnessRequest, ColorRequest}
import com.worxbend.echoctl.render.FramePreview

final class MatrixCommands(val ctx: CliContext) extends CommandSupport:
  def fill(color: String): Unit =
    val (r, g, b) = resolveFillColor(color)
    printResponse(ctx.client.matrixFill(requireDevice(), ColorRequest(r, g, b)), _.status)

  def clear(): Unit =
    printResponse(ctx.client.matrixClear(requireDevice()), _.status)

  def brightness(value: Int): Unit =
    if value < 0 || value > 255 then
      failValidation("brightness must be in 0..255")
    printResponse(ctx.client.matrixBrightness(requireDevice(), BrightnessRequest(value)), _.status)

  def frame(path: String): Unit =
    FramePreview.load(path) match
      case Left(message) => failValidation(message)
      case Right(frame) =>
        val text = FramePreview.ansiGrid(frame.rows)
        ctx.output.verbose(text)
        ctx.output.warn("frame upload is not supported by /api/v1 matrix endpoints")
        ctx.output.short("frame loaded and previewed from local file")

  private def resolveFillColor(raw: String): (Int, Int, Int) =
    parseColor(raw) match
      case Some((r, g, b)) => (r, g, b)
      case None => failValidation(s"invalid color '$raw'")
