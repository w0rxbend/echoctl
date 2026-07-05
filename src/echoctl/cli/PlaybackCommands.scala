package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{PlayRequest, PresetRequest, Presets}
import com.worxbend.echoctl.api.Rgb

final class PlaybackCommands(val ctx: CliContext) extends CommandSupport:
  def play(
    animation: String,
    duration: Option[String],
    priority: Option[Int],
    restore: Option[String],
    interruptMode: Option[String]
  ): Unit =
    val device = requireDevice()
    val catalog = ctx.client.catalogAnimations()
    catalog match
      case Left(error) => fail(error)
      case Right(response) =>
        val known = response.value.animations.find(_.id == animation)
        known match
          case Some(entry) if entry.playable.contains(false) =>
            failValidation(s"'$animation' is not playable. Use `preset $animation` instead.")
          case _ =>
            val request = PlayRequest(
              animation = animation,
              duration = duration.map(validateDuration),
              interrupt_mode = interruptMode,
              priority = priority,
              restore = restore,
              params = None
            )
            printResponse(ctx.client.play(device, request), _ => s"enqueued $animation")

  def preset(animation: String): Unit =
    val device = requireDevice()
    printResponse(ctx.client.playPreset(device, animation), value => s"preset queued: ${value.animation.getOrElse(animation)}")

  def effect(
    effectId: String,
    interval: Option[String],
    color: Option[String],
    r: Option[Int],
    g: Option[Int],
    b: Option[Int]
  ): Unit =
    val id = Presets.resolve(effectId).getOrElse(failValidation(s"unknown effect '$effectId'"))
    val rgb = resolveRgb(color, r, g, b)
    val device = requireDevice()
    val request = PresetRequest(effect_id = id, interval = interval.map(validateDuration), r = rgb.r, g = rgb.g, b = rgb.b)
    printResponse(ctx.client.matrixPreset(device, request), _.status)

  def stop(): Unit =
    val device = requireDevice()
    val request = PresetRequest(effect_id = 0, interval = Some("1s"), r = 0, g = 0, b = 0)
    printResponse(ctx.client.matrixPreset(device, request), _.status)

  private def resolveRgb(color: Option[String], r: Option[Int], g: Option[Int], b: Option[Int]): Rgb =
    (color, r, g, b) match
      case (Some(raw), None, None, None) =>
        parseColor(raw) match
          case Some((r1, g1, b1)) => Rgb(r1, g1, b1)
          case None => failValidation(s"invalid color '$raw'")
      case (None, Some(red), Some(green), Some(blue)) =>
        if red < 0 || red > 255 || green < 0 || green > 255 || blue < 0 || blue > 255 then
          failValidation("--r/--g/--b must be in 0..255")
        Rgb(red, green, blue)
      case (None, None, None, None) =>
        Rgb(255, 255, 255)
      case _ =>
        failValidation("provide --color or all of --r --g --b")
