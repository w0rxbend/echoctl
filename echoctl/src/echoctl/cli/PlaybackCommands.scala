package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{PlayRequest, PresetRequest, Presets}
import com.worxbend.echoctl.api.Rgb

object PlaybackCommands:
  /** Mirrors animations.LoopPolicy in the server. */
  val LoopPolicies: Seq[String] = Seq("none", "until_deadline", "forever")

final class PlaybackCommands(val ctx: CliContext) extends CommandSupport:
  def play(
    animation: String,
    duration: Option[String],
    priority: Option[Int],
    restore: Option[String],
    interruptMode: Option[String],
    loop: Option[String]
  ): Unit =
    // "forever" with no deadline never completes and holds the play queue, so the
    // server rejects it; catch it here where the message can name the flag.
    loop.foreach { value =>
      if !PlaybackCommands.LoopPolicies.contains(value) then
        failValidation(s"invalid --loop '$value'; expected one of ${PlaybackCommands.LoopPolicies.mkString(", ")}")
      if value == "forever" && duration.isEmpty then
        failValidation("--loop forever requires --duration, otherwise the animation never finishes")
    }
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
              params = None,
              loop = loop
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
    val id = Presets
      .resolve(effectId)
      .getOrElse(
        failValidation(s"unknown effect '$effectId'; expected an id 0..${Presets.MaxEffectId} or one of: ${Presets.names.mkString(", ")}")
      )
    val colourWasGiven = color.isDefined || r.isDefined || g.isDefined || b.isDefined
    // The firmware answers OK whether or not the renderer uses the colour, so
    // without this warning `effect fire --color off` looks like it worked and
    // leaves the panel fully lit.
    if colourWasGiven && Presets.ignoresColorFor(id) then
      ctx.output.warn(s"effect '${Presets.name(id)}' computes its own colours and ignores --color/--r/--g/--b")
    if Presets.isRetired(id) then
      val replacement = Presets.replacementFor(id).map(rep => s"; prefer '$rep'").getOrElse("")
      ctx.output.warn(s"effect '${Presets.name(id)}' is retired — it is a parameter variant of another effect$replacement")
    val rgb = resolveRgb(color, r, g, b)
    val device = requireDevice()
    val request = PresetRequest(effect_id = id, interval = interval.map(validateDuration), r = rgb.r, g = rgb.g, b = rgb.b)
    printResponse(ctx.client.matrixPreset(device, request), _.status)

  /** Lists the effects worth choosing from, marking the ones that ignore colour. */
  def effects(): Unit =
    val rows = Presets.effects.toSeq.sortBy(_._1).collect {
      case (id, name) if id != 0 && !Presets.isRetired(id) =>
        val note = if Presets.ignoresColorFor(id) then "  (ignores --color)" else ""
        f"  $id%2d  $name%-14s$note"
    }
    ctx.output.short((Seq("firmware effects:") ++ rows).mkString("\n"))

  /** Stops the running firmware effect. This does not clear the panel: the firmware
    * leaves the frame buffer untouched, so the last effect frame stays lit. Use
    * `matrix clear` to go dark, or `matrix panel off` for a dark panel that idle
    * background convergence will not repaint.
    */
  def stop(): Unit =
    val device = requireDevice()
    val request = PresetRequest(effect_id = 0, interval = Some("1s"), r = 0, g = 0, b = 0)
    printResponse(ctx.client.matrixPreset(device, request), _ => "effect stopped (the last frame stays lit; use 'matrix clear' to go dark)")

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
