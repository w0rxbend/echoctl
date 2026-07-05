package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.BackgroundState

final class BackgroundCommands(val ctx: CliContext) extends CommandSupport:
  def get(): Unit =
    printResponse(ctx.client.getBackground(requireDevice()), renderBackground)

  def set(animation: String, noRestore: Boolean): Unit =
    val device = requireDevice()
    val request = BackgroundState(
      animation = Some(animation),
      restore_on_idle = Some(!noRestore)
    )
    printResponse(ctx.client.setBackground(device, request), _.status)

  private def renderBackground(response: com.worxbend.echoctl.api.BackgroundState): String =
    val restore = response.restore_on_idle.getOrElse(false)
    val animation = response.animation.getOrElse("<disabled>")
    s"""animation=$animation
restore_on_idle=$restore"""
