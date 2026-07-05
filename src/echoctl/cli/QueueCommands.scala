package com.worxbend.echoctl.cli

final class QueueCommands(val ctx: CliContext) extends CommandSupport:
  def show(): Unit =
    printResponse(ctx.client.getQueue(requireDevice()), renderQueue)

  def clear(): Unit =
    printResponse(ctx.client.clearQueue(requireDevice()), response => s"cleared=${response.cleared}")

  private def renderQueue(response: com.worxbend.echoctl.api.QueueResponse): String =
    val header = s"""state=${response.state}
depth=${response.depth}"""
    if response.items.isEmpty then
      header
    else
      val lines = response.items.zipWithIndex.map { case (item, idx) => s"  ${idx + 1}. ${item}" }.mkString("\n")
      s"$header\nitems:\n$lines"
