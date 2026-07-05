package com.worxbend.echoctl.cli

final class DeviceCommands(val ctx: CliContext) extends CommandSupport:
  def devices(): Unit =
    ctx.client.listDevices() match
      case Left(error) => fail(error)
      case Right(response) =>
        if response.value.devices.isEmpty then
          if ctx.output.isJson then
            ctx.output.withJson(response.raw, "[]")
          else
            ctx.output.short("no devices found")
        else
          if ctx.output.isJson then
            ctx.output.withJson(response.raw, "")
          else
            ctx.output.prettyPrint(response.value.devices.sorted)
