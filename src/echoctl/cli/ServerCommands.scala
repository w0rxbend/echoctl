package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.ReadyzResponse

final class ServerCommands(val ctx: CliContext) extends CommandSupport:
  def health(): Unit =
    printResponse(ctx.client.health(), _ => "status: ok")

  def ready(): Unit =
    printResponse(ctx.client.ready(), renderReady)

  private def renderReady(response: ReadyzResponse): String =
    val header = s"status: ${response.status}"
    val workers = response.workers_running.getOrElse(false)
    val draining = response.draining.getOrElse(false)
    val deviceLines = response.devices.toSeq.sortBy(_._1).map { case (name, device) =>
      val scheduler = device.scheduler_state.getOrElse("unknown")
      val matrix = device.matrix_connected.getOrElse(false)
      val background = device.background.flatMap(_.state).getOrElse("unknown")
      s"  $name\n    scheduler=$scheduler\n    matrix_connected=$matrix\n    background=$background"
    }.mkString("\n")
    val details = if deviceLines.isEmpty then "" else "\n" + deviceLines
    header +
      s"\nworkers_running=$workers\ndraining=$draining" +
      details

  def openapi(): Unit =
    ctx.client.openapiSpec() match
      case Left(error) => fail(error)
      case Right(response) =>
        if ctx.output.isJson then
          ctx.output.withJson(response.raw, "")
        else
          ctx.output.prettyPrint(response.value)

  def metrics(): Unit =
    printText(ctx.client.getText("/metrics"))
