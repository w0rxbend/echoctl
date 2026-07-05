package com.worxbend.echoctl.cli

final class AnimationCommands(val ctx: CliContext) extends CommandSupport:
  def list(): Unit =
    ctx.client.listAnimations() match
      case Left(error) => fail(error)
      case Right(response) =>
        if response.value.animations.isEmpty then
          if ctx.output.isJson then
            ctx.output.withJson(response.raw, "[]")
          else
            ctx.output.short("no animations available")
        else
          if ctx.output.isJson then
            ctx.output.withJson(response.raw, "")
          else
            ctx.output.prettyPrint(response.value.animations.sorted)

  def catalog(): Unit =
    printResponse(ctx.client.catalogAnimations(), renderCatalog)

  private def renderCatalog(response: com.worxbend.echoctl.api.AnimationCatalogResponse): String =
    if response.animations.isEmpty then
      "no animations"
    else
      response.animations
        .sortBy(_.id)
        .map { entry =>
          val playable = entry.playable.getOrElse(false)
          val kind = entry.kind.getOrElse("unknown")
          val extra = (entry.effect_id, entry.interval, entry.color) match
            case (Some(effect), Some(interval), Some(_)) =>
              s", effect=$effect interval=$interval"
            case (Some(effect), Some(interval), None) =>
              s", effect=$effect interval=$interval"
            case (Some(effect), None, _) => s", effect=$effect"
            case _ => ""
          s"${entry.id}\t$kind\tplayable=$playable$extra"
        }
        .mkString("\n")
