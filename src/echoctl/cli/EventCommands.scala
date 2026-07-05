package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{EventRequest, NotifyRequest}

final class EventCommands(val ctx: CliContext) extends CommandSupport:
  def notify(
    message: String,
    duration: Option[String],
    animation: Option[String],
    level: Option[String],
    priority: Option[Int],
    restore: Option[String],
    title: Option[String],
    attrs: Seq[String]
  ): Unit =
    val request = NotifyRequest(
      title = title,
      message = message,
      duration = duration.map(validateDuration),
      animation = animation,
      level = level,
      priority = priority,
      restore = restore,
      params = if attrs.isEmpty then None else Some(parseAttributes(attrs))
    )
    printResponse(ctx.client.notify(requireDevice(), request), _.event_id)

  def event(
    text: Option[String],
    source: Option[String],
    eventType: Option[String],
    id: Option[String],
    channel: Option[String],
    target: Option[String],
    actor: Option[String],
    priority: Option[Int],
    attrs: Seq[String]
  ): Unit =
    val resolvedType =
      eventType.getOrElse(failValidation("--type is required"))
    val request = EventRequest(
      source = source,
      `type` = Some(resolvedType),
      id = id,
      text = text,
      channel = channel,
      target = target,
      actor = actor,
      priority = priority,
      attributes = if attrs.isEmpty then None else Some(parseAttributes(attrs))
    )
    printResponse(ctx.client.event(requireDevice(), request), _.event_id)
