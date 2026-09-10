package com.worxbend.echoctl.cli

import com.worxbend.echoctl.config.Profile
import com.worxbend.echoctl.config.Config

final class ConfigCommands(val ctx: CliContext) extends CommandSupport:
  private def path: String = ctx.config.configPath.toString

  private def maskedConfig =
    val raw = Config.readOrCreate(Some(path))
    raw.copy(
      profiles = raw.profiles
        .view
        .map((name, profile) => name -> maskToken(profile))
        .toMap
    )

  private def maskToken(profile: Profile): Profile =
    profile.copy(token = profile.token.map(_ => "***"))

  def list(): Unit =
    val profiles = Config.listProfiles(Some(path))
    if profiles.isEmpty then
      ctx.output.short("no profiles configured")
    else
      ctx.output.prettyPrint(profiles)

  def show(): Unit =
    ctx.output.prettyPrint(maskedConfig)

  def init(
    profile: String,
    server: Option[String],
    device: Option[String],
    token: Option[String]
  ): Unit =
    val _ = Config.initProfile(Some(path), profile, server, device, token)
    ctx.output.ok(s"initialized profile '$profile' in $path")

  def use(profile: String): Unit =
    Config.useProfile(Some(path), profile) match
      case Some(_) => ctx.output.ok(s"default profile set to '$profile'")
      case None =>
        failValidation(s"profile '$profile' is unknown")
