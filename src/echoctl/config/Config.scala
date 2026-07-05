package com.worxbend.echoctl.config

import upickle.default._
import upickle.default.ReadWriter
import os.Path
import scala.util.Try

case class Profile(
  server: Option[String] = None,
  device: Option[String] = None,
  token: Option[String] = None
) derives ReadWriter

case class StoredConfig(
  default_profile: Option[String] = None,
  profiles: Map[String, Profile] = Map.empty
) derives ReadWriter

case class ResolvedConfig(
  server: String,
  token: Option[String],
  device: Option[String],
  profile: Option[String],
  configPath: Path,
  file: StoredConfig
)

object Config:
  private val defaultServer = "http://127.0.0.1:8080"
  private val fallbackConfigPath: Path =
    os.home / ".config" / "echoctl" / "config.json"

  private def readFile(path: Path): StoredConfig =
    if os.exists(path) then
      val raw = os.read(path)
      if raw.trim.isEmpty then StoredConfig()
      else Try(read[StoredConfig](raw)).getOrElse(StoredConfig())
    else StoredConfig()

  def configPath(overridePath: Option[String]): Path =
    overridePath.map(os.Path(_, os.pwd)).getOrElse(fallbackConfigPath)

  private def env(name: String, env: Map[String, String]): Option[String] =
    env.get(name).map(_.trim).filter(_.nonEmpty)

  private def fallbackProfile(
    file: StoredConfig,
    requestedProfile: Option[String]
  ): Option[String] =
    requestedProfile
      .orElse(file.default_profile)
      .orElse(file.profiles.keys.headOption)

  def resolve(
    serverArg: Option[String],
    tokenArg: Option[String],
    deviceArg: Option[String],
    profileArg: Option[String],
    pathArg: Option[String] = None,
    env: Map[String, String] = sys.env
  ): ResolvedConfig =
    val path = configPath(pathArg)
    val file = readFile(path)
    val selectedProfile = fallbackProfile(file, profileArg)
    val profile = selectedProfile.flatMap(file.profiles.get)

    val resolvedServer = serverArg
      .orElse(env("ECHOCTL_SERVER"))
      .orElse(profile.flatMap(_.server))
      .getOrElse(defaultServer)

    val resolvedToken = tokenArg
      .orElse(env("ECHOCTL_TOKEN"))
      .orElse(env("MATRIX_PROXY_ADMIN_TOKEN"))
      .orElse(profile.flatMap(_.token))

    val resolvedDevice = deviceArg
      .orElse(env("ECHOCTL_DEVICE"))
      .orElse(profile.flatMap(_.device))

    ResolvedConfig(
      resolvedServer,
      resolvedToken,
      resolvedDevice,
      selectedProfile,
      path,
      file
    )

  def listProfiles(pathArg: Option[String] = None): Seq[String] =
    val path = configPath(pathArg)
    val file = readFile(path)
    file.profiles.keys.toSeq.sorted

  def readOrCreate(pathArg: Option[String] = None): StoredConfig = readFile(configPath(pathArg))

  private def ensureParent(path: Path): Unit =
    if !os.exists(path / os.up) then
      os.makeDir.all(path / os.up)

  def write(pathArg: Option[String], config: StoredConfig): Unit =
    val path = configPath(pathArg)
    ensureParent(path)
    os.write.over(path, write(config, indent = 2))

  def useProfile(pathArg: Option[String], profile: String): Option[StoredConfig] =
    val path = configPath(pathArg)
    val file = readFile(path)
    if !file.profiles.contains(profile) then
      None
    else
      val updated = file.copy(default_profile = Some(profile))
      write(pathArg, updated)
      Some(updated)

  def initProfile(
    pathArg: Option[String],
    profile: String,
    server: Option[String],
    device: Option[String],
    token: Option[String]
  ): StoredConfig =
    val path = configPath(pathArg)
    val base = readFile(path)
    val existing = base.profiles.getOrElse(profile, Profile())
    val updatedProfile = Profile(
      server = server.orElse(existing.server).orElse(Some(defaultServer)),
      device = device.orElse(existing.device),
      token = token.orElse(existing.token)
    )
    val updated = base.copy(
      default_profile = Some(profile),
      profiles = base.profiles + (profile -> updatedProfile)
    )
    write(pathArg, updated)
    updated
