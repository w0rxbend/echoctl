package com.worxbend.echoctl.config

import upickle.default._
import upickle.default.ReadWriter
import os.Path
import scala.util.{Failure, Success, Try}

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

  /** Thrown when a config file exists but cannot be parsed.
    *
    * Swallowing the parse failure and returning an empty config was actively
    * destructive: every write path reads the file, adds to the result, and
    * writes it back, so one stray comma meant reading nothing, adding one
    * profile, and overwriting the user's other profiles with it.
    */
  final class ConfigParseError(val path: Path, cause: Throwable)
      extends RuntimeException(s"$path is not valid JSON: ${cause.getMessage}", cause)

  private def readFile(path: Path): StoredConfig =
    if !os.exists(path) then StoredConfig()
    else
      val raw = os.read(path)
      if raw.trim.isEmpty then StoredConfig()
      else
        Try(read[StoredConfig](raw)) match
          case Success(config) => config
          case Failure(cause) => throw ConfigParseError(path, cause)

  def configPath(overridePath: Option[String]): Path =
    overridePath.map(os.Path(_, os.pwd)).getOrElse(fallbackConfigPath)

  private def envValue(name: String, values: Map[String, String]): Option[String] =
    values.get(name).map(_.trim).filter(_.nonEmpty)

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
      .orElse(envValue("ECHOCTL_SERVER", env))
      .orElse(profile.flatMap(_.server))
      .getOrElse(defaultServer)

    val resolvedToken = tokenArg
      .orElse(envValue("ECHOCTL_TOKEN", env))
      .orElse(envValue("MATRIX_PROXY_ADMIN_TOKEN", env))
      .orElse(profile.flatMap(_.token))

    val resolvedDevice = deviceArg
      .orElse(envValue("ECHOCTL_DEVICE", env))
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

  /** The config file holds Profile.token, the bearer token that unlocks every
    * admin route on the server, so neither it nor its directory may be readable
    * by other users on the machine.
    */
  private val filePerms: os.PermSet = os.PermSet.fromString("rw-------")
  private val dirPerms: os.PermSet = os.PermSet.fromString("rwx------")

  private def ensureParent(path: Path): Unit =
    if !os.exists(path / os.up) then
      os.makeDir.all(path / os.up, perms = dirPerms)

  def write(pathArg: Option[String], config: StoredConfig): Unit =
    val path = configPath(pathArg)
    ensureParent(path)
    os.write.over(path, upickle.default.write(config, indent = 2), perms = filePerms)
    // write.over reuses an existing file's mode, so tighten a file that was
    // created before this was enforced.
    os.perms.set(path, filePerms)

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
