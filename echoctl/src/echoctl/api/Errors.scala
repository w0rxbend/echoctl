package com.worxbend.echoctl.api

import scala.util.Try

object ExitCode:
  val Ok = 0
  val ApiError = 1
  val Usage = 2
  val Connection = 3
  val Validation = 4

  /** The config file exists but could not be parsed. Distinct from Usage so a
    * script can tell "you typed it wrong" from "your config file is broken".
    */
  val Config = 5

  /** An unexpected exception escaped a command. Previously these were reported
    * as Connection, which made a bug in the CLI indistinguishable from the
    * server being unreachable.
    */
  val Internal = 6

sealed trait ApiError:
  def exitCode: Int
  def kind: String
  def message: String
  def body: String
  def path: String
  def status: Option[Int]

object ApiError:
  def from(status: Int, path: String, body: String, parsed: Option[String]): ApiError =
    parsed match
      case Some(error) => HttpError(status, path, error, body)
      case None => HttpError(status, path, "request failed", body)

final case class HttpError(
  statusCode: Int,
  requestPath: String,
  detail: String,
  rawBody: String
) extends ApiError:
  val exitCode = ExitCode.ApiError
  val kind = "http"
  val message: String = s"HTTP ${statusCode} on ${requestPath}: ${detail}"
  val body: String = rawBody
  val path: String = requestPath
  val status: Option[Int] = Some(statusCode)

final case class DecodeError(
  requestPath: String,
  rawBody: String,
  cause: String
) extends ApiError:
  val exitCode = ExitCode.ApiError
  val kind = "decode"
  val message: String = s"failed decoding response for ${requestPath}: ${cause}"
  val body: String = rawBody
  val path: String = requestPath
  val status: Option[Int] = None

final case class ConnectionError(
  requestPath: String,
  cause: String
) extends ApiError:
  val exitCode = ExitCode.Connection
  val kind = "connection"
  val message: String = cause
  val body: String = ""
  val path: String = requestPath
  val status: Option[Int] = None

final case class ApiResponse[T](status: Int, path: String, raw: String, value: T)
