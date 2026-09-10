package com.worxbend.echoctl.cli

import com.worxbend.echoctl.api.{ApiError, ApiResponse, ExitCode}
import com.worxbend.echoctl.config.ResolvedConfig
import com.worxbend.echoctl.render.Output

import scala.util.matching.Regex
import scala.util.Try

class CliFailure(val exitCode: Int, message: String) extends RuntimeException(message)

case class CliContext(
  config: ResolvedConfig,
  output: Output,
  client: com.worxbend.echoctl.api.EchoClient
)

trait CommandSupport:
  val ctx: CliContext

  private val deviceRequired = "--device is required for this command"

  protected def requireDevice(explicit: Option[String] = None): String =
    explicit.orElse(ctx.config.device) match
      case Some(value) => value
      case None => failUsage(deviceRequiredMessage())

  /** Naming the available devices is an enhancement to the usage message, so its
    * failure must not replace it. /api/v1/devices is admin-only: without a token
    * the lookup returns 401, and reporting that told the user about the wrong
    * problem when all they had done was omit --device.
    */
  private def deviceRequiredMessage(): String =
    ctx.client.listDevices() match
      case Left(_) => deviceRequired
      case Right(response) =>
        val devices = response.value.devices.sorted
        if devices.isEmpty then deviceRequired
        else s"$deviceRequired. Available: ${devices.mkString(", ")}"

  protected def ok(message: String): Unit =
    ctx.output.ok(message)

  protected def warn(message: String): Unit =
    ctx.output.warn(message)

  protected def fail(error: ApiError): Nothing =
    ctx.output.error(error.message)
    if error.body.nonEmpty then
      ctx.output.verbose(error.body)
    throw CliFailure(error.exitCode, error.message)

  protected def failUsage(message: String): Nothing =
    ctx.output.error(message)
    throw CliFailure(ExitCode.Usage, message)

  protected def failValidation(message: String): Nothing =
    ctx.output.error(message)
    throw CliFailure(ExitCode.Validation, message)

  protected def printResponse[T](result: Either[ApiError, ApiResponse[T]], render: T => String): Unit =
    result match
      case Right(resp) =>
        val raw = resp.raw
        if ctx.output.isJson then
          ctx.output.withJson(raw, render(resp.value))
        else
          ctx.output.short(render(resp.value))
      case Left(error) =>
        fail(error)

  protected def printText(result: Either[ApiError, String]): Unit =
    result match
      case Right(value) => ctx.output.short(value)
      case Left(error) => fail(error)

  protected def validateColor(raw: String): (Int, Int, Int) =
    parseColor(raw).getOrElse(throw CliFailure(ExitCode.Validation, s"invalid color '$raw'"))

  protected def parseColor(raw: String): Option[(Int, Int, Int)] =
    CommandSupport.parseColor(raw)

  protected def validateDuration(raw: String): String =
    if CommandSupport.isDuration(raw) then raw
    else throw CliFailure(ExitCode.Validation, s"invalid duration '$raw'")

  protected def parseAttributes(values: Seq[String]): Map[String, String] =
    values.foldLeft(Map.empty[String, String]) { (acc, value) =>
      value.split("=", 2) match
        case Array(k, v) if k.nonEmpty =>
          if acc.contains(k) then
            throw CliFailure(ExitCode.Validation, s"duplicate attribute '$k'")
          acc + (k -> v)
        case _ =>
          throw CliFailure(ExitCode.Validation, s"invalid attribute '$value'; expected key=value")
    }

object CommandSupport:
  private val shortHex: Regex = "^#([0-9a-fA-F]{6})$".r
  private val rgb: Regex = "^rgb\\((\\d{1,3})\\s*,\\s*(\\d{1,3})\\s*,\\s*(\\d{1,3})\\)$".r
  private val duration: Regex = "^(\\d+(?:\\.\\d+)?)(ns|us|µs|ms|s|m|h)$".r

  private val named: Map[String, (Int, Int, Int)] =
    Map(
      "red" -> (255, 0, 0),
      "green" -> (0, 255, 0),
      "blue" -> (0, 0, 255),
      "white" -> (255, 255, 255),
      "off" -> (0, 0, 0)
    )

  def parseColor(raw: String): Option[(Int, Int, Int)] =
    val trimmed = raw.trim.toLowerCase
    named.get(trimmed).orElse {
      shortHex.findFirstMatchIn(trimmed).flatMap { m =>
        Try(Integer.parseInt(m.group(1), 16)).toOption.map { value =>
          ((value >> 16) & 0xff, (value >> 8) & 0xff, value & 0xff)
        }
      }
    }.orElse {
      rgb.findFirstMatchIn(trimmed).flatMap { m =>
        Try(
          (
            m.group(1).toInt,
            m.group(2).toInt,
            m.group(3).toInt
          )
        ).toOption.filter { case (r, g, b) =>
          (0 <= r && r <= 255) && (0 <= g && g <= 255) && (0 <= b && b <= 255)
        }
      }
    }

  def isDuration(value: String): Boolean =
    val trimmed = value.trim.toLowerCase
    duration.matches(trimmed)
