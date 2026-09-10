package com.worxbend.echoctl

import com.worxbend.echoctl.cli.CliContext
import com.worxbend.echoctl.cli.CliFailure
import com.worxbend.echoctl.cli.BackgroundCommands
import com.worxbend.echoctl.cli.ConfigCommands
import com.worxbend.echoctl.cli.DeviceCommands
import com.worxbend.echoctl.cli.MatrixCommands
import com.worxbend.echoctl.cli.AnimationCommands
import com.worxbend.echoctl.cli.PlaybackCommands
import com.worxbend.echoctl.cli.EventCommands
import com.worxbend.echoctl.cli.QueueCommands
import com.worxbend.echoctl.cli.ServerCommands
import com.worxbend.echoctl.config.Config
import com.worxbend.echoctl.render.Output
import com.worxbend.echoctl.api.EchoClient
import com.worxbend.echoctl.api.ExitCode
import picocli.CommandLine
import picocli.CommandLine.{Command, IExecutionExceptionHandler, Parameters, ParentCommand}
import picocli.CommandLine.Option as CliOption

import scala.jdk.CollectionConverters._

object Main:
  def main(args: Array[String]): Unit =
    val root = EchoCtl()
    val cli = CommandLine(root).setExecutionExceptionHandler(CliExecutionExceptionHandler(root))
    val code = cli.execute(args*)
    System.exit(code)

class CliExecutionExceptionHandler(root: EchoCtl) extends IExecutionExceptionHandler:
  override def handleExecutionException(
    ex: Exception,
    commandLine: CommandLine,
    parseResult: CommandLine.ParseResult
  ): Int =
    ex match
      case failure: CliFailure =>
        // Already reported through Output by whoever threw it.
        failure.exitCode
      case broken: Config.ConfigParseError =>
        commandLine.getErr.println(describe(broken))
        ExitCode.Config
      case other =>
        commandLine.getErr.println(describe(other))
        if root.verbose then other.printStackTrace(commandLine.getErr)
        ExitCode.Internal

  /** getMessage is null for plenty of JVM exceptions -- an NPE among them -- so
    * fall back to the class name rather than printing the word "null".
    */
  private def describe(ex: Throwable): String =
    Option(ex.getMessage).filter(_.nonEmpty).getOrElse(ex.getClass.getName)

@Command(
  name = "echoctl",
  subcommands = Array(
    classOf[HealthCommand],
    classOf[ReadyCommand],
    classOf[OpenApiCommand],
    classOf[MetricsCommand],
    classOf[DevicesCommand],
    classOf[AnimationsCommand],
    classOf[PlayCommand],
    classOf[PresetCommand],
    classOf[EffectCommand],
    classOf[EffectsListCommand],
    classOf[StopCommand],
    classOf[NotifyCommand],
    classOf[EventCommand],
    classOf[MatrixCommand],
    classOf[BackgroundCommand],
    classOf[QueueCommand],
    classOf[ConfigCommand]
  )
)
class EchoCtl extends Runnable:
  @CliOption(names = Array("-h", "--help"), usageHelp = true, description = Array("Show this help message and exit"))
  var help: Boolean = false

  @CliOption(names = Array("--server"), description = Array("Echo API base URL"))
  var server: String = null

  @CliOption(names = Array("--token"), description = Array("Authorization token"))
  var token: String = null

  @CliOption(names = Array("--device"), description = Array("Default device"))
  var device: String = null

  @CliOption(names = Array("--profile"), description = Array("Config profile"))
  var profile: String = null

  @CliOption(names = Array("--config"), description = Array("Config file path"))
  var configPath: String = null

  @CliOption(names = Array("--json"), description = Array("Emit raw JSON output"))
  var json: Boolean = false

  @CliOption(names = Array("--verbose"), description = Array("Verbose output"))
  var verbose: Boolean = false

  private var cachedContext: CliContext = null

  def context: CliContext =
    if cachedContext == null then
      val resolved = Config.resolve(
        serverArg = Option(server).filter(_.nonEmpty),
        tokenArg = Option(token).filter(_.nonEmpty),
        deviceArg = Option(device).filter(_.nonEmpty),
        profileArg = Option(profile).filter(_.nonEmpty),
        pathArg = Option(configPath).filter(_.nonEmpty)
      )
      val output = Output(json, verbose)
      cachedContext = CliContext(resolved, output, EchoClient(resolved.server, resolved.token))
    cachedContext

  override def run(): Unit =
    // Resolve config first, so a broken config file is reported as such rather
    // than hidden behind the usage message.
    val _ = context
    throw CliFailure(ExitCode.Usage, "missing command; use --help")

@Command(name = "health", description = Array("GET /healthz"))
class HealthCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    ServerCommands(parent.context).health()

@Command(name = "ready", description = Array("GET /readyz"))
class ReadyCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    ServerCommands(parent.context).ready()

@Command(name = "openapi", description = Array("Dump /openapi.json"))
class OpenApiCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    ServerCommands(parent.context).openapi()

@Command(name = "metrics", description = Array("Fetch Prometheus metrics"))
class MetricsCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    ServerCommands(parent.context).metrics()

@Command(name = "devices", description = Array("List devices"))
class DevicesCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    DeviceCommands(parent.context).devices()

@Command(
  name = "animations",
  description = Array("Animation namespace"),
  subcommands = Array(
    classOf[AnimationListCommand],
    classOf[AnimationCatalogCommand]
  )
)
class AnimationsCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  def context: CliContext = parent.context

  override def run(): Unit =
    throw CliFailure(ExitCode.Usage, "animations list|catalog")

@Command(name = "list", description = Array("List animations"))
class AnimationListCommand extends Runnable:
  @ParentCommand
  private var parent: AnimationsCommand = null

  override def run(): Unit =
    AnimationCommands(parent.context).list()

@Command(name = "catalog", description = Array("Catalog animations"))
class AnimationCatalogCommand extends Runnable:
  @ParentCommand
  private var parent: AnimationsCommand = null

  override def run(): Unit =
    AnimationCommands(parent.context).catalog()

@Command(name = "play", description = Array("Enqueue generated/frame animation"))
class PlayCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @Parameters(index = "0", paramLabel = "ANIMATION", description = Array("Animation id"))
  var animation: String = null

  @CliOption(names = Array("--duration"), description = Array("Go duration"))
  var duration: String = null

  @CliOption(names = Array("--priority"), description = Array("Priority"))
  var priority: Integer = null

  @CliOption(names = Array("--restore"), description = Array("Restore state"))
  var restore: String = null

  @CliOption(names = Array("--interrupt-mode"), description = Array("Interrupt mode"))
  var interruptMode: String = null

  @CliOption(
    names = Array("--loop"),
    description = Array("Repeat the animation: none (default), until_deadline, or forever (requires --duration)")
  )
  var loop: String = null

  override def run(): Unit =
    PlaybackCommands(parent.context).play(
      animation,
      Option(duration),
      Option(priority).map(_.toInt),
      Option(restore),
      Option(interruptMode),
      Option(loop)
    )

@Command(name = "preset", description = Array("Queue firmware preset by animation id"))
class PresetCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @Parameters(index = "0", paramLabel = "ANIMATION", description = Array("Preset animation id"))
  var animation: String = null

  override def run(): Unit =
    PlaybackCommands(parent.context).preset(animation)

@Command(name = "effects", description = Array("List the firmware effects worth choosing from"))
class EffectsListCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    PlaybackCommands(parent.context).effects()

@Command(name = "effect", description = Array("Apply firmware effect preset (see `effects` for the list)"))
class EffectCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @Parameters(index = "0", paramLabel = "EFFECT", description = Array("Effect id or name"))
  var effectId: String = null

  @CliOption(names = Array("--interval"), description = Array("Interval duration"))
  var interval: String = null

  @CliOption(names = Array("--color"), description = Array("#RRGGBB or rgb(r,g,b)"))
  var color: String = null

  @CliOption(names = Array("--r"), description = Array("Red"))
  var r: Integer = null

  @CliOption(names = Array("--g"), description = Array("Green"))
  var g: Integer = null

  @CliOption(names = Array("--b"), description = Array("Blue"))
  var b: Integer = null

  override def run(): Unit =
    PlaybackCommands(parent.context).effect(
      effectId,
      Option(interval),
      Option(color),
      Option(r).map(_.toInt),
      Option(g).map(_.toInt),
      Option(b).map(_.toInt)
    )

@Command(name = "stop", description = Array("Stop playback"))
class StopCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  override def run(): Unit =
    PlaybackCommands(parent.context).stop()

@Command(name = "notify", description = Array("Post generic notify event"))
class NotifyCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @Parameters(index = "0", paramLabel = "MESSAGE", description = Array("Notification message"))
  var message: String = null

  @CliOption(names = Array("--duration"))
  var duration: String = null

  @CliOption(names = Array("--animation"))
  var animation: String = null

  @CliOption(names = Array("--level"))
  var level: String = null

  @CliOption(names = Array("--priority"))
  var priority: Integer = null

  @CliOption(names = Array("--restore"))
  var restore: String = null

  @CliOption(names = Array("--title"))
  var title: String = null

  @CliOption(names = Array("--attr"), arity = "*")
  var attrs: java.util.List[String] = new java.util.ArrayList[String]()

  override def run(): Unit =
    EventCommands(parent.context).notify(
      message,
      Option(duration),
      Option(animation),
      Option(level),
      Option(priority).map(_.toInt),
      Option(restore),
      Option(title),
      attrs.asScala.toSeq
    )

@Command(name = "event", description = Array("Post generic event"))
class EventCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @Parameters(index = "0", arity = "0..1", paramLabel = "TEXT", description = Array("Event text"))
  var text: String = null

  @CliOption(names = Array("--type"), description = Array("Event type (required)"))
  var eventType: String = null

  @CliOption(names = Array("--source"), description = Array("Event source"))
  var source: String = null

  @CliOption(names = Array("--id"), description = Array("Event id"))
  var eventId: String = null

  @CliOption(names = Array("--channel"), description = Array("Event channel"))
  var channel: String = null

  @CliOption(names = Array("--target"), description = Array("Event target"))
  var target: String = null

  @CliOption(names = Array("--actor"), description = Array("Event actor"))
  var actor: String = null

  @CliOption(names = Array("--priority"), description = Array("Priority"))
  var priority: Integer = null

  @CliOption(names = Array("--attr"), arity = "*")
  var attrs: java.util.List[String] = new java.util.ArrayList[String]()

  override def run(): Unit =
    EventCommands(parent.context).event(
      Option(text),
      Option(source),
      Option(eventType),
      Option(eventId),
      Option(channel),
      Option(target),
      Option(actor),
      Option(priority).map(_.toInt),
      attrs.asScala.toSeq
    )

@Command(
  name = "matrix",
  description = Array("Direct matrix controls"),
  subcommands = Array(
    classOf[MatrixFillCommand],
    classOf[MatrixClearCommand],
    classOf[MatrixBrightnessCommand],
    classOf[MatrixFrameCommand],
    classOf[MatrixAnimationCommand],
    classOf[MatrixPixelCommand],
    classOf[MatrixPanelCommand]
  )
)
class MatrixCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  def context: CliContext = parent.context

  override def run(): Unit =
    // Resolve config for its side effect: a broken config file should be
    // reported as such rather than hidden behind the usage message.
    val _ = parent.context
    throw CliFailure(ExitCode.Usage, "matrix fill|clear|brightness|pixel|panel|frame|animation")

@Command(name = "fill", description = Array("Fill matrix with color (background state may restore over fill/clear)"))
class MatrixFillCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0", paramLabel = "COLOR")
  var color: String = null

  override def run(): Unit =
    MatrixCommands(parent.context).fill(color)

@Command(name = "clear", description = Array("Clear matrix (background state may restore over clear)"))
class MatrixClearCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  override def run(): Unit =
    MatrixCommands(parent.context).clear()

@Command(name = "brightness", description = Array("Set matrix brightness"))
class MatrixBrightnessCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0", paramLabel = "VALUE")
  var value: Integer = null

  override def run(): Unit =
    MatrixCommands(parent.context).brightness(value.intValue())

@Command(name = "frame", description = Array("Upload a local frame file to the device (use --preview-only to just render it)"))
class MatrixFrameCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0", paramLabel = "PATH")
  var path: String = null

  @CliOption(names = Array("--delay"), description = Array("Frame delay, e.g. 100ms"))
  var delay: String = "100ms"

  @CliOption(names = Array("--preview-only"), description = Array("Render locally without uploading"))
  var previewOnly: Boolean = false

  override def run(): Unit =
    MatrixCommands(parent.context).frame(path, delay, previewOnly)

@Command(name = "animation", description = Array("Upload up to 8 frames for the device to loop locally"))
class MatrixAnimationCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0..*", paramLabel = "PATH", description = Array("Frame files, in play order"))
  var paths: java.util.List[String] = new java.util.ArrayList[String]()

  @CliOption(names = Array("--delay"), description = Array("Delay between frames, e.g. 80ms"))
  var delay: String = "80ms"

  override def run(): Unit =
    MatrixCommands(parent.context).animation(paths.asScala.toSeq, delay)

@Command(name = "pixel", description = Array("Set one pixel: pixel X Y COLOR"))
class MatrixPixelCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0", paramLabel = "X")
  var x: Integer = null

  @Parameters(index = "1", paramLabel = "Y")
  var y: Integer = null

  @Parameters(index = "2", paramLabel = "COLOR")
  var color: String = null

  override def run(): Unit =
    MatrixCommands(parent.context).pixel(x.intValue(), y.intValue(), color)

@Command(name = "panel", description = Array("Blank or restore output without discarding the image: panel on|off"))
class MatrixPanelCommand extends Runnable:
  @ParentCommand
  private var parent: MatrixCommand = null

  @Parameters(index = "0", paramLabel = "STATE", description = Array("on or off"))
  var state: String = null

  override def run(): Unit =
    MatrixCommands(parent.context).panel(state)

@Command(
  name = "background",
  description = Array("Background animation commands"),
  subcommands = Array(
    classOf[BackgroundGetCommand],
    classOf[BackgroundSetCommand]
  )
)
class BackgroundCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  def context: CliContext = parent.context

  override def run(): Unit =
    // Resolve config for its side effect: a broken config file should be
    // reported as such rather than hidden behind the usage message.
    val _ = parent.context
    throw CliFailure(ExitCode.Usage, "background get|set")

@Command(name = "get", description = Array("Get background state"))
class BackgroundGetCommand extends Runnable:
  @ParentCommand
  private var parent: BackgroundCommand = null

  override def run(): Unit =
    BackgroundCommands(parent.context).get()

@Command(name = "set", description = Array("Set background animation"))
class BackgroundSetCommand extends Runnable:
  @ParentCommand
  private var parent: BackgroundCommand = null

  @Parameters(index = "0", paramLabel = "ANIMATION")
  var animation: String = null

  @CliOption(names = Array("--no-restore"))
  var noRestore: Boolean = false

  override def run(): Unit =
    BackgroundCommands(parent.context).set(animation, noRestore)

@Command(name = "queue", description = Array("Queue commands"))
class QueueCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  @CliOption(names = Array("--clear"), description = Array("Clear queue"))
  var clear: Boolean = false

  override def run(): Unit =
    val queue = QueueCommands(parent.context)
    if clear then queue.clear() else queue.show()

@Command(
  name = "config",
  description = Array("Config profile commands"),
  subcommands = Array(
    classOf[ConfigListCommand],
    classOf[ConfigShowCommand],
    classOf[ConfigInitCommand],
    classOf[ConfigUseCommand]
  )
)
class ConfigCommand extends Runnable:
  @ParentCommand
  private var parent: EchoCtl = null

  def context: CliContext = parent.context

  override def run(): Unit =
    // Resolve config for its side effect: a broken config file should be
    // reported as such rather than hidden behind the usage message.
    val _ = parent.context
    throw CliFailure(ExitCode.Usage, "config list|show|init|use")

@Command(name = "list", description = Array("List config profiles"))
class ConfigListCommand extends Runnable:
  @ParentCommand
  private var parent: ConfigCommand = null

  override def run(): Unit =
    ConfigCommands(parent.context).list()

@Command(name = "show", description = Array("Show current config"))
class ConfigShowCommand extends Runnable:
  @ParentCommand
  private var parent: ConfigCommand = null

  override def run(): Unit =
    ConfigCommands(parent.context).show()

@Command(name = "init", description = Array("Initialize profile"))
class ConfigInitCommand extends Runnable:
  @ParentCommand
  private var parent: ConfigCommand = null

  @Parameters(index = "0", paramLabel = "PROFILE")
  var profile: String = null

  @CliOption(names = Array("--server"))
  var server: String = null

  @CliOption(names = Array("--device"))
  var device: String = null

  @CliOption(names = Array("--token"))
  var token: String = null

  override def run(): Unit =
    ConfigCommands(parent.context).init(
      profile,
      Option(server),
      Option(device),
      Option(token)
    )

@Command(name = "use", description = Array("Switch active profile"))
class ConfigUseCommand extends Runnable:
  @ParentCommand
  private var parent: ConfigCommand = null

  @Parameters(index = "0", paramLabel = "PROFILE")
  var profile: String = null

  override def run(): Unit =
    ConfigCommands(parent.context).use(profile)
