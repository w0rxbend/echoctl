package com.worxbend.echoctl

import com.worxbend.echoctl.api.EchoClient
import ujson.{Obj, Value}
import utest.{TestSuite, Tests, test}

import scala.util.Try

private object FakeEchoBackend extends cask.MainRoutes:
  var portValue: Int = 0
  override def port: Int = portValue

  private var lastAuth: Option[String] = None
  private var lastFill: Option[(String, Value)] = None
  private var lastPlayAnimation: Option[String] = None
  private var lastPresetPath: Option[String] = None
  private var lastQueueDevice: Option[String] = None

  def reset(): Unit =
    lastAuth = None
    lastFill = None
    lastPlayAnimation = None
    lastPresetPath = None
    lastQueueDevice = None

  def lastAuthorization: Option[String] = lastAuth
  def lastFillPayload: Option[(String, Value)] = lastFill
  def lastPlayAnimationName: Option[String] = lastPlayAnimation
  def lastPresetDevice: Option[String] = lastPresetPath
  def lastQueueRequestedDevice: Option[String] = lastQueueDevice

  private def unauthorizedResponse(reason: String) =
    cask.Response(ujson.write(Obj("error" -> reason)), 401, Seq("Content-Type" -> "application/json"))

  private def jsonResponse(value: Value) =
    cask.Response(ujson.write(value), 200, Seq("Content-Type" -> "application/json"))

  private def parseAuth(req: cask.Request): Option[String] =
    req.headers.get("Authorization").orElse(req.headers.get("authorization")).flatMap(_.headOption)

  @cask.get("/healthz")
  def healthz() = jsonResponse(Obj("status" -> "ok"))

  @cask.get("/readyz")
  def readyz() = jsonResponse(
    Obj(
      "status" -> "ok",
      "workers_running" -> true,
      "draining" -> false,
      "devices" -> Obj(
        "living-room" -> Obj(
          "scheduler_state" -> "running",
          "matrix_connected" -> true,
          "background" -> Obj("state" -> "idle")
        )
      )
    )
  )

  @cask.get("/api/v1/devices")
  def devices() = jsonResponse(Obj("devices" -> ujson.Arr("living-room", "desk")))

  @cask.get("/api/v1/animations")
  def animations() = jsonResponse(Obj("animations" -> ujson.Arr("alert_pulse", "matrix_rain_background")))

  @cask.get("/api/v1/animations/catalog")
  def catalog() = jsonResponse(
    Obj(
      "animations" -> ujson.Arr(
        Obj(
          "id" -> "alert_pulse",
          "kind" -> "generated",
          "playable" -> true
        ),
        Obj(
          "id" -> "matrix_rain_background",
          "kind" -> "firmware_preset",
          "playable" -> false
        )
      )
    )
  )

  @cask.post("/api/v1/devices/:device/notify")
  def notifyDevice(device: String, request: cask.Request) =
    cask.Response(
      ujson.write(Obj("event_id" -> s"notify-$device")),
      200,
      Seq("Content-Type" -> "application/json")
    )

  @cask.post("/api/v1/devices/:device/events")
  def eventDevice(device: String, request: cask.Request) =
    cask.Response(
      ujson.write(Obj("event_id" -> s"event-$device")),
      200,
      Seq("Content-Type" -> "application/json")
    )

  @cask.post("/api/v1/devices/:device/play")
  def playDevice(device: String, request: cask.Request) =
    parseAndRecordPlay(request.text(), device)

  private def parseAndRecordPlay(body: String, device: String) =
    Try(ujson.read(body)).toOption match
      case Some(value) =>
        lastPlayAnimation = value.obj.get("animation").flatMap(_.str)
        lastQueueDevice = Some(device)
        jsonResponse(Obj("request_id" -> "ok"))
      case None =>
        cask.Response(ujson.write(Obj("error" -> "invalid json")), 400, Seq("Content-Type" -> "application/json"))

  @cask.post("/api/v1/devices/:device/preset/:animation")
  def playPreset(device: String, animation: String) =
    lastPresetPath = Some(device)
    if animation.startsWith("missing") then unauthorizedResponse("not found")
    else
      lastQueueDevice = Some(device)
      jsonResponse(Obj("status" -> "ok", "animation" -> animation))

  @cask.post("/api/v1/devices/:device/matrix/preset")
  def matrixPreset(device: String, request: cask.Request) =
    cask.Response(ujson.write(Obj("status" -> "ok")), 200, Seq("Content-Type" -> "application/json"))

  @cask.post("/api/v1/devices/:device/matrix/fill")
  def matrixFill(device: String, request: cask.Request) =
    lastAuth = parseAuth(request)
    lastFill = Some(device -> ujson.read(request.text()))
    jsonResponse(Obj("status" -> "ok"))

  @cask.post("/api/v1/devices/:device/matrix/clear")
  def matrixClear(device: String, request: cask.Request) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("status" -> "ok"))

  @cask.post("/api/v1/devices/:device/matrix/brightness")
  def matrixBrightness(device: String, request: cask.Request) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("status" -> "ok"))

  @cask.get("/api/v1/devices/:device/background")
  def getBackground(device: String, request: cask.Request) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("animation" -> "matrix_rain_background", "restore_on_idle" -> true))

  @cask.put("/api/v1/devices/:device/background")
  def setBackground(device: String, request: cask.Request) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("status" -> "ok", "animation" -> "ok"))

  @cask.get("/api/v1/devices/:device/queue")
  def getQueue(device: String) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("state" -> "idle", "depth" -> 0, "items" -> ujson.Arr()))

  @cask.delete("/api/v1/devices/:device/queue")
  def clearQueue(device: String) =
    lastQueueDevice = Some(device)
    jsonResponse(Obj("cleared" -> 0))

  initialize()

private final class FakeServer(port: Int):
  FakeEchoBackend.portValue = port

  private val thread = new Thread(() => FakeEchoBackend.main(Array.empty))
  thread.setDaemon(true)
  thread.start()

  def baseUrl: String = s"http://localhost:$port"

  def shutdown(): Unit =
    thread.interrupt()

object ClientSpecTests extends TestSuite:
  private def freePort(): Int =
    val socket = new java.net.ServerSocket(0)
    val port = socket.getLocalPort
    socket.close()
    port

  private def waitForStart(base: String): Unit =
    var remaining = 100
    while remaining > 0 do
      Try(requests.get(s"$base/healthz").statusCode == 200) match
        case scala.util.Success(true) => return
        case _ =>
          Thread.sleep(25)
          remaining -= 1
    throw new RuntimeException("fake server did not start")

  val tests = Tests(
    test("client includes bearer header and payload on matrix fill") {
      val server = FakeServer(freePort())
      try
        val base = server.baseUrl
        waitForStart(base)
        FakeEchoBackend.reset()

        val client = EchoClient(s"$base", Some("unit-token"))
        val response = client.matrixFill("living-room", com.worxbend.echoctl.api.ColorRequest(10, 20, 30))

        assert(response.isRight)
        assert(FakeEchoBackend.lastFillPayload.exists { case (device, body) =>
          device == "living-room" &&
            body("r").num.toInt == 10 &&
            body("g").num.toInt == 20 &&
            body("b").num.toInt == 30
        })
        assert(FakeEchoBackend.lastAuthorization.contains("Bearer unit-token"))
      finally
        server.shutdown()
    },

    test("client enforces HTTP errors from server") {
      val server = FakeServer(freePort())
      try
        val base = server.baseUrl
        waitForStart(base)

        val client = EchoClient(s"$base", Some("unit-token"))
        val response = client.playPreset("living-room", "missing-animation")
        assert(response.isLeft)
        response match
          case scala.util.Left(error) =>
            assert(error.exitCode == 1)
          case _ => assert(false)
      finally
        server.shutdown()
    },

    test("client requests catalog and list endpoints") {
      val server = FakeServer(freePort())
      try
        val base = server.baseUrl
        waitForStart(base)

        val client = EchoClient(s"$base", None)
        val animations = client.listAnimations()
        val catalog = client.catalogAnimations()
        val ready = client.ready()

        assert(animations.isRight)
        assert(catalog.isRight)
        assert(ready.isRight)
      finally
        server.shutdown()
    }
  )
