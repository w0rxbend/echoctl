package com.worxbend.echoctl.api

import requests.Session
import upickle.default._

import scala.util.{Failure, Success, Try}

final case class EchoClient(baseUrl: String, token: Option[String]):
  private val base = normalize(baseUrl)
  private val session = Session()

  private def normalize(raw: String): String =
    val trimmed = raw.trim
    if trimmed.endsWith("/") then trimmed.dropRight(1) else trimmed

  private def authHeaders: Map[String, String] =
    token
      .filter(_.nonEmpty)
      .map(t => Map("Authorization" -> s"Bearer $t"))
      .getOrElse(Map.empty)

  private val commonHeaders: Map[String, String] =
    Map("Content-Type" -> "application/json", "Accept" -> "*/*")

  private def url(path: String): String =
    val suffix = if path.startsWith("/") then path else s"/$path"
    s"$base$suffix"

  private def decodeError(path: String, body: String): Option[String] =
    Try(read[ErrorResponse](body)).toOption.map(_.error)

  private def request(
    method: String,
    path: String,
    body: Option[String] = None
  ): Either[ApiError, requests.Response] =
    Try {
      val route = url(path)
      method match
        case "GET" =>
          session.get(
            route,
            headers = commonHeaders ++ authHeaders,
            readTimeout = 10000,
            connectTimeout = 5000,
            check = false
          )
        case "DELETE" =>
          session.delete(
            route,
            headers = commonHeaders ++ authHeaders,
            readTimeout = 10000,
            connectTimeout = 5000,
            check = false
          )
        case "POST" =>
          session.post(
            route,
            headers = commonHeaders ++ authHeaders,
            data = body.getOrElse("{}"),
            readTimeout = 10000,
            connectTimeout = 5000,
            check = false
          )
        case "PUT" =>
          session.put(
            route,
            headers = commonHeaders ++ authHeaders,
            data = body.getOrElse("{}"),
            readTimeout = 10000,
            connectTimeout = 5000,
            check = false
          )
        case other =>
          throw new IllegalArgumentException(s"unsupported method $other")
    } match
      case Success(response) => Right(response)
      case Failure(error) => Left(ConnectionError(path, error.toString))

  private def withDecode[T: Reader](path: String, response: requests.Response): Either[ApiError, ApiResponse[T]] =
    val text = response.text()
    if response.statusCode < 200 || response.statusCode >= 300 then
      Left(ApiError.from(response.statusCode, path, text, decodeError(path, text)))
    else
      Try(read[T](text)) match
        case Success(value) => Right(ApiResponse(response.statusCode, path, text, value))
        case Failure(error) => Left(DecodeError(path, text, error.toString))

  private def withText(path: String, response: requests.Response): Either[ApiError, String] =
    val text = response.text()
    if response.statusCode < 200 || response.statusCode >= 300 then
      Left(ApiError.from(response.statusCode, path, text, decodeError(path, text)))
    else
      Right(text)

  def get[T: Reader](path: String): Either[ApiError, ApiResponse[T]] =
    request("GET", path).flatMap(withDecode(path, _))

  def post[Req: Writer, Res: Reader](path: String, body: Req): Either[ApiError, ApiResponse[Res]] =
    request("POST", path, Some(write(body)))
      .flatMap(withDecode(path, _))

  def put[Req: Writer, Res: Reader](path: String, body: Req): Either[ApiError, ApiResponse[Res]] =
    request("PUT", path, Some(write(body)))
      .flatMap(withDecode(path, _))

  def delete[T: Reader](path: String): Either[ApiError, ApiResponse[T]] =
    request("DELETE", path).flatMap(withDecode(path, _))

  def getText(path: String): Either[ApiError, String] =
    request("GET", path).flatMap(withText(path, _))

  def health(): Either[ApiError, ApiResponse[HealthResponse]] =
    get[HealthResponse]("/healthz")

  def ready(): Either[ApiError, ApiResponse[ReadyzResponse]] =
    get[ReadyzResponse]("/readyz")

  def listDevices(): Either[ApiError, ApiResponse[DeviceListResponse]] =
    get[DeviceListResponse]("/api/v1/devices")

  def listAnimations(): Either[ApiError, ApiResponse[AnimationListResponse]] =
    get[AnimationListResponse]("/api/v1/animations")

  def catalogAnimations(): Either[ApiError, ApiResponse[AnimationCatalogResponse]] =
    get[AnimationCatalogResponse]("/api/v1/animations/catalog")

  def notify(device: String, request: NotifyRequest): Either[ApiError, ApiResponse[EventAccepted]] =
    post[NotifyRequest, EventAccepted](s"/api/v1/devices/$device/notify", request)

  def event(device: String, request: EventRequest): Either[ApiError, ApiResponse[EventAccepted]] =
    post[EventRequest, EventAccepted](s"/api/v1/devices/$device/events", request)

  def play(device: String, request: PlayRequest): Either[ApiError, ApiResponse[RequestAccepted]] =
    post[PlayRequest, RequestAccepted](s"/api/v1/devices/$device/play", request)

  def playPreset(device: String, animation: String): Either[ApiError, ApiResponse[StatusOKPreset]] =
    post[Map[String, String], StatusOKPreset](s"/api/v1/devices/$device/preset/$animation", Map.empty)

  def matrixPreset(device: String, request: PresetRequest): Either[ApiError, ApiResponse[StatusOK]] =
    post[PresetRequest, StatusOK](s"/api/v1/devices/$device/matrix/preset", request)

  def matrixFill(device: String, request: ColorRequest): Either[ApiError, ApiResponse[StatusOK]] =
    post[ColorRequest, StatusOK](s"/api/v1/devices/$device/matrix/fill", request)

  def matrixClear(device: String): Either[ApiError, ApiResponse[StatusOK]] =
    post[Map[String, String], StatusOK](s"/api/v1/devices/$device/matrix/clear", Map.empty)

  def matrixBrightness(device: String, request: BrightnessRequest): Either[ApiError, ApiResponse[StatusOK]] =
    post[BrightnessRequest, StatusOK](s"/api/v1/devices/$device/matrix/brightness", request)

  def getBackground(device: String): Either[ApiError, ApiResponse[BackgroundState]] =
    get[BackgroundState](s"/api/v1/devices/$device/background")

  def setBackground(device: String, request: BackgroundState): Either[ApiError, ApiResponse[StatusOKPreset]] =
    put[BackgroundState, StatusOKPreset](s"/api/v1/devices/$device/background", request)

  def getQueue(device: String): Either[ApiError, ApiResponse[QueueResponse]] =
    get[QueueResponse](s"/api/v1/devices/$device/queue")

  def clearQueue(device: String): Either[ApiError, ApiResponse[QueueClearResponse]] =
    delete[QueueClearResponse](s"/api/v1/devices/$device/queue")

  def openapiSpec(): Either[ApiError, ApiResponse[ujson.Value]] =
    get[ujson.Value]("/openapi.json")
