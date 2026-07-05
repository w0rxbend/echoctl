package com.worxbend.echoctl.api

import upickle.default._

type UjsonValue = ujson.Value

case class StatusResponse(status: String) derives ReadWriter

case class HealthResponse(status: String) derives ReadWriter

case class Rgb(r: Int, g: Int, b: Int) derives ReadWriter {
  def toEffectMap: Map[String, Int] = Map("r" -> r, "g" -> g, "b" -> b)
}

case class NotifyRequest(
  title: Option[String] = None,
  message: String,
  duration: Option[String] = None,
  animation: Option[String] = None,
  level: Option[String] = None,
  params: Option[Map[String, String]] = None,
  priority: Option[Int] = None,
  restore: Option[String] = None
) derives ReadWriter

case class EventRequest(
  source: Option[String] = None,
  `type`: Option[String] = None,
  id: Option[String] = None,
  text: Option[String] = None,
  channel: Option[String] = None,
  target: Option[String] = None,
  actor: Option[String] = None,
  priority: Option[Int] = None,
  attributes: Option[Map[String, String]] = None
) derives ReadWriter

case class PlayRequest(
  animation: String,
  duration: Option[String] = None,
  interrupt_mode: Option[String] = None,
  params: Option[Map[String, String]] = None,
  priority: Option[Int] = None,
  restore: Option[String] = None
) derives ReadWriter

case class PresetRequest(
  effect_id: Int,
  interval: Option[String] = None,
  r: Int,
  g: Int,
  b: Int
) derives ReadWriter

case class ColorRequest(
  r: Int,
  g: Int,
  b: Int
) derives ReadWriter

case class BrightnessRequest(value: Int) derives ReadWriter

case class AnimationCatalogEntry(
  id: String,
  kind: Option[String] = None,
  playable: Option[Boolean] = None,
  effect_id: Option[Int] = None,
  interval: Option[String] = None,
  color: Option[Rgb] = None
) derives ReadWriter

case class AnimationCatalogResponse(animations: Seq[AnimationCatalogEntry]) derives ReadWriter

case class AnimationListResponse(animations: Seq[String]) derives ReadWriter

case class DeviceListResponse(devices: Seq[String]) derives ReadWriter

case class EventAccepted(event_id: String) derives ReadWriter

case class RequestAccepted(request_id: String) derives ReadWriter

case class StatusOK(status: String) derives ReadWriter

case class StatusOKPreset(status: String, animation: Option[String] = None) derives ReadWriter

case class QueueResponse(
  state: String,
  depth: Int,
  items: Seq[UjsonValue] = Seq.empty
) derives ReadWriter

case class QueueClearResponse(cleared: Int) derives ReadWriter

case class ErrorResponse(error: String) derives ReadWriter

case class BackgroundState(animation: Option[String] = None, restore_on_idle: Option[Boolean] = None) derives ReadWriter

case class BackgroundReady(
  configured_id: Option[String] = None,
  kind: Option[String] = None,
  state: Option[String] = None,
  dirty: Option[Boolean] = None,
  converged: Option[Boolean] = None,
  last_attempt: Option[String] = None,
  last_success: Option[String] = None,
  next_retry: Option[String] = None,
  failure_count: Option[Int] = None,
  last_error: Option[String] = None,
  last_error_class: Option[String] = None
) derives ReadWriter

case class DeviceReady(
  scheduler_state: Option[String] = None,
  matrix_connected: Option[Boolean] = None,
  background: Option[BackgroundReady] = None,
  last_success: Option[String] = None,
  last_failure: Option[String] = None
) derives ReadWriter

case class EventWorkerState(
  state: Option[String] = None,
  stage: Option[String] = None,
  active_duration_seconds: Option[Double] = None,
  active_since: Option[String] = None
) derives ReadWriter

case class ReadyzResponse(
  status: String,
  workers_running: Option[Boolean] = None,
  draining: Option[Boolean] = None,
  event_worker: Option[EventWorkerState] = None,
  devices: Map[String, DeviceReady] = Map.empty,
  outcome_reports_dropped: Option[Long] = None,
  outcome_recording_panics: Option[Long] = None,
  tcp_reconnect_log_events_dropped: Option[Long] = None,
  observability_callback_panics: Option[Long] = None,
  observability_callback_panic_counts: Option[Map[String, Long]] = None
) derives ReadWriter

case class MatrixFrameRequest(
  name: String,
  frames: Seq[Seq[Rgb]]
) derives ReadWriter
