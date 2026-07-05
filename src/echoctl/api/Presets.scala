package com.worxbend.echoctl.api

object Presets:
  val effects: Map[Int, String] = Map(
    0 -> "stop",
    1 -> "chase",
    2 -> "bounce",
    3 -> "comet",
    4 -> "fire",
    5 -> "ripple",
    6 -> "pulse",
    7 -> "spark",
    8 -> "twinkle",
    9 -> "rainbow",
    10 -> "halo",
    11 -> "scan",
    12 -> "wave",
    13 -> "matrix",
    14 -> "plasma",
    15 -> "noise",
    16 -> "flicker",
    17 -> "rain",
    18 -> "drip",
    19 -> "glow",
    20 -> "strobe",
    21 -> "spiral",
    22 -> "confetti"
  )

  private val normalized: Map[String, Int] =
    effects.view.mapValues(_.toLowerCase).toMap ++ Map("stop" -> 0)

  def resolve(raw: String): Option[Int] =
    val normalizedValue = raw.trim.toLowerCase.replace('-', '_')
    normalized.get(normalizedValue) match
      case some @ Some(_) => some
      case None =>
        normalizedValue.toIntOption match
          case Some(int) if effects.contains(int) => Some(int)
          case _ => None

  def name(id: Int): String =
    effects.getOrElse(id, s"effect_${id}")
