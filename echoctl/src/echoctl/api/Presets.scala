package com.worxbend.echoctl.api

/** Firmware effect ids.
  *
  * This table is the firmware's, not the CLI's. The authority is
  * `TcpMatrixServer::applyCommand` in led-matrix-controller, documented in that
  * repo's `CLIENT_PROTOCOL.md`; `PresetSpecTests` asserts every entry against
  * that document so the two cannot drift again.
  *
  * Do not invent names here. A name that does not exist in the firmware resolves
  * to an id the device interprets as a different effect, which fails silently —
  * the panel simply plays the wrong animation.
  */
object Presets:
  /** Highest effect id the firmware implements. Anything above this is rejected
    * by the device with status 0x04 (invalid payload).
    */
  val MaxEffectId: Int = 22

  val effects: Map[Int, String] = Map(
    0 -> "stop",
    1 -> "chase",
    2 -> "color_wipe",
    3 -> "blink",
    4 -> "wave",
    5 -> "rain",
    6 -> "meteor",
    7 -> "rainbow",
    8 -> "breathing",
    9 -> "scanner",
    10 -> "sparkle",
    11 -> "fire",
    12 -> "matrix_rain",
    13 -> "ripple",
    14 -> "theater_chase",
    15 -> "twinkle",
    16 -> "comet",
    17 -> "plasma",
    18 -> "diagonal",
    19 -> "border_chase",
    20 -> "heartbeat",
    21 -> "pulse_wipe",
    22 -> "confetti"
  )

  private val normalized: Map[String, Int] =
    effects.map { case (id, name) => name.toLowerCase -> id }

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

  /** Effect names in id order, for help text and error messages. */
  def names: Seq[String] =
    effects.toSeq.sortBy(_._1).map(_._2)
