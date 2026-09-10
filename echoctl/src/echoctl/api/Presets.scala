package com.worxbend.echoctl.api

/** Firmware effect ids.
  *
  * This table is the firmware's, not the CLI's. The authority is
  * `TcpMatrixServer::applyCommand` in led-matrix-controller, documented in that
  * repo's `CLIENT_PROTOCOL.md`; `PresetSpecTests` asserts every entry — including
  * the retired and colour-ignoring flags — against `spec/firmware-effects.tsv`, so
  * the two cannot drift.
  *
  * Do not invent names here. A name that does not exist in the firmware resolves to
  * an id the device interprets as a different effect, which fails silently — the
  * panel simply plays the wrong animation.
  *
  * Ids are never renumbered: configs and the server's animation registry pin them.
  */
object Presets:
  /** Highest effect id the firmware implements. Anything above this is rejected by
    * the device with status 0x04 (invalid payload).
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

  /** Effects that still work and still resolve, but are parameter variants of
    * another effect rather than a distinct look on a 64-pixel panel. Hidden from
    * help and listings so nobody picks one by mistake; never renumbered, so
    * existing configs keep working.
    */
  val retired: Map[Int, String] = Map(
    1 -> "comet",
    4 -> "diagonal",
    5 -> "twinkle",
    6 -> "comet",
    10 -> "twinkle",
    21 -> "color_wipe"
  )

  /** Effects that accept the RGB payload and discard it, because the renderer
    * computes its own colours. The device still answers OK, so without this the
    * only symptom is that `--color` silently does nothing.
    */
  val ignoresColor: Set[Int] = Set(7, 11, 17, 22)

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

  def isRetired(id: Int): Boolean = retired.contains(id)

  /** The effect this retired id is a variant of. */
  def replacementFor(id: Int): Option[String] = retired.get(id)

  def ignoresColorFor(id: Int): Boolean = ignoresColor.contains(id)

  /** Effect names a user should choose from, in id order: everything except the
    * stop sentinel and the retired variants.
    */
  def names: Seq[String] =
    effects.toSeq.sortBy(_._1).collect { case (id, n) if id != 0 && !isRetired(id) => n }

  /** Every name in id order, retired included — for error messages that need to
    * explain why a name was accepted.
    */
  def allNames: Seq[String] = effects.toSeq.sortBy(_._1).map(_._2)
