package com.worxbend.echoctl

import com.worxbend.echoctl.api.Presets
import utest.{TestSuite, Tests, test}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Guards the CLI's effect table against the firmware's.
  *
  * The firmware owns these ids. When the two disagree the failure is silent —
  * `echoctl effect fire` simply plays whatever effect the device has at that id —
  * so the agreement has to be asserted, not assumed.
  */
object PresetSpecTests extends TestSuite:

  /** Walks up from the working directory to find the vendored contract, so the
    * test does not depend on where the build runner starts.
    */
  private def firmwareEffectsFile: Path =
    val relative = Paths.get("spec", "firmware-effects.tsv")
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .getOrElse(
        throw new AssertionError(
          s"could not locate $relative by walking up from ${Paths.get("").toAbsolutePath}"
        )
      )

  private lazy val firmwareEffects: Map[Int, String] =
    Files
      .readAllLines(firmwareEffectsFile)
      .asScala
      .map(_.trim)
      .filter(line => line.nonEmpty && !line.startsWith("#"))
      .map { line =>
        line.split("\t").toList match
          case id :: name :: Nil => id.trim.toInt -> name.trim
          case _ => throw new AssertionError(s"malformed effect row: '$line'")
        }
      .toMap

  val tests: Tests = Tests {
    test("the vendored firmware table is non-empty and covers 0..MaxEffectId") {
      assert(firmwareEffects.nonEmpty)
      val expectedIds = (0 to Presets.MaxEffectId).toSet
      assert(firmwareEffects.keySet == expectedIds)
    }

    test("every CLI effect name matches the firmware at the same id") {
      val mismatched = firmwareEffects.toSeq.sortBy(_._1).collect {
        case (id, firmwareName) if Presets.effects.get(id) != Some(firmwareName) =>
          s"id $id: firmware '$firmwareName' but CLI '${Presets.effects.getOrElse(id, "<missing>")}'"
      }
      assert(mismatched.isEmpty)
    }

    test("the CLI invents no effect the firmware does not implement") {
      val extra = (Presets.effects.keySet -- firmwareEffects.keySet).toSeq.sorted
      assert(extra.isEmpty)
      val unknownNames = Presets.effects.values.toSet -- firmwareEffects.values.toSet
      assert(unknownNames.isEmpty)
    }

    test("resolve accepts firmware names, hyphenated aliases, and raw ids") {
      assert(Presets.resolve("matrix_rain").contains(12))
      assert(Presets.resolve("matrix-rain").contains(12))
      assert(Presets.resolve("MATRIX_RAIN").contains(12))
      assert(Presets.resolve("12").contains(12))
      assert(Presets.resolve("stop").contains(0))
    }

    test("resolve rejects ids the firmware would reject") {
      assert(Presets.resolve((Presets.MaxEffectId + 1).toString).isEmpty)
      assert(Presets.resolve("255").isEmpty)
      assert(Presets.resolve("-1").isEmpty)
    }

    test("resolve rejects names that are not firmware effects") {
      // These were in the CLI's old hand-written table and are not firmware
      // effects; each one used to resolve to an unrelated id.
      for name <- Seq("bounce", "pulse", "spark", "halo", "scan", "noise", "flicker", "drip", "glow", "strobe", "spiral", "matrix") do
        assert(Presets.resolve(name).isEmpty)
    }

    test("names are listed in id order") {
      assert(Presets.names.head == "stop")
      assert(Presets.names.last == "confetti")
      assert(Presets.names.length == Presets.MaxEffectId + 1)
    }
  }
