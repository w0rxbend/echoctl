package com.worxbend.echoctl

import com.worxbend.echoctl.api.Rgb
import com.worxbend.echoctl.cli.MatrixCommands
import com.worxbend.echoctl.render.ParsedFrame
import utest.{TestSuite, Tests, test}

/** Covers the pixel-grid → palette-and-rows conversion the upload path performs.
  *
  * The API speaks palette-and-rows so that uploaded animations use the same
  * vocabulary as config-authored ones. That conversion is the only real logic in
  * the CLI's upload path, so it is where the tests belong.
  */
object MatrixUploadSpecTests extends TestSuite:

  private val black = Rgb(0, 0, 0)
  private val green = Rgb(0, 255, 85)
  private val red = Rgb(255, 0, 0)

  private def frameOf(lit: Rgb, litRows: Set[Int]): ParsedFrame =
    ParsedFrame(
      rows = (0 until 8).map(y => (0 until 8).map(_ => if litRows.contains(y) then lit else black)),
      palette = Map.empty
    )

  val tests: Tests = Tests {
    test("black is pinned to '.' and colours get stable symbols") {
      val request = MatrixCommands.buildUpload(Seq(frameOf(green, Set(0))), "80ms") match
        case Right(value) => value
        case Left(message) => throw new AssertionError(message)

      assert(request.palette.get(".").contains("#000000"))
      assert(request.palette.values.toSet.contains("#00FF55"))
      assert(request.frames.length == 1)
      val rows = request.frames.head.rows
      assert(rows.length == 8)
      assert(rows.forall(_.length == 8))
      // Row 0 is fully lit, the rest are black.
      assert(rows.head.distinct.length == 1 && rows.head.head != '.')
      assert(rows.tail.forall(_ == "........"))
    }

    test("every symbol used in a row exists in the palette") {
      val request = MatrixCommands.buildUpload(Seq(frameOf(green, Set(1)), frameOf(red, Set(2))), "80ms") match
        case Right(value) => value
        case Left(message) => throw new AssertionError(message)

      val used = request.frames.flatMap(_.rows).flatten.map(_.toString).toSet
      val declared = request.palette.keySet
      assert(used.subsetOf(declared))
    }

    test("the palette is shared across frames so a colour keeps one symbol") {
      val request = MatrixCommands.buildUpload(Seq(frameOf(green, Set(0)), frameOf(green, Set(7))), "80ms") match
        case Right(value) => value
        case Left(message) => throw new AssertionError(message)

      val firstSymbol = request.frames.head.rows.head.head
      val secondSymbol = request.frames(1).rows(7).head
      assert(firstSymbol == secondSymbol)
      // green + black only
      assert(request.palette.size == 2)
    }

    test("the declared delay reaches every frame") {
      val request = MatrixCommands.buildUpload(Seq(frameOf(green, Set(0)), frameOf(red, Set(1))), "120ms") match
        case Right(value) => value
        case Left(message) => throw new AssertionError(message)
      assert(request.frames.forall(_.delay == "120ms"))
    }

    test("more frames than the firmware slot holds is rejected") {
      val frames = (0 to MatrixCommands.MaxAnimationFrames).map(_ => frameOf(green, Set(0)))
      val result = MatrixCommands.buildUpload(frames, "80ms")
      assert(result.isLeft)
      assert(result.left.exists(_.contains(MatrixCommands.MaxAnimationFrames.toString)))
    }

    test("an empty frame list is rejected") {
      assert(MatrixCommands.buildUpload(Seq.empty, "80ms").isLeft)
    }

    test("a frame that is not 8x8 is rejected") {
      val ragged = ParsedFrame(rows = Seq(Seq(green, black)), palette = Map.empty)
      val result = MatrixCommands.buildUpload(Seq(ragged), "80ms")
      assert(result.isLeft)
      assert(result.left.exists(_.contains("8x8")))
    }
  }
