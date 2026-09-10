package com.worxbend.echoctl

import com.worxbend.echoctl.render.FramePreview
import com.worxbend.echoctl.api.Rgb
import utest.{TestSuite, Tests, test}

object FrameSpecTests extends TestSuite:
  private def writeFrame(content: String): String =
    val dir = os.temp.dir()
    val path = dir / "frame.txt"
    os.write.over(path, content)
    path.toString

  private def assertRows(rows: Seq[Seq[Rgb]], expectedRows: Int = 8, expectedCols: Int = 8): Unit =
    assert(rows.size == expectedRows)
    assert(rows.forall(_.size == expectedCols))

  val tests = Tests {
    test("parses explicit palette and rows section") {
      val path = writeFrame(
        """palette:
          |  off: "#000000"
          |  red: red
          |rows:
          |  - [off,off,off,off,off,off,off,off]
          |  - [red, red, red, red, red, red, red, red]
          |  - [off, off, off, off, off, off, off, off]
          |  - [red, red, red, red, red, red, red, red]
          |  - [off, off, off, off, off, off, off, off]
          |  - [red, red, red, red, red, red, red, red]
          |  - [off, off, off, off, off, off, off, off]
          |  - [red, red, red, red, red, red, red, red]
          |""".stripMargin
      )

      val parsed = FramePreview.load(path)
      assert(parsed.isRight)
      val frame = parsed.toOption.get
      assertRows(frame.rows)
      assert(frame.palette("off") == Rgb(0, 0, 0))
      assert(frame.palette("red") == Rgb(255, 0, 0))
    }

    test("parses legacy token stream into 8x8") {
      val content = Seq.fill(64)("#ff0000").mkString(" ")
      val path = writeFrame(content)
      val parsed = FramePreview.load(path)
      assert(parsed.isRight)
      val frame = parsed.toOption.get
      assertRows(frame.rows)
      assert(frame.rows(0).head == Rgb(255, 0, 0))
    }

    test("rejects non-8x8 frame content") {
      val bad = writeFrame("#ff0000 #00ff00")
      assert(FramePreview.load(bad).isLeft)
    }

    test("parses comments and numeric rgb() syntax") {
      val path = writeFrame(
        """
          |# frame comment
          |rows:
          |  - [rgb(1,2,3), rgb(4,5,6), rgb(7,8,9), rgb(10,11,12), rgb(13,14,15), rgb(16,17,18), rgb(19,20,21), rgb(22,23,24)]
          |  - [rgb(25,26,27), rgb(28,29,30), rgb(31,32,33), rgb(34,35,36), rgb(37,38,39), rgb(40,41,42), rgb(43,44,45), rgb(46,47,48)]
          |  - [rgb(49,50,51), rgb(52,53,54), rgb(55,56,57), rgb(58,59,60), rgb(61,62,63), rgb(64,65,66), rgb(67,68,69), rgb(70,71,72)]
          |  - [rgb(73,74,75), rgb(76,77,78), rgb(79,80,81), rgb(82,83,84), rgb(85,86,87), rgb(88,89,90), rgb(91,92,93), rgb(94,95,96)]
          |  - [rgb(97,98,99), rgb(100,101,102), rgb(103,104,105), rgb(106,107,108), rgb(109,110,111), rgb(112,113,114), rgb(115,116,117), rgb(118,119,120)]
          |  - [rgb(121,122,123), rgb(124,125,126), rgb(127,128,129), rgb(130,131,132), rgb(133,134,135), rgb(136,137,138), rgb(139,140,141), rgb(142,143,144)]
          |  - [rgb(145,146,147), rgb(148,149,150), rgb(151,152,153), rgb(154,155,156), rgb(157,158,159), rgb(160,161,162), rgb(163,164,165), rgb(166,167,168)]
          |  - [rgb(169,170,171), rgb(172,173,174), rgb(175,176,177), rgb(178,179,180), rgb(181,182,183), rgb(184,185,186), rgb(187,188,189), rgb(190,191,192)]
          |""".stripMargin
      )
      val parsed = FramePreview.load(path)
      assert(parsed.isRight)
      assertRows(parsed.toOption.get.rows)
    }
  }

/** The palette section exists so art can name its own colours. Before these tests
  * the declared palette was parsed and then ignored when resolving rows, so a
  * custom symbol was silently dropped — shifting every later pixel in the row.
  */
object FramePaletteSpecTests extends TestSuite:
  private def writeFrame(content: String): String =
    val dir = os.temp.dir()
    val path = dir / "frame.txt"
    os.write.over(path, content)
    path.toString

  private val litFrame =
    """palette:
      |  blank: "#000000"
      |  lit: "#FF0044"
      |rows:
      |  - [lit,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,blank,blank]
      |  - [blank,blank,blank,blank,blank,blank,lit,lit]
      |""".stripMargin

  val tests = Tests {
    test("custom palette symbols resolve to their declared colours") {
      val parsed = FramePreview.load(writeFrame(litFrame))
      assert(parsed.isRight)
      val frame = parsed.toOption.get
      assert(frame.rows.size == 8)
      assert(frame.rows.forall(_.size == 8))
      assert(frame.rows.head.head == Rgb(255, 0, 68))
      assert(frame.rows.head(1) == Rgb(0, 0, 0))
      assert(frame.rows(7)(6) == Rgb(255, 0, 68))
      assert(frame.rows(7)(7) == Rgb(255, 0, 68))
    }

    test("a palette symbol overrides a built-in colour name of the same name") {
      val content = litFrame.replace("lit", "green").replace("blank", "off")
      val parsed = FramePreview.load(writeFrame(content))
      assert(parsed.isRight)
      // "green" is declared as #FF0044 here, which must beat the built-in green.
      assert(parsed.toOption.get.rows.head.head == Rgb(255, 0, 68))
    }

    test("an unknown symbol is an error, not a dropped pixel") {
      val content = litFrame.replace("[lit,", "[mystery,")
      val parsed = FramePreview.load(writeFrame(content))
      assert(parsed.isLeft)
      assert(parsed.left.exists(_.contains("mystery")))
      // The message lists what is accepted so the fix is obvious.
      assert(parsed.left.exists(_.contains("lit")))
    }
  }
