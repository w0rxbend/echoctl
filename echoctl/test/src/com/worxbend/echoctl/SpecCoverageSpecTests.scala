package com.worxbend.echoctl

import utest.{TestSuite, Tests, test}

import java.nio.file.{Files, Path, Paths}

/** Checks the CLI against the vendored `spec/openapi.json`.
  *
  * `spec/openapi.json` is a copy of the server's generated spec. Nothing forced the
  * two to agree before, so the CLI could call a route the server does not serve and
  * only find out at runtime with a 404. These tests make the vendored spec a checked
  * contract: every route the client calls must appear in it, at the method used.
  */
object SpecCoverageSpecTests extends TestSuite:

  private def specFile: Path =
    val relative = Paths.get("spec", "openapi.json")
    Iterator
      .iterate(Paths.get("").toAbsolutePath)(_.getParent)
      .takeWhile(_ != null)
      .map(_.resolve(relative))
      .find(Files.exists(_))
      .getOrElse(throw new AssertionError(s"could not locate $relative"))

  private lazy val spec: ujson.Value = ujson.read(Files.readString(specFile))

  private lazy val specPaths: Map[String, Set[String]] =
    spec("paths").obj.map { case (path, operations) =>
      path -> operations.obj.keys.map(_.toLowerCase).toSet
    }.toMap

  /** Every route EchoClient reaches, with concrete path parameters replaced by the
    * spec's template form.
    */
  private val clientRoutes: Seq[(String, String)] = Seq(
    "get" -> "/api/v1/devices",
    "get" -> "/api/v1/animations",
    "get" -> "/api/v1/animations/catalog",
    "post" -> "/api/v1/devices/{device}/notify",
    "post" -> "/api/v1/devices/{device}/events",
    "post" -> "/api/v1/devices/{device}/play",
    "post" -> "/api/v1/devices/{device}/preset/{animation}",
    "post" -> "/api/v1/devices/{device}/matrix/preset",
    "post" -> "/api/v1/devices/{device}/matrix/fill",
    "post" -> "/api/v1/devices/{device}/matrix/clear",
    "post" -> "/api/v1/devices/{device}/matrix/brightness",
    "post" -> "/api/v1/devices/{device}/matrix/pixel",
    "post" -> "/api/v1/devices/{device}/matrix/panel",
    "post" -> "/api/v1/devices/{device}/matrix/static",
    "post" -> "/api/v1/devices/{device}/matrix/animation",
    "get" -> "/api/v1/devices/{device}/background",
    "put" -> "/api/v1/devices/{device}/background",
    "get" -> "/api/v1/devices/{device}/queue",
    "delete" -> "/api/v1/devices/{device}/queue"
  )

  val tests: Tests = Tests {
    test("the vendored spec parses and declares paths") {
      assert(specPaths.nonEmpty)
    }

    test("every route the client calls exists in the spec at that method") {
      val missing = clientRoutes.filterNot { case (method, path) =>
        specPaths.get(path).exists(_.contains(method))
      }
      assert(missing.isEmpty)
    }

    test("the CLI covers every device matrix route the server exposes") {
      val serverMatrixRoutes = specPaths.keySet.filter(_.contains("/matrix/"))
      val covered = clientRoutes.map(_._2).toSet
      val uncovered = (serverMatrixRoutes -- covered).toSeq.sorted
      assert(uncovered.isEmpty)
    }
  }
