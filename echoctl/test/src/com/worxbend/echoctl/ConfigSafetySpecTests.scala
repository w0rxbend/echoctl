package com.worxbend.echoctl

import com.worxbend.echoctl.config.Config
import com.worxbend.echoctl.config.{Profile, StoredConfig}
import upickle.default.write
import utest.{TestSuite, Tests, test}

import scala.util.Try

/** Covers the two ways the config file could lose or leak a token.
  *
  * The file holds the bearer token that unlocks every admin route on the
  * server, so it must not be readable by other users on the machine; and
  * because every write path reads the file before writing it back, a parse
  * failure must never be silently downgraded to an empty config.
  */
object ConfigSafetySpecTests extends TestSuite:
  val tests = Tests {

    test("a malformed config file is an error, not an empty config") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      os.write(path, """{"profiles": {"home": {"server": "http://x"},}}""")

      val thrown = Try(Config.listProfiles(Some(path.toString))).failed.get
      assert(thrown.isInstanceOf[Config.ConfigParseError])
      // The message has to name the file, or the user cannot act on it.
      assert(thrown.getMessage.contains("config.json"))
    }

    test("a malformed config file is never overwritten") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      val corrupt = """{"profiles": {"home": {"server": "http://x"},}}"""
      os.write(path, corrupt)

      // Before this was an error, initProfile read nothing, added one profile,
      // and wrote the result back -- destroying every other profile in the file.
      assert(Try(Config.initProfile(Some(path.toString), "work", None, None, None)).isFailure)
      assert(os.read(path) == corrupt)
    }

    test("an absent config file is still an empty config, not an error") {
      val dir = os.temp.dir()
      assert(Config.listProfiles(Some((dir / "nothing-here.json").toString)).isEmpty)
    }

    test("an empty config file is still an empty config, not an error") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      os.write(path, "   \n")
      assert(Config.listProfiles(Some(path.toString)).isEmpty)
    }

    test("a written config file is not readable by group or other") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      Config.write(
        Some(path.toString),
        StoredConfig(
          default_profile = Some("home"),
          profiles = Map("home" -> Profile(server = Some("http://x"), token = Some("s3cret")))
        )
      )

      assert(os.read(path).contains("s3cret"))
      assert(os.perms(path).toString == "rw-------")
    }

    test("an existing loose-permission config file is tightened on write") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      os.write(path, write(StoredConfig()))
      os.perms.set(path, "rw-rw-r--")

      Config.write(Some(path.toString), StoredConfig(profiles = Map("home" -> Profile())))

      assert(os.perms(path).toString == "rw-------")
    }
  }
