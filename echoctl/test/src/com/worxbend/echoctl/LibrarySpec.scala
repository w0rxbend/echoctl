package com.worxbend.echoctl

import com.worxbend.echoctl.cli.CommandSupport
import com.worxbend.echoctl.api.Presets
import com.worxbend.echoctl.config.Config
import com.worxbend.echoctl.config.{Profile, StoredConfig}
import upickle.default.write
import utest.{TestSuite, Tests, test}

object LibrarySpec extends TestSuite:
  val tests = Tests {
    test("command support parses colors and durations") {
      assert(CommandSupport.parseColor("#00ff55").contains((0, 255, 85)))
      assert(CommandSupport.parseColor("rgb(1, 2, 3)").contains((1, 2, 3)))
      assert(CommandSupport.parseColor("blue").contains((0, 0, 255)))
      assert(CommandSupport.parseColor("invalid").isEmpty)

      assert(CommandSupport.isDuration("3s"))
      assert(CommandSupport.isDuration("80ms"))
      assert(!CommandSupport.isDuration("three-seconds"))
      assert(!CommandSupport.isDuration("3"))
    }

    test("effect table resolves by name and index") {
      assert(Presets.resolve("chase").contains(1))
      assert(Presets.resolve("CIRCLE").isEmpty)
      assert(Presets.resolve("9").contains(9))
      assert(Presets.resolve("99").isEmpty)
      assert(Presets.name(1) == "chase")
      assert(Presets.name(99).startsWith("effect_"))
    }

    test("config resolution uses flags over env over file") {
      val dir = os.temp.dir()
      val path = dir / "config.json"
      val file = StoredConfig(
        default_profile = Some("home"),
        profiles = Map(
          "home" -> Profile(
            server = Some("http://from-file.example"),
            device = Some("device-from-file"),
            token = Some("file-token")
          )
        )
      )
      os.write.over(path, write(file))

      val resolved = Config.resolve(
        serverArg = Some("http://flag.example"),
        tokenArg = Some("flag-token"),
        deviceArg = None,
        profileArg = None,
        pathArg = Some(path.toString),
        env = Map(
          "ECHOCTL_SERVER" -> "http://env.example",
          "ECHOCTL_TOKEN" -> "env-token",
          "ECHOCTL_DEVICE" -> "device-from-env"
        )
      )

      assert(resolved.server == "http://flag.example")
      assert(resolved.token.contains("flag-token"))
      assert(resolved.device.contains("device-from-env"))
      assert(resolved.profile.contains("home"))
      assert(resolved.configPath == path)
    }
  }
