# echoctl

A Scala 3 CLI client for [`echo`](https://github.com/w0rxbend/echo), the LED matrix HTTP proxy.
It intentionally talks only to the HTTP API (`/api/v1/...`) and never uses the raw TCP firmware protocol.

```bash
# Basic health checks
echoctl health
echoctl ready
echoctl devices

# Play animations
echoctl animations list
echoctl animations catalog
echoctl play alert --priority 80
echoctl preset matrix_rain_background
echoctl effect chase --interval 80ms --color '#00FF55'
echoctl stop

# Direct matrix controls
echoctl matrix fill '#00FF55'
echoctl matrix clear
echoctl matrix brightness 20

# Discovery
echoctl openapi
echoctl metrics

# Notify and generic event
 echoctl notify "deploy done" --duration 3s
echoctl event --source cli --type deploy --attr animation=notify --attr duration=2s

# Background and queue
echoctl background get
echoctl background set matrix_rain_background
echoctl queue
echoctl queue --clear
```

## Features implemented

- Endpoints for `health`, `ready`, `devices`, `animations`, `play`, `preset`, `effect`, `stop`, `notify`, `event`, `matrix`, `background`, and `queue`.
- Additional helper endpoints: `/openapi.json` and `/metrics`.
- Config resolution precedence: flags > env > config file > defaults.
- Built-in firmware effect id/name table for `effect`.
- Duration/interval and color input validation.
- `--json` and `--verbose` output modes.
- `com.lihaoyi`-backed CLI ergonomics (`picocli`, `fansi`, `pprint`, `os-lib`, `requests`, `upickle`).
- Mill build with `NativeImageModule` support.
- Unit tests cover color/duration parsing, preset resolution, and config precedence.

## Configuration

Config path: `~/.config/echoctl/config.json`.

```json
{
  "default_profile": "home",
  "profiles": {
    "home": {
      "server": "http://pi4.lan:8080",
      "device": "living-room",
      "token": ""
    }
  }
}
```

Environment variables:

- `ECHOCTL_SERVER`
- `ECHOCTL_TOKEN`
- `ECHOCTL_DEVICE`
- `MATRIX_PROXY_ADMIN_TOKEN` (token fallback)

Global flags:

- `--server`
- `--token`
- `--device`
- `--profile`
- `--config`
- `--json`
- `--verbose`
- `--help`
