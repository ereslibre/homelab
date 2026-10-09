# esp32-madoka

ESPHome firmware for the ESP32 that bridges the Daikin Madoka thermostat (BLE)
to Home Assistant, and doubles as a Bluetooth proxy.

- Config: `madoka.yaml`
- Component: [`ereslibre/madoka-esphome@madoka`](https://github.com/ereslibre/madoka-esphome/tree/madoka)
- Device address: `10.0.1.60`

## Usage

```
just build madoka              # compile
just upload madoka 10.0.1.60   # compile and flash over the air
just logs madoka 10.0.1.60     # follow device logs
```

## ESPHome version

ESPHome is pinned to **2026.9.1**, run through `uvx` (`uv` itself comes from a
pinned nixpkgs revision). The component branch is rebased on ESPHome `dev`
(2026.10.0-dev at the time of writing), where the component is named
`daikin_madoka`.

| ESPHome | Result |
| --- | --- |
| 2024.3.1 | Rejects `ota: - platform: esphome`; has no `climate.climate_schema`. |
| 2025.10.5 (previous pin) | Worked with the old `madoka` component (pre-rebase branch). |
| 2026.9.1 | Works with `daikin_madoka`. |

`uvx` is used instead of the nixpkgs `esphome` package because no nixpkgs
revision was found that both has a working version and builds on macOS.
`--with pip` is required: the ESP-IDF build script shells out to `pip`.

## Troubleshooting

After switching ESPHome versions, the build can fail at the CMake configure
step (for example `Failed to resolve component 'esp_driver_bitscrambler'`).
`esphome clean` does not remove the generated sources; delete the build
directory instead:

```
rm -rf .esphome/build/esp32-madoka
```

After the component branch is force-pushed (rebased), drop the cached checkout
so it is cloned again:

```
rm -rf .esphome/external_components
```

## History

- 2026-10-09: flashed with ESPHome 2025.10.5 (logger enabled at INFO, `ota`
  moved to platform syntax), alongside Home Assistant 2026.10.0.
