# nidavellir case

A two-part printable case for the Raspberry Pi Zero 2 W. It sits on the
table next to the printer; optional ears bolt it onto a 2020 slot of the
Ender 5 S1 frame instead.

[`case.scad`](case.scad) is [OpenSCAD](https://openscad.org) source: the
model is described as code, and a slicer cannot open it. It has to be
rendered into STL files first, which is what the `just` targets here do.
OpenSCAD itself comes from nixpkgs on demand; nothing needs installing.

## Targets

Run these from this directory:

| Target | Produces | |
|---|---|---|
| `just` / `just plate` | `case.stl` | both parts side by side, to print in one go |
| `just parts` | `base.stl`, `lid.stl` | each part in its own file |
| `just base` | `base.stl` | the tray: standoffs and port notches |
| `just lid` | `lid.stl` | the cover, already turned over for printing |
| `just preview` | `assembly.png` | a picture of the assembled case |
| `just gui` | | opens the model in the OpenSCAD GUI |
| `just clean` | | removes everything rendered |

The rendered files are not kept in git.

## Printing

1. `just`
2. Open Creality Print (or any slicer), pick the Ender 5 S1 and import
   `case.stl`. It holds both parts, already laid out next to each other.
3. Keep the orientation as imported: both parts lie flat, the lid with
   its outer face on the bed and its posts pointing up.
4. Slice with 0.2 mm layers and no supports. PETG if the case sits near
   the bed or PSU, otherwise PLA is fine.
5. Print from the slicer, or export the G-code and upload it to OctoPrint
   at `http://nidavellir.local/`.

## Hardware

- 4 × M2.5 × 12 screws — through the lid and the board, self-tapping
  into the base standoffs;
- only with `mount = "2020"`: 2 × M4 × 8 screws + M4 T-nuts for the
  frame.

## Adjusting the model

The parameters are at the top of `case.scad`; change them there and
render again, or try them out live in the Customizer panel of `just gui`.

| Parameter | Default | |
|---|---|---|
| `mount` | `"none"` | `"2020"` adds ears for bolting to the printer frame |
| `mount_screw_d` | `4.4` | ears only: M4 into a T-nut; `5.4` for M5 |
| `clearance` | `0.5` | gap between board edge and wall; raise if the board is tight |
| `gpio_slot` | `true` | slot over the 40-pin header |
| `hdmi_notch` | `true` | cut-out for the mini-HDMI port |
| `camera_notch` | `false` | notch for the CSI ribbon |
| `standoff_h` | `3.0` | room under the board |
| `top_h` | `5.0` | room above the board: connectors, a low heatsink |

Cut-outs that are always there: both micro-USB sockets, the SD card
(swappable without opening the case) and vents over the SoC.
