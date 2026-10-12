// Case for nidavellir: a Raspberry Pi Zero 2 W sitting on the table next
// to the Ender 5 S1.
//
// Two parts, both printed flat with no supports:
//   base — tray with standoffs and port notches
//   lid  — plate with posts that clamp the board, vents and a GPIO slot
//
// Four M2.5 x 12 screws go through the lid, its posts and the board, and
// self-tap into the base standoffs.
//
// Board dimensions follow the official Zero mechanical drawing. Origin is
// the board corner next to the mini-HDMI port, X along the port edge.
//
//   openscad -o case.stl case.scad    (both parts on one plate)
//   openscad -D 'part="base"' -o base.stl case.scad
//   openscad -D 'part="lid"'  -o lid.stl  case.scad

/* [What to render] */
part = "both"; // [both, base, lid, assembly]

/* [Options] */
// "2020" adds two ears for bolting to a slot of the printer frame.
mount = "none"; // [none, 2020]
// Slot above the 40-pin header (needed for a Zero 2 WH).
gpio_slot = true;
hdmi_notch = true;
// Notch for the CSI ribbon on the short edge opposite the SD card.
camera_notch = false;

/* [Fit] */
// Gap between board edge and wall. Raise if the board is tight.
clearance = 0.5;
wall = 2.0;
floor_t = 2.0;
lid_t = 2.0;
// Room under the board (solder tails of a hand-soldered header).
standoff_h = 3.0;
// Room above the board: connectors, a low heatsink, cable overmolds.
top_h = 5.0;
// Pilot hole for M2.5 threading straight into plastic.
screw_pilot_d = 2.1;
screw_clear_d = 2.9;
// Ears only: M4 into a T-nut; use 5.4 for M5.
mount_screw_d = 4.4;

/* [Hidden] */
pcb = [65, 30, 1.5];
pcb_r = 3;
hole_inset = 3.5;
holes = [
  for (x = [hole_inset, pcb.x - hole_inset], y = [hole_inset, pcb.y - hole_inset]) [x, y]
];

// Connector centres along their board edge, with the opening each one
// needs for the cable's overmold rather than for the bare socket.
hdmi_x = 12.4;
usb_x = 41.4;
pwr_x = 54.0;
sd_y = 16.9;
cam_y = 15.0;
hdmi_w = 15;
usb_w = 10.5;
sd_w = 13.5;
cam_w = 18;

standoff_d = 6;
post_d = 5.5;
ear_len = 14;
ear_w = 16;
ear_t = 3;

pcb_z = floor_t + standoff_h;
pcb_top = pcb_z + pcb.z;
base_h = pcb_top + top_h;
outer_r = pcb_r + clearance + wall;
eps = 0.01;
$fn = 48;

module rounded_rect(size, r) {
  hull()
    for (x = [r, size.x - r], y = [r, size.y - r])
      translate([x, y]) circle(r);
}

// Outline of the board grown by `d`.
module outline(d) {
  translate([-d, -d])
    rounded_rect([pcb.x + 2 * d, pcb.y + 2 * d], pcb_r + d);
}

// Open-topped notch through the wall of the port (y = 0) edge.
module port_notch(x, w, z0) {
  translate([x - w / 2, -clearance - wall - eps, z0])
    cube([w, wall + clearance + 2 * eps, base_h]);
}

// Same, through a short edge; `far` selects the x = pcb.x side.
module end_notch(y, w, z0, far = false) {
  translate([far ? pcb.x - eps : -clearance - wall - eps, y - w / 2, z0])
    cube([wall + clearance + 2 * eps, w, base_h]);
}

module ears() {
  for (side = [0, 1])
    translate([side ? pcb.x + clearance + wall : -clearance - wall, pcb.y / 2])
      mirror([side ? 0 : 1, 0])
        difference() {
          linear_extrude(ear_t)
            hull() {
              translate([-outer_r, -ear_w / 2]) square([outer_r, ear_w]);
              translate([ear_len - ear_w / 2, 0]) circle(d = ear_w);
            }
          // Slotted so the two T-nuts do not have to be spaced exactly.
          translate([ear_len - ear_w / 2, 0, -eps])
            linear_extrude(ear_t + 2 * eps)
              hull()
                for (dx = [-1.5, 1.5])
                  translate([dx, 0]) circle(d = mount_screw_d);
        }
}

module base() {
  difference() {
    union() {
      linear_extrude(base_h) outline(clearance + wall);
      if (mount == "2020") ears();
    }
    // Cavity.
    translate([0, 0, floor_t])
      linear_extrude(base_h) outline(clearance);
    // Plugs sit centred roughly 1.3 mm above the board, so their
    // overmolds reach below it as well.
    if (hdmi_notch) port_notch(hdmi_x, hdmi_w, pcb_top - 2.5);
    port_notch(usb_x, usb_w, pcb_top - 2.5);
    port_notch(pwr_x, usb_w, pcb_top - 2.5);
    end_notch(sd_y, sd_w, pcb_top - 0.5);
    if (camera_notch) end_notch(cam_y, cam_w, pcb_top - 0.5, far = true);
  }
  for (h = holes)
    translate([h.x, h.y, floor_t - eps])
      difference() {
        cylinder(d = standoff_d, h = standoff_h + eps);
        // Runs on into the floor so a 12 mm screw cannot bottom out.
        translate([0, 0, -1.2]) cylinder(d = screw_pilot_d, h = standoff_h + 2);
      }
}

// Modelled where it sits on the base, in board coordinates.
module lid_in_place() {
  difference() {
    union() {
      translate([0, 0, base_h]) linear_extrude(lid_t) outline(clearance + wall);
      for (h = holes)
        translate([h.x, h.y, pcb_top + 0.2])
          cylinder(d = post_d, h = top_h - 0.2 + eps);
    }
    for (h = holes)
      translate([h.x, h.y, pcb_top])
        cylinder(d = screw_clear_d, h = top_h + lid_t + 1);
    // Vents over the SoC and the Wi-Fi can.
    for (i = [-3 : 3])
      translate([28 + i * 4.5 - 0.9, 5, base_h - eps])
        cube([1.8, 13, lid_t + 2 * eps]);
    if (gpio_slot)
      translate([pcb.x / 2 - 51.3 / 2, pcb.y - hole_inset - 2.8, base_h - eps])
        cube([51.3, 5.6, lid_t + 2 * eps]);
  }
}

// Turned over for printing: outer face on the bed, posts pointing up.
module lid() {
  translate([0, pcb.y, base_h + lid_t]) rotate([180, 0, 0]) lid_in_place();
}

if (part == "base") {
  base();
} else if (part == "lid") {
  lid();
} else if (part == "assembly") {
  base();
  %translate([0, 0, pcb_z]) linear_extrude(pcb.z) outline(0);
  lid_in_place();
} else {
  base();
  translate([0, pcb.y + 2 * (clearance + wall) + 8, 0]) lid();
}
