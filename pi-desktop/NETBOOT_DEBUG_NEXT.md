# pi-desktop netboot: next on-site debugging session

## Try this first — likely root cause (2026-04-27)

Per the [official Pi bootloader docs](https://www.raspberrypi.com/documentation/computers/raspberry-pi.html#BOOT_ORDER),
`BOOT_ORDER` nibbles are `0x2`=NETWORK and `0x4`=USB-MSD (we had these
swapped in the README). The currently-flashed value `0xf24` is read
LSB-first as **USB-MSD → NETWORK → RESTART**. With the installer USB
plugged in, the bootloader is *correctly* booting USB first and never
attempting network — the "silent on the wire" symptom is consistent with
expected behaviour, not a firmware bug.

Reflash the EEPROM with:

- `BOOT_ORDER=0xf42` — NETWORK → USB-MSD → RESTART
- `TFTP_PREFIX=1` — literal-string prefix (the docs say `2` means MAC
  address, so the current flash with `TFTP_PREFIX=2 +
  TFTP_PREFIX_STR=pi-desktop` ignores the string entirely)
- keep `TFTP_PREFIX_STR=pi-desktop`, `TFTP_IP=10.0.4.2`,
  `BOOT_UART=1`, `NET_INSTALL_AT_POWER_ON=0`

Cold-cycle and re-run the `tcpdump`. If DHCP DISCOVERs now appear from
the Pi MAC, the rest of the steps below (UART, firmware downgrade,
u-boot fallback) are unnecessary.

## Current conclusion (pre-2026-04-27 finding — kept for context)

The failure is happening **before TFTP and before NixOS**.

What is already established:

- The Pi 4 EEPROM config is flashed and persists across a full cold power cycle.
- The staged TFTP tree on Synology looks complete and internally consistent.
- The iSCSI target is reachable and was already used to install the system.
- During the Pi bootloader netboot window, there are **zero packets** from the
  Pi MAC on the wire.
- The only DHCP packet seen is later from the USB installer userland after the
  Pi falls through to USB boot.

That means the failing stage is one of:

1. EEPROM bootloader decides not to attempt network boot.
2. EEPROM bootloader attempts network boot but never brings Ethernet up.
3. EEPROM bootloader brings Ethernet up but fails before sending DHCP.

It is **not** currently pointing to:

- bad NixOS initrd logic
- bad iSCSI config
- missing TFTP files
- kernel boot failure

## Most useful next inspection

Use **UART bootloader logging**.

Reason:

- `tcpdump` already showed that the bootloader is silent on the network.
- UART is the most direct way to see what the EEPROM bootloader thinks it is
  doing.
- Without UART, the remaining hypotheses are mostly guesswork.

## UART setup

Reflash EEPROM with `BOOT_UART=1` if it is not already set.

Hardware:

- USB-to-TTL adapter with **3.3V logic**
- Pi GPIO 8 = TX (GPIO14)
- Pi GPIO 10 = RX (GPIO15)
- Pi GPIO 6 = GND

Do **not** use a 5V serial adapter.

Serial settings:

- `115200 8N1`

What to capture:

- full output from cold power-on until the machine either netboots or falls back
  to the USB installer

What we want to learn from UART:

- whether the EEPROM bootloader is entering the network boot path at all
- whether the PHY/link comes up
- whether DHCP is attempted
- whether there is a bootloader error code or early abort

## If no UART adapter is available

Run another packet capture during a **full cold boot**:

```bash
sudo tcpdump -i enp36s0f1 'ether host e4:5f:01:01:3b:24 or (port 67 or port 68 or port 69)'
```

Expected interpretations:

- no packets at all:
  bootloader still failing before or during NIC bring-up
- DHCP but no TFTP:
  bootloader network stack is alive; investigate DHCP reply / option handling
- TFTP requests:
  EEPROM netboot path is alive; move focus to file fetches and later stages

## Best next change to try

Try an **older EEPROM bootloader**.

Why:

- current bootloader version is `2026/01/09`
- a firmware regression on this Pi 400 or its PHY is plausible
- this is one of the few high-value variables left in the pre-TFTP stage

When testing an older EEPROM:

- set `BOOT_UART=1`
- do a **full cold power cycle** after flashing
- remove stale `/firmware/pieeprom.upd`, `/firmware/pieeprom.sig`,
  `/firmware/recovery.bin` if they remain

## Optional longer-shot test

Try EEPROM `NETCONSOLE`:

- set `NETCONSOLE=10.0.4.20:6666`
- listen on hulk with:

```bash
nc -ul 6666
```

This only helps if the bootloader network path is at least partially working, so
it is less reliable than UART.

## Practical fallback if EEPROM netboot remains opaque

Use **U-Boot on the USB SSD** as the netboot stage:

- let EEPROM continue falling through to USB
- let U-Boot do DHCP + TFTP for `kernel8.img`, `initrd`, and DTB
- keep iSCSI root exactly as-is

This is less elegant than pure EEPROM netboot, but it bypasses the current blind
spot entirely and should be much easier to debug.

## Short decision tree

1. If a UART adapter is available, do UART first.
2. If UART is unavailable, downgrade EEPROM and repeat cold-boot `tcpdump`.
3. If EEPROM still stays silent, stop spending time on TFTP/NixOS details.
4. Switch to U-Boot-driven TFTP boot as the pragmatic path forward.
