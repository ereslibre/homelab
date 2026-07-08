#!/usr/bin/env python3
"""Google Workspace setup for Hermes Agent using gog CLI.

Must be run inside the hermes-gateway NixOS container — that's the only
place gog's keyring (GOG_KEYRING_BACKEND=file + GOG_KEYRING_PASSWORD) is
configured. Running on the host or anywhere else exits immediately with
instructions.

Commands:
  setup.py --check                           # Is auth configured? Exit 0=yes, 1=no
  setup.py --credentials /path/to.json       # Store OAuth client credentials
  setup.py --auth-flow --email EMAIL         # Interactive auth (URL + redirect paste)
  setup.py --auth-flow --email EMAIL \
           --services LIST                   # Limit scopes (default: all)
  setup.py --revoke [--email EMAIL]          # Revoke and remove account
  setup.py --list                            # List configured accounts

Agent workflow:
  1. --check: if exit 0, auth is good — skip setup.
  2. Ask user for client_secret.json path. Run --credentials PATH.
  3. Ask user for their Google email address.
  4. --auth-flow --email EMAIL: gog prints a URL. Send it to the user.
     The browser will redirect to http://127.0.0.1:PORT and fail — that's
     expected. They copy the FULL redirect URL from the address bar and
     paste it at the prompt that gog shows.
  5. --check to verify.
"""

import argparse
import json
import os
import subprocess
import sys
from pathlib import Path

_PENDING_PATH = Path.home() / ".hermes" / "gog_pending.json"
_DEFAULT_SERVICES = "gmail,calendar,drive,contacts,docs,sheets"
_CONTAINER_ENV_FILE = Path("/etc/hermes-gateway.env")


def _in_hermes_gateway_container():
    """True only inside the hermes-gateway NixOS container.

    /etc/hermes-gateway.env also exists on the host (it's the source of
    the bind mount), so that alone doesn't prove we're in the container —
    pair it with systemd-detect-virt, which only reports a container type
    from inside one.
    """
    if not _CONTAINER_ENV_FILE.exists():
        return False
    try:
        result = subprocess.run(
            ["systemd-detect-virt", "--container"],
            capture_output=True, text=True,
        )
    except FileNotFoundError:
        return False
    virt = (result.stdout or "").strip()
    return result.returncode == 0 and virt not in ("", "none")


def _require_container():
    if _in_hermes_gateway_container():
        return
    print(
        "ERROR: this script must be run inside the hermes-gateway NixOS "
        "container, not on the host.\n"
        "gog's keyring is only configured (GOG_KEYRING_BACKEND=file) "
        "there — on the host it hangs trying to reach a D-Bus Secret "
        "Service that doesn't exist.\n\n"
        "Enter the container first, e.g.:\n"
        "  sudo nixos-container root-login hermes-gateway\n"
        "then re-run this command from that shell."
    )
    sys.exit(1)


def _load_container_env():
    """Load gog's keyring settings from the container's env file.

    Mirrors what the hermes-gateway systemd unit gets via
    EnvironmentFile/Environment, so this script behaves the same whether
    invoked interactively or by the agent. Never overrides a var the
    caller already set explicitly.
    """
    for line in _CONTAINER_ENV_FILE.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, _, value = line.partition("=")
        os.environ.setdefault(key, value)
    os.environ.setdefault("GOG_KEYRING_BACKEND", "file")


def _gog(*args, capture=False):
    result = subprocess.run(["gog"] + list(args), capture_output=capture, text=True)
    return result


def check_auth():
    result = _gog("auth", "list", "--check", "--no-input", capture=True)
    if result.returncode == 0:
        out = (result.stdout or result.stderr or "").strip()
        print(f"AUTHENTICATED: {out or 'Account configured.'}")
        return True
    else:
        out = (result.stderr or result.stdout or "").strip()
        print(f"NOT_AUTHENTICATED: {out or 'No accounts configured. Run setup.'}")
        return False


def store_credentials(path):
    src = Path(path).expanduser().resolve()
    if not src.exists():
        print(f"ERROR: File not found: {src}")
        sys.exit(1)
    result = _gog("auth", "credentials", str(src))
    if result.returncode != 0:
        print("ERROR: Failed to store credentials.")
        sys.exit(result.returncode or 1)
    print("OK: OAuth client credentials stored.")


def auth_flow(email, services=_DEFAULT_SERVICES):
    """Run the complete interactive auth flow using --manual mode.

    gog prints the URL, waits for the user to paste the redirect URL,
    then completes the token exchange — all in one process so state is
    kept in memory across both steps.
    """
    print(f"Starting auth flow for {email}.")
    print("gog will print a URL — open it in your browser, authorize,")
    print("then paste the FULL redirect URL (http://127.0.0.1:...) when prompted.\n")
    proc = subprocess.Popen(
        ["gog", "auth", "add", email,
         "--manual",
         "--services", services,
         "--readonly",
         "--drive-scope=readonly"],
        stdin=sys.stdin,
        stdout=sys.stdout,
        stderr=sys.stderr,
    )
    proc.wait()
    if proc.returncode != 0:
        sys.exit(proc.returncode)
    _PENDING_PATH.unlink(missing_ok=True)
    print("\nOK: run --check to verify.")


def revoke(email=None):
    if not email:
        if _PENDING_PATH.exists():
            email = json.loads(_PENDING_PATH.read_text()).get("email")
        if not email:
            result = _gog("auth", "list", "--no-input", "--json", capture=True)
            if result.returncode == 0:
                try:
                    accounts = json.loads(result.stdout)
                    if isinstance(accounts, list) and accounts:
                        email = accounts[0].get("email") or accounts[0].get("account")
                except Exception:
                    pass
        if not email:
            print("ERROR: Could not determine account. Pass --email EMAIL.")
            sys.exit(1)

    result = _gog("auth", "remove", email, "--force")
    if result.returncode != 0:
        sys.exit(result.returncode or 1)
    _PENDING_PATH.unlink(missing_ok=True)
    print(f"OK: Revoked {email}.")


def list_accounts():
    result = _gog("auth", "list", "--no-input")
    sys.exit(result.returncode)


def main():
    _require_container()
    _load_container_env()

    parser = argparse.ArgumentParser(description="Google Workspace gog CLI setup for Hermes")
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--check", action="store_true", help="Check auth (exit 0=ok, 1=not configured)")
    group.add_argument("--credentials", metavar="PATH", help="Store OAuth client_secret.json")
    group.add_argument("--auth-flow", action="store_true", help="Interactive auth (prints URL, waits for redirect URL)")
    group.add_argument("--revoke", action="store_true", help="Revoke account")
    group.add_argument("--list", action="store_true", help="List configured accounts")
    parser.add_argument("--email", metavar="EMAIL", help="Google account email")
    parser.add_argument("--services", default=_DEFAULT_SERVICES,
                        help=f"Comma-separated services (default: {_DEFAULT_SERVICES})")
    args = parser.parse_args()

    if args.check:
        sys.exit(0 if check_auth() else 1)
    elif args.credentials:
        store_credentials(args.credentials)
    elif args.auth_flow:
        if not args.email:
            print("ERROR: --auth-flow requires --email EMAIL")
            sys.exit(1)
        auth_flow(args.email, args.services)
    elif args.revoke:
        revoke(args.email)
    elif args.list:
        list_accounts()


if __name__ == "__main__":
    main()
