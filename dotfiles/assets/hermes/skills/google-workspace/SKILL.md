---
name: google-workspace
description: "Gmail, Calendar, Drive, Docs, Sheets, and Contacts via gog CLI."
version: 2.0.0
author: openclaw
license: MIT
platforms: [linux, macos, windows]
metadata:
  hermes:
    tags: [Google, Gmail, Calendar, Drive, Sheets, Docs, Contacts, Email, OAuth]
    homepage: https://gogcli.sh
    related_skills: [himalaya]
---

# Google Workspace

Gmail, Calendar, Drive, Contacts, Sheets, and Docs via the `gog` CLI. After
setup you call `gog` commands directly — no wrapper scripts.

## References

- `references/gmail-search-syntax.md` — Gmail search operators

## My Calendars

Calendar IDs are stored as sops secrets and exposed as environment variables (see [Required Secrets](#required-secrets)).

| Label             | Env var / Calendar ID                          | Notes                                                                                               |
|-------------------|------------------------------------------------|-----------------------------------------------------------------------------------------------------|
| Personal          | `$CAL_PERSONAL` (alias: `primary`)             | Primary personal calendar                                                                           |
| Serebris          | `$CAL_SEREBRIS`                                | Shared calendar: `Rafa:` prefix = me only, `Raquel:` prefix = partner only, unprefixed = both of us |
| Legal             | `$CAL_LEGAL`                                   | Legal matters                                                                                       |
| Birthdays         | `$CAL_BIRTHDAYS`                               | Birthday reminders                                                                                  |
| Holidays in Spain | `es.spain#holiday@group.v.calendar.google.com` | Spanish public holidays (public ID, not a secret)                                                   |
| Work              | `$CAL_WORK_ICS_URL` (ICS feed)                 | Work calendar — read-only, fetched via ICS (see [ICS Calendars](#ics-calendars))                    |

When aggregating the schedule, query **all five Google Calendar API calendars** and merge results by time, then also fetch the Work ICS calendar (see [ICS Calendars](#ics-calendars)) and merge those events in. For Serebris, include events prefixed `Rafa:` and all unprefixed events (those prefixed `Raquel:` apply to my partner only, not to me). The authorized account for all Google Calendar API calls is `ereslibre@gmail.com`.

## Required Secrets

These sops secrets must exist and be exported as environment variables before using calendar features:

| Env var            | Sops secret path                  | Value                          |
|--------------------|-----------------------------------|--------------------------------|
| `CAL_PERSONAL`     | `hermes/calendars/personal`       | Personal Google Calendar ID    |
| `CAL_SEREBRIS`     | `hermes/calendars/serebris`       | Serebris shared calendar ID    |
| `CAL_LEGAL`        | `hermes/calendars/legal`          | Legal calendar ID              |
| `CAL_BIRTHDAYS`    | `hermes/calendars/birthdays`      | Birthdays calendar ID          |
| `CAL_WORK_ICS_URL` | `hermes/calendars/work`           | Work calendar public ICS URL   |

## Scripts

- `scripts/setup.py` — one-time OAuth setup

## First-Time Setup

Define a shorthand:

```bash
GSETUP="python ${HERMES_HOME:-$HOME/.hermes}/skills/productivity/google-workspace/scripts/setup.py"
```

### Step 0: Check if already set up

```bash
$GSETUP --check
```

If it prints `AUTHENTICATED`, skip to Usage.

### Step 1: Triage

Ask the user:

**"What Google services do you need?"**

- Email only → use the `himalaya` skill instead (simpler App Password setup).
- Everything else → continue here.

### Step 2: Create OAuth credentials (one-time, ~5 minutes)

Tell the user:

> 1. Create or select a Google Cloud project:
>    https://console.cloud.google.com/projectselector2/home/dashboard
> 2. Enable APIs: Gmail, Google Calendar, Google Drive, Google Sheets, Google Docs, People API
>    https://console.cloud.google.com/apis/library
> 3. Create credentials → OAuth 2.0 Client ID → Application type: Desktop app
>    https://console.cloud.google.com/apis/credentials
> 4. If the app is in Testing, add your Google account as a test user:
>    https://console.cloud.google.com/auth/audience
> 5. Download the JSON file and share the file path with me.
>
> Note: if the path starts with `/`, send it in a sentence to avoid slash-command
> interpretation: "The path is: /home/user/Downloads/client_secret.json"

Once they provide the path:

```bash
$GSETUP --credentials /path/to/client_secret.json
```

### Step 3: Authorize

Ask for the user's Google account email address, then run the interactive flow:

```bash
$GSETUP --auth-flow --email user@gmail.com
```

`gog` will print a URL. Send it to the user. Tell them:
- Open the URL in a browser and authorize.
- The browser will redirect to `http://127.0.0.1:PORT/...` and show an error — that's expected.
- Copy the **full redirect URL** from the browser address bar and paste it at the prompt.

If the user gets `403: access_denied`, send them to add themselves as a test user:
https://console.cloud.google.com/auth/audience

### Step 4: Verify

```bash
$GSETUP --check
```

Should print `AUTHENTICATED`. Note the email for use in commands below.

### Revoking

```bash
$GSETUP --revoke --email user@gmail.com
```

## Usage

Call `gog` directly. Always pass `--account EMAIL` (replace with the authorized
account) and `--no-input` for automation. Use `--json` to get structured output.

Set a shorthand to avoid repeating the account:

```bash
GOG="gog --account user@gmail.com --no-input"
```

Or export `GOG_ACCOUNT=user@gmail.com` to omit `--account` from every command.

### Gmail

```bash
# Search (one result per thread)
gog gmail search 'is:unread' --max 10 --json --account EMAIL --no-input
gog gmail search 'newer_than:7d from:boss' --max 20 --json --account EMAIL --no-input

# Search (one result per message, ignores threading)
gog gmail messages search 'in:inbox' --max 20 --json --account EMAIL --no-input

# Read full message
gog gmail get MESSAGE_ID --json --account EMAIL --no-input

# Send (plain text, multi-line via heredoc)
gog gmail send --to a@b.com --subject "Hi" --body-file - --account EMAIL <<'EOF'
Hi,

Message body here.

Regards
EOF

# Send (single line)
gog gmail send --to a@b.com --subject "Hi" --body "Hello" --account EMAIL

# Send (HTML)
gog gmail send --to a@b.com --subject "Hi" --body-html "<p>Hello</p>" --account EMAIL

# Reply (thread by message ID)
gog gmail send --to a@b.com --subject "Re: Hi" --body "Thanks" \
  --reply-to-message-id MESSAGE_ID --account EMAIL

# Draft
gog gmail drafts create --to a@b.com --subject "Draft" --body "..." --json --account EMAIL
gog gmail drafts send DRAFT_ID --account EMAIL

# Labels
gog gmail labels list --json --account EMAIL --no-input
```

### Calendar

```bash
# List all available calendars (use to verify calendar IDs are accessible)
gog calendar calendars --json --account ereslibre@gmail.com --no-input

# Query all my calendars and merge — run for each calendar ID, then sort by time:
for CAL in \
  "$CAL_PERSONAL" \
  "$CAL_SEREBRIS" \
  "$CAL_LEGAL" \
  "$CAL_BIRTHDAYS" \
  "es.spain#holiday@group.v.calendar.google.com"
do
  gog calendar events "$CAL" --from DATE --to DATE --json --account ereslibre@gmail.com --no-input
done
# Also fetch Work ICS calendar separately — see ICS Calendars section below

# List events from a single calendar
gog calendar events "$CAL_PERSONAL" --from 2026-06-01 --to 2026-06-30 --json --account ereslibre@gmail.com --no-input
gog calendar events "$CAL_SEREBRIS" \
  --from 2026-06-01 --to 2026-06-30 --json --account ereslibre@gmail.com --no-input
gog calendar events "$CAL_PERSONAL" --from 2026-06-24T09:00:00+02:00 --to 2026-06-24T18:00:00+02:00 \
  --json --account ereslibre@gmail.com --no-input

# Create event (ISO 8601 with timezone required)
gog calendar create primary --summary "Team Standup" \
  --from 2026-06-25T10:00:00+02:00 --to 2026-06-25T10:30:00+02:00 \
  --json --account EMAIL

# Create with attendees and location
gog calendar create primary --summary "Review" \
  --from 2026-06-25T14:00:00+02:00 --to 2026-06-25T15:00:00+02:00 \
  --location "Room 1" --attendee alice@co.com --attendee bob@co.com \
  --json --account EMAIL

# Update event
gog calendar update primary EVENT_ID --summary "New Title" --account EMAIL

# Show available event colors
gog calendar colors --account EMAIL

# Delete event
gog calendar delete primary EVENT_ID --force --account EMAIL

# Freebusy check
gog calendar freebusy alice@co.com,bob@co.com \
  --from 2026-06-25T00:00:00Z --to 2026-06-26T00:00:00Z \
  --json --account EMAIL --no-input
```

### ICS Calendars

The Work calendar is read-only and fetched via its ICS feed stored in `$CAL_WORK_ICS_URL`.

```bash
# Fetch raw ICS data
curl -s "$CAL_WORK_ICS_URL"

# Fetch and parse with Python (icalendar package) — filter by date range
python3 - <<'EOF'
import sys, os
from urllib.request import urlopen
from icalendar import Calendar
from datetime import datetime, timezone

url = os.environ["CAL_WORK_ICS_URL"]
with urlopen(url) as resp:
    cal = Calendar.from_ical(resp.read())

date_from = datetime(2026, 6, 1, tzinfo=timezone.utc)
date_to   = datetime(2026, 6, 30, 23, 59, 59, tzinfo=timezone.utc)

for component in cal.walk("VEVENT"):
    dtstart = component.get("DTSTART").dt
    if not hasattr(dtstart, "hour"):
        from datetime import time
        dtstart = datetime.combine(dtstart, time.min, tzinfo=timezone.utc)
    if date_from <= dtstart <= date_to:
        print(dtstart.isoformat(), component.get("SUMMARY"))
EOF
```

### Drive

```bash
# Search
gog drive search "quarterly report" --max 10 --json --account EMAIL --no-input

# Get file metadata
gog drive get FILE_ID --json --account EMAIL --no-input

# Upload
gog drive upload /path/to/file.pdf --json --account EMAIL
gog drive upload /path/to/image.png --parent FOLDER_ID --name "Logo.png" --json --account EMAIL

# Download (Google-native files export automatically)
gog drive download FILE_ID --out /path/to/output.pdf --account EMAIL
gog drive download DOC_ID --format txt --out /tmp/doc.txt --account EMAIL

# Create folder
gog drive mkdir "Reports" --json --account EMAIL
gog drive mkdir "Q4" --parent FOLDER_ID --json --account EMAIL

# Share
gog drive share FILE_ID --to user --email alice@example.com --role reader --json --account EMAIL
gog drive share FILE_ID --to anyone --role reader --account EMAIL

# Delete (use delete for permanent, or move to trash by default)
gog drive delete FILE_ID --force --account EMAIL
```

### Contacts

```bash
gog contacts list --max 50 --json --account EMAIL --no-input
gog contacts search "John" --json --account EMAIL --no-input
```

### Sheets

```bash
# Read
gog sheets get SHEET_ID "Sheet1!A1:D10" --json --account EMAIL --no-input

# Write
gog sheets update SHEET_ID "Sheet1!A1:B2" \
  --values-json '[["Name","Score"],["Alice","95"]]' \
  --input USER_ENTERED --account EMAIL

# Append
gog sheets append SHEET_ID "Sheet1!A:C" \
  --values-json '[["new","row","data"]]' \
  --insert INSERT_ROWS --account EMAIL

# Clear
gog sheets clear SHEET_ID "Sheet1!A2:Z" --account EMAIL

# Create
gog sheets create "Q4 Budget" --json --account EMAIL
```

### Docs

```bash
# Read as plain text
gog docs cat DOC_ID --account EMAIL --no-input

# Export
gog docs export DOC_ID --format txt --out /tmp/doc.txt --account EMAIL
gog docs export DOC_ID --format pdf --out /tmp/doc.pdf --account EMAIL

# Create
gog docs create "Meeting Notes" --json --account EMAIL

# Append text
gog docs write DOC_ID --append --text "New paragraph." --account EMAIL

# Append markdown
gog docs write DOC_ID --append --markdown --text "## Status\n\n- Item one" --account EMAIL
```

## Email Formatting

- Prefer plain text. Use `--body-file -` with a heredoc for multi-paragraph messages.
- `--body` does not unescape `\n`. Use a heredoc or `$'Line 1\n\nLine 2'` for inline newlines.
- Use `--body-html` only when rich formatting is needed.

## Rules

1. **Never send email, create/delete calendar events, delete Drive files, or share files
   without confirming with the user first.** Show what will be done and get approval.
   Prefer `drive delete` (permanent) only after explicit confirmation; otherwise omit
   `--force` on calendar delete and let the user confirm.
2. **Check auth before first use** — run `$GSETUP --check`. If it fails, guide setup.
3. **Use the Gmail search syntax reference** for complex queries:
   `skill_view("google-workspace", file_path="references/gmail-search-syntax.md")`
4. **Calendar times must include timezone** — ISO 8601 with offset or UTC `Z`.
5. **Avoid rapid sequential API calls** — batch reads when possible.

## Troubleshooting

| Problem | Fix |
|---------|-----|
| `NOT_AUTHENTICATED` | Run setup Steps 2–5 above |
| `gog: command not found` | `gog` not in PATH; check system packages |
| `403: access_denied` | Add Google account as test user in Cloud Console |
| Auth code expired | Re-run `--auth-url`, use the newest redirect URL only |
| `permission denied` or scope error | `$GSETUP --revoke --email EMAIL` then redo Steps 3–5 with full services |
| `GOG_KEYRING_PASSWORD not set` | Add `hermes/gogcli_keyring_password` to sops secrets |
| `CAL_*` env vars empty/unset | Add the corresponding `hermes/cal_*` entries to sops secrets and export them |
| Rotated OAuth client secret | Download new `client_secret.json` → `$GSETUP --revoke --email ereslibre@gmail.com` → `$GSETUP --credentials /path/to/new.json` → `$GSETUP --auth-flow --email ereslibre@gmail.com` |
| Calendar not found / permission error | Run `gog calendar calendars --json --account ereslibre@gmail.com --no-input` to list accessible calendars |
