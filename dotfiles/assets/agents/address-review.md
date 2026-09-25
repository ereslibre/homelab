---
name: address-review
description: Work through the review notes in this worktree's AGENT_REVIEW.md
---

Read `AGENT_REVIEW.md` in the current working directory. It is a local scratch file that is never committed (it is ignored through the global gitignore), so do not involve git in finding or reading it: no `git rev-parse`, `git status`, `git log` or `git diff` for it. If it is missing or empty, say there is nothing to address and stop.

Each entry is a `## <file>:<lines> (<revision>)` heading, optionally a fenced excerpt of the code or diff hunk it is about, and my note. Line numbers may be stale: locate the spot by the excerpt, not the numbers.

For each entry, in order:

1. Address it in the code. Follow the repository's own rules (CLAUDE.md/AGENTS.md) as for any other change.
2. Remove the entry from `AGENT_REVIEW.md` once it is resolved.
3. If you disagree, the note is ambiguous, or it needs a decision from me, leave the entry in place and add a reply under it starting with `> **agent:**`. Do not guess.

Never stage or commit `AGENT_REVIEW.md`. Do not commit your changes unless I ask.

Finish with a short summary: which entries you resolved, and which you left open and why.

Additional instructions, if any: $ARGUMENTS
