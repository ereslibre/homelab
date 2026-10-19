//! `claude-sessions hook`: run by Claude Code on every hook event we
//! subscribe to, with the event as JSON on stdin. Keeps the session's state
//! file current. Sessions outside tmux are not tracked: there would be
//! nothing to read or type into.

use std::{io::Read, path::Path};

use anyhow::{Context, Result};
use serde::Deserialize;

use crate::{
    state::{self, Session, State},
    tmux,
};

const PROMPT_CHARS: usize = 200;

/// The fields we use; Claude Code sends more.
#[derive(Deserialize)]
struct Input {
    session_id: String,
    #[serde(default)]
    cwd: String,
    hook_event_name: String,
    #[serde(default)]
    prompt: Option<String>,
    #[serde(default)]
    message: Option<String>,
    #[serde(default)]
    notification_type: Option<String>,
}

pub async fn run(dir: &Path) -> Result<()> {
    let mut raw = String::new();
    std::io::stdin().read_to_string(&mut raw)?;
    let input: Input = serde_json::from_str(&raw).context("parsing hook input")?;

    let Some(pane) = std::env::var("TMUX_PANE").ok().filter(|p| !p.is_empty()) else {
        return Ok(());
    };

    if input.hook_event_name == "SessionEnd" {
        return state::remove(dir, &input.session_id);
    }

    if input.hook_event_name == "SessionStart" {
        // One Claude Code per pane: whatever was there before is gone, even
        // if it never got to run its SessionEnd hook.
        for old in state::list(dir) {
            if old.tmux_pane == pane && old.session_id != input.session_id {
                state::remove(dir, &old.session_id)?;
            }
        }
    }

    let previous = state::read(dir, &input.session_id);
    let tmux_session = match tmux::session_of(&pane).await {
        Ok(name) => name,
        Err(e) => previous.as_ref().map(|s| s.tmux_session.clone()).ok_or(e)?,
    };
    let previous_state = previous.as_ref().map_or(State::Idle, |s| s.state);
    let (mut last_prompt, mut message) = previous
        .map(|s| (s.last_prompt, s.message))
        .unwrap_or_default();

    let state = match input.hook_event_name.as_str() {
        "SessionStart" | "Stop" => State::Idle,
        "UserPromptSubmit" => {
            last_prompt = input.prompt.map(|p| p.chars().take(PROMPT_CHARS).collect());
            State::Working
        }
        // A tool about to run or just run means nothing is blocking anymore,
        // e.g. a permission prompt that was answered.
        "PreToolUse" | "PostToolUse" => State::Working,
        "Notification" => match input.notification_type.as_deref() {
            Some("permission_prompt" | "elicitation_dialog") => {
                message = input.message;
                State::Waiting
            }
            Some("idle_prompt") => State::Idle,
            _ => previous_state,
        },
        _ => previous_state,
    };
    if state != State::Waiting {
        message = None;
    }

    state::write(
        dir,
        &Session {
            session_id: input.session_id,
            cwd: input.cwd,
            tmux_session,
            tmux_pane: pane,
            state,
            last_prompt,
            message,
            updated_at: state::now(),
        },
    )
}
