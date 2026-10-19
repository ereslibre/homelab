//! One JSON file per Claude Code session, written by `claude-sessions hook`
//! and read by the MCP server. The file name is the session id.

use std::{
    fs,
    path::{Path, PathBuf},
    time::{SystemTime, UNIX_EPOCH},
};

use anyhow::{Context, Result};
use rmcp::schemars::{self, JsonSchema};
use serde::{Deserialize, Serialize};

#[derive(Clone, Copy, Debug, Deserialize, Serialize, JsonSchema, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum State {
    /// Waiting for a prompt.
    Idle,
    /// Running a turn.
    Working,
    /// Blocked on the user, e.g. a permission prompt. `message` says on what.
    Waiting,
}

#[derive(Clone, Debug, Deserialize, Serialize, JsonSchema)]
pub struct Session {
    pub session_id: String,
    pub cwd: String,
    /// tmux session the Claude Code process runs in.
    pub tmux_session: String,
    /// tmux pane id (`%N`), stable for the life of the pane.
    pub tmux_pane: String,
    pub state: State,
    /// The last prompt submitted, truncated.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub last_prompt: Option<String>,
    /// Why the session is waiting, as Claude Code reported it.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,
    /// Unix seconds of the last hook event.
    pub updated_at: u64,
}

pub fn now() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs())
        .unwrap_or_default()
}

/// `$XDG_STATE_HOME/claude-sessions`, else `~/.local/state/claude-sessions`.
/// The hook runs under Claude Code and the server under systemd, so both
/// derive it the same way instead of being told.
pub fn default_dir() -> Result<PathBuf> {
    let base = match std::env::var_os("XDG_STATE_HOME").filter(|v| !v.is_empty()) {
        Some(dir) => PathBuf::from(dir),
        None => {
            PathBuf::from(std::env::var_os("HOME").context("HOME is not set")?).join(".local/state")
        }
    };
    Ok(base.join("claude-sessions"))
}

fn path(dir: &Path, session_id: &str) -> Result<PathBuf> {
    // The id becomes a file name; never let it walk out of the directory.
    anyhow::ensure!(
        !session_id.is_empty()
            && session_id
                .chars()
                .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_'),
        "unexpected session id {session_id:?}"
    );
    Ok(dir.join(format!("{session_id}.json")))
}

pub fn read(dir: &Path, session_id: &str) -> Option<Session> {
    let bytes = fs::read(path(dir, session_id).ok()?).ok()?;
    serde_json::from_slice(&bytes).ok()
}

pub fn write(dir: &Path, session: &Session) -> Result<()> {
    fs::create_dir_all(dir).with_context(|| format!("creating {}", dir.display()))?;
    let target = path(dir, &session.session_id)?;
    // Write and rename, so the server never reads half a file.
    let tmp = target.with_extension("json.tmp");
    fs::write(&tmp, serde_json::to_vec(session)?)
        .with_context(|| format!("writing {}", tmp.display()))?;
    fs::rename(&tmp, &target).with_context(|| format!("renaming to {}", target.display()))
}

pub fn remove(dir: &Path, session_id: &str) -> Result<()> {
    match fs::remove_file(path(dir, session_id)?) {
        Err(e) if e.kind() != std::io::ErrorKind::NotFound => Err(e.into()),
        _ => Ok(()),
    }
}

/// Every readable session file. Unparsable ones are skipped.
pub fn list(dir: &Path) -> Vec<Session> {
    let Ok(entries) = fs::read_dir(dir) else {
        return Vec::new();
    };
    entries
        .flatten()
        .filter(|e| e.path().extension().is_some_and(|ext| ext == "json"))
        .filter_map(|e| serde_json::from_slice(&fs::read(e.path()).ok()?).ok())
        .collect()
}
