//! The few tmux commands we need, against the default server of the user
//! we run as.

use std::{collections::HashSet, process::Stdio, time::Duration};

use anyhow::{Context, Result, bail};
use tokio::{io::AsyncWriteExt, process::Command};

async fn tmux(args: &[&str]) -> Result<String> {
    let out = Command::new("tmux")
        .args(args)
        .stdin(Stdio::null())
        .output()
        .await
        .context("running tmux")?;
    if !out.status.success() {
        bail!(
            "tmux {}: {}",
            args.first().unwrap_or(&""),
            String::from_utf8_lossy(&out.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&out.stdout).into_owned())
}

/// Ids (`%N`) of every pane on the server. No server means no panes.
pub async fn live_panes() -> HashSet<String> {
    tmux(&["list-panes", "-a", "-F", "#{pane_id}"])
        .await
        .map(|out| out.lines().map(str::to_owned).collect())
        .unwrap_or_default()
}

/// The tmux session a pane belongs to.
pub async fn session_of(pane: &str) -> Result<String> {
    Ok(
        tmux(&["display-message", "-p", "-t", pane, "#{session_name}"])
            .await?
            .trim()
            .to_owned(),
    )
}

/// The last `lines` lines of a pane, history included, joined where tmux
/// wrapped them and without the blank tail of the screen.
pub async fn capture(pane: &str, lines: usize) -> Result<String> {
    let start = format!("-{lines}");
    let out = tmux(&["capture-pane", "-p", "-J", "-t", pane, "-S", &start]).await?;
    let all: Vec<&str> = out.trim_end().lines().collect();
    Ok(all[all.len().saturating_sub(lines)..].join("\n"))
}

/// Type `text` into a pane, then press Enter when `submit`. A single line
/// goes in as keystrokes, so it also answers a menu ("1", "2", ...). More
/// than one goes in as a bracketed paste, so the newlines stay in the
/// prompt instead of submitting it line by line.
pub async fn send(pane: &str, text: &str, submit: bool) -> Result<()> {
    if text.contains('\n') {
        const BUFFER: &str = "claude-sessions";
        let mut child = Command::new("tmux")
            .args(["load-buffer", "-b", BUFFER, "-"])
            .stdin(Stdio::piped())
            .spawn()
            .context("running tmux")?;
        let mut stdin = child.stdin.take().context("tmux stdin")?;
        stdin.write_all(text.as_bytes()).await?;
        drop(stdin);
        if !child.wait().await?.success() {
            bail!("tmux load-buffer failed");
        }
        tmux(&["paste-buffer", "-d", "-p", "-b", BUFFER, "-t", pane]).await?;
    } else if !text.is_empty() {
        tmux(&["send-keys", "-t", pane, "-l", "--", text]).await?;
    }
    if submit {
        // Claude Code reads input in chunks; an Enter in the same chunk as
        // the text can be taken as part of it.
        tokio::time::sleep(Duration::from_millis(150)).await;
        tmux(&["send-keys", "-t", pane, "Enter"]).await?;
    }
    Ok(())
}
