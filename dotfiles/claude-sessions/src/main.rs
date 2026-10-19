//! Track the Claude Code sessions running in tmux on this machine, and
//! expose them over MCP so an agent elsewhere can see and drive them.
//!
//! - `claude-sessions hook` is wired as a Claude Code hook and keeps one
//!   state file per session.
//! - `claude-sessions serve` reads those files and tmux, and serves
//!   `list_sessions`, `read_session` and `send_to_session` at `/mcp`.

mod hook;
mod server;
mod state;
mod tmux;

use std::{
    net::{IpAddr, SocketAddr},
    path::PathBuf,
};

use anyhow::Result;
use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(version, about)]
struct Cli {
    /// Where the session state files live.
    #[arg(long, env = "CLAUDE_SESSIONS_STATE_DIR", global = true)]
    state_dir: Option<PathBuf>,
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Record a Claude Code hook event read from stdin. Never fails the hook.
    Hook,
    /// Serve the sessions over MCP (Streamable HTTP at `/mcp`).
    Serve {
        /// Address to listen on.
        #[arg(long, default_value = "127.0.0.1:8765")]
        listen: SocketAddr,
        /// Accepted `Host` header, with or without port; repeat for more.
        /// Without any, only loopback hosts are accepted.
        #[arg(long = "allowed-host")]
        allowed_hosts: Vec<String>,
        /// Client address allowed to connect; repeat for more. Loopback is
        /// always allowed. Without any, every client is.
        #[arg(long)]
        allow_from: Vec<IpAddr>,
    },
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();
    let dir = match cli.state_dir {
        Some(dir) => dir,
        None => state::default_dir()?,
    };
    match cli.command {
        Command::Hook => {
            // A broken tracker must never get in the way of Claude Code: a
            // failing hook shows up in the session, so say why and exit 0.
            if let Err(e) = hook::run(&dir).await {
                eprintln!("claude-sessions: {e:#}");
            }
            Ok(())
        }
        Command::Serve {
            listen,
            allowed_hosts,
            allow_from,
        } => server::run(dir, listen, allowed_hosts, allow_from).await,
    }
}
