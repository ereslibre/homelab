//! `claude-sessions serve`: the MCP server over Streamable HTTP.
//!
//! Only sessions with a state file and a live pane are visible, and only
//! they can be read or typed into: a target never resolves to an arbitrary
//! tmux pane.

use std::{
    net::{IpAddr, SocketAddr},
    path::PathBuf,
    sync::Arc,
};

use anyhow::Result;
use axum::{
    extract::{ConnectInfo, Request, State},
    http::StatusCode,
    middleware::{self, Next},
    response::{IntoResponse, Response},
};
use rmcp::{
    Json, ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{Implementation, ServerCapabilities, ServerConfig},
    schemars::{self, JsonSchema},
    tool, tool_handler, tool_router,
    transport::streamable_http_server::{
        StreamableHttpServerConfig, StreamableHttpService, session::local::LocalSessionManager,
    },
};
use serde::{Deserialize, Serialize};
use tokio_util::sync::CancellationToken;

use crate::{
    state::{self, Session},
    tmux,
};

const DEFAULT_LINES: usize = 60;
const MAX_LINES: usize = 500;

#[derive(Clone)]
struct Sessions {
    dir: Arc<PathBuf>,
    tool_router: ToolRouter<Self>,
}

#[derive(Serialize, JsonSchema)]
struct SessionList {
    sessions: Vec<Listed>,
}

#[derive(Serialize, JsonSchema)]
struct Listed {
    #[serde(flatten)]
    session: Session,
    /// Seconds since the last hook event.
    seconds_since_update: u64,
}

#[derive(Deserialize, JsonSchema)]
struct ReadArgs {
    /// A tmux session name, or a pane id (`%N`) when the tmux session runs
    /// more than one Claude Code.
    target: String,
    /// How many lines to return, from the bottom of the pane. Defaults to 60.
    #[serde(default)]
    lines: Option<usize>,
}

#[derive(Deserialize, JsonSchema)]
struct SendArgs {
    /// A tmux session name, or a pane id (`%N`) when the tmux session runs
    /// more than one Claude Code.
    target: String,
    /// What to type. To pick an option in a menu such as a permission
    /// prompt, send its number with `submit` false.
    text: String,
    /// Press Enter after the text. Defaults to true.
    #[serde(default)]
    submit: Option<bool>,
}

impl Sessions {
    /// Sessions whose pane still exists, ordered by tmux session.
    async fn live(&self) -> Vec<Session> {
        let panes = tmux::live_panes().await;
        let mut sessions: Vec<_> = state::list(&self.dir)
            .into_iter()
            .filter(|s| panes.contains(&s.tmux_pane))
            .collect();
        sessions
            .sort_by(|a, b| (&a.tmux_session, &a.tmux_pane).cmp(&(&b.tmux_session, &b.tmux_pane)));
        sessions
    }

    async fn resolve(&self, target: &str) -> Result<Session, String> {
        let sessions = self.live().await;
        let mut matches: Vec<_> = sessions
            .iter()
            .filter(|s| s.tmux_pane == target || s.tmux_session == target)
            .collect();
        match matches.len() {
            1 => Ok(matches.remove(0).clone()),
            0 => {
                let known: Vec<_> = sessions.iter().map(|s| s.tmux_session.as_str()).collect();
                Err(format!(
                    "no Claude Code session at {target:?}; running: {}",
                    if known.is_empty() {
                        "none".to_owned()
                    } else {
                        known.join(", ")
                    }
                ))
            }
            _ => {
                let panes: Vec<_> = matches
                    .iter()
                    .map(|s| format!("{} ({})", s.tmux_pane, s.cwd))
                    .collect();
                Err(format!(
                    "tmux session {target:?} runs more than one Claude Code; pick a pane: {}",
                    panes.join(", ")
                ))
            }
        }
    }
}

#[tool_router(router = tool_router)]
impl Sessions {
    fn new(dir: Arc<PathBuf>) -> Self {
        Self {
            dir,
            tool_router: Self::tool_router(),
        }
    }

    /// List the Claude Code sessions running in tmux on this machine: where
    /// they run, what they were last asked, and whether they are idle,
    /// working, or waiting on the user (and on what).
    #[tool(annotations(title = "List sessions", read_only_hint = true))]
    async fn list_sessions(&self) -> Json<SessionList> {
        let now = state::now();
        Json(SessionList {
            sessions: self
                .live()
                .await
                .into_iter()
                .map(|session| Listed {
                    seconds_since_update: now.saturating_sub(session.updated_at),
                    session,
                })
                .collect(),
        })
    }

    /// Show the bottom of a session's terminal, as the user would see it.
    #[tool(annotations(title = "Read session", read_only_hint = true))]
    async fn read_session(&self, Parameters(args): Parameters<ReadArgs>) -> Result<String, String> {
        let session = self.resolve(&args.target).await?;
        let lines = args.lines.unwrap_or(DEFAULT_LINES).clamp(1, MAX_LINES);
        tmux::capture(&session.tmux_pane, lines)
            .await
            .map_err(|e| e.to_string())
    }

    /// Type into a session as the user would: a prompt, or an answer to a
    /// question or permission prompt it is waiting on.
    #[tool(annotations(
        title = "Send to session",
        read_only_hint = false,
        destructive_hint = true,
        idempotent_hint = false,
        open_world_hint = false
    ))]
    async fn send_to_session(
        &self,
        Parameters(args): Parameters<SendArgs>,
    ) -> Result<String, String> {
        let session = self.resolve(&args.target).await?;
        tmux::send(&session.tmux_pane, &args.text, args.submit.unwrap_or(true))
            .await
            .map_err(|e| e.to_string())?;
        Ok(format!(
            "sent to {} ({})",
            session.tmux_session, session.tmux_pane
        ))
    }
}

#[tool_handler(router = self.tool_router)]
impl ServerHandler for Sessions {
    fn get_info(&self) -> ServerConfig {
        ServerConfig::new(ServerCapabilities::builder().enable_tools().build())
            .with_server_info(Implementation::new(
                env!("CARGO_PKG_NAME"),
                env!("CARGO_PKG_VERSION"),
            ))
            .with_instructions(
                "The Claude Code sessions running in tmux on one machine. \
                 Start with list_sessions; read_session shows a session's screen \
                 and send_to_session types into it. Address a session by its \
                 tmux session name.",
            )
    }
}

/// Refuse peers outside `allow`, loopback aside. Empty lets everyone in.
async fn check_peer(
    State(allow): State<Arc<Vec<IpAddr>>>,
    ConnectInfo(peer): ConnectInfo<SocketAddr>,
    request: Request,
    next: Next,
) -> Response {
    let ip = peer.ip().to_canonical();
    if allow.is_empty() || ip.is_loopback() || allow.contains(&ip) {
        next.run(request).await
    } else {
        StatusCode::FORBIDDEN.into_response()
    }
}

pub async fn run(
    dir: PathBuf,
    listen: SocketAddr,
    allowed_hosts: Vec<String>,
    allow_from: Vec<IpAddr>,
) -> Result<()> {
    let dir = Arc::new(dir);
    let cancel = CancellationToken::new();
    let mut config = StreamableHttpServerConfig::default().with_cancellation_token(cancel.clone());
    // Without hosts, rmcp only accepts loopback `Host` headers.
    if !allowed_hosts.is_empty() {
        config = config.with_allowed_hosts(allowed_hosts);
    }
    let service: StreamableHttpService<Sessions, LocalSessionManager> = StreamableHttpService::new(
        move || Ok(Sessions::new(dir.clone())),
        Default::default(),
        config,
    );
    let router =
        axum::Router::new()
            .nest_service("/mcp", service)
            .layer(middleware::from_fn_with_state(
                Arc::new(allow_from),
                check_peer,
            ));
    let listener = tokio::net::TcpListener::bind(listen).await?;
    eprintln!("listening on http://{}/mcp", listener.local_addr()?);
    axum::serve(
        listener,
        router.into_make_service_with_connect_info::<SocketAddr>(),
    )
    .with_graceful_shutdown(async move {
        let _ = tokio::signal::ctrl_c().await;
        cancel.cancel();
    })
    .await?;
    Ok(())
}
