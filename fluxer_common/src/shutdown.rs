// SPDX-License-Identifier: AGPL-3.0-or-later

use tokio_util::sync::CancellationToken;

pub fn signal_token() -> CancellationToken {
    let token = CancellationToken::new();
    let signals = Signals::install();
    let cancel = token.clone();
    tokio::spawn(async move {
        signals.recv().await;
        cancel.cancel();
    });
    token
}

pub async fn wait_for_shutdown() {
    Signals::install().recv().await;
}

#[cfg(unix)]
struct Signals {
    interrupt: tokio::signal::unix::Signal,
    terminate: tokio::signal::unix::Signal,
}

#[cfg(unix)]
impl Signals {
    fn install() -> Self {
        use tokio::signal::unix::{SignalKind, signal};
        Self {
            interrupt: signal(SignalKind::interrupt()).expect("failed to install SIGINT handler"),
            terminate: signal(SignalKind::terminate()).expect("failed to install SIGTERM handler"),
        }
    }

    async fn recv(mut self) {
        tokio::select! {
            _ = self.interrupt.recv() => {
                tracing::info!("received SIGINT, beginning shutdown");
            }
            _ = self.terminate.recv() => {
                tracing::info!("received SIGTERM, beginning shutdown");
            }
        }
    }
}

#[cfg(not(unix))]
struct Signals;

#[cfg(not(unix))]
impl Signals {
    fn install() -> Self {
        Self
    }

    async fn recv(self) {
        tokio::signal::ctrl_c().await.ok();
        tracing::info!("received SIGINT, beginning shutdown");
    }
}
