// SPDX-License-Identifier: AGPL-3.0-or-later

pub async fn wait_for_shutdown() {
    let ctrl_c = tokio::signal::ctrl_c();

    #[cfg(unix)]
    {
        use tokio::signal::unix::{SignalKind, signal};
        let mut sigterm =
            signal(SignalKind::terminate()).expect("failed to install SIGTERM handler");
        tokio::select! {
            _ = ctrl_c => {
                tracing::info!("received SIGINT, beginning shutdown");
            }
            _ = sigterm.recv() => {
                tracing::info!("received SIGTERM, beginning shutdown");
            }
        }
    }

    #[cfg(not(unix))]
    {
        ctrl_c.await.ok();
        tracing::info!("received SIGINT, beginning shutdown");
    }
}
