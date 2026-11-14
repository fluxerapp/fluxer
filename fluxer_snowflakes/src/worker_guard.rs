// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::types::SERVICE_NAME;
use fluxer_svc::transport::{NoResponders, Transport, TransportSubscriber, reply_message};
use std::future::Future;
use std::time::Duration;

const PROBE_TIMEOUT: Duration = Duration::from_secs(1);

pub async fn run<T, F>(enabled: bool, transport: &T, worker_id: u32, serve: F) -> anyhow::Result<()>
where
    T: Transport,
    F: Future<Output = anyhow::Result<()>>,
{
    if !enabled {
        return serve.await;
    }
    let subject = format!("svc.{SERVICE_NAME}.worker.{worker_id}");
    match transport.request(&subject, b"", PROBE_TIMEOUT).await {
        Ok(_) => anyhow::bail!(
            "another snowflakes instance answers {subject}, refusing to hand out worker id {worker_id} twice"
        ),
        Err(error) if error.is::<NoResponders>() || error.is::<tokio::time::error::Elapsed>() => {}
        Err(error) => return Err(error.context(format!("worker guard probe on {subject} failed"))),
    }
    let mut subscriber = transport.subscribe(&subject).await?;
    transport.flush().await?;
    tracing::info!(subject, "claimed snowflakes worker id");
    let payload = worker_id.to_string();
    let answer = async {
        while let Some(message) = subscriber.next().await {
            if let Err(error) = reply_message(&message, transport, payload.as_bytes()).await {
                tracing::debug!(error = %error, subject, "failed to answer worker guard probe");
            }
        }
        anyhow::bail!("worker guard subscription on {subject} closed")
    };
    tokio::select! {
        result = serve => result,
        result = answer => result,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fluxer_svc::transport::InMemoryTransport;

    #[tokio::test]
    async fn second_instance_with_the_same_worker_id_refuses_to_start() {
        let transport = InMemoryTransport::new();
        let (stop, stopped) = tokio::sync::oneshot::channel::<()>();
        let first_transport = transport.clone();
        let first = tokio::spawn(async move {
            run(true, &first_transport, 7, async {
                stopped.await.ok();
                Ok(())
            })
            .await
        });
        tokio::time::sleep(Duration::from_millis(50)).await;

        let second = run(true, &transport, 7, async { Ok(()) }).await;
        assert!(second.is_err());
        run(true, &transport, 8, async { Ok(()) }).await.unwrap();

        stop.send(()).unwrap();
        first.await.unwrap().unwrap();
        run(true, &transport, 7, async { Ok(()) }).await.unwrap();
    }
}
