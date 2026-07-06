// SPDX-License-Identifier: AGPL-3.0-or-later

use async_trait::async_trait;
use bytes::Bytes;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;
use tokio::sync::Notify;
use tracing::{info, warn};

const SLOW_CONSUMER_LOG_INTERVAL_MS: u64 = 1_000;
const SLOW_CONSUMER_LOG_NEVER_MS: u64 = u64::MAX;

#[async_trait]
pub trait Transport: Clone + Send + Sync + 'static {
    type Message: TransportMessage + Send + Sync + 'static;
    type Subscriber: TransportSubscriber<Message = Self::Message> + Send;

    async fn request(
        &self,
        subject: &str,
        payload: &[u8],
        timeout: Duration,
    ) -> anyhow::Result<Vec<u8>>;
    async fn subscribe(&self, subject: &str) -> anyhow::Result<Self::Subscriber>;
    async fn subscribe_queue(
        &self,
        subject: &str,
        queue_group: &str,
    ) -> anyhow::Result<Self::Subscriber>;
    async fn publish(&self, subject: &str, payload: &[u8]) -> anyhow::Result<()>;
    async fn wait_for_reconnect(&self);
    fn is_connected(&self) -> bool;
}

#[async_trait]
pub trait TransportSubscriber {
    type Message: TransportMessage + Send + Sync + 'static;

    async fn next(&mut self) -> Option<Self::Message>;
}

pub trait TransportMessage {
    fn payload(&self) -> &[u8];
    fn subject(&self) -> &str;
    fn reply_subject(&self) -> Option<&str>;

    fn has_reply(&self) -> bool {
        self.reply_subject().is_some()
    }
}

pub async fn reply_message<T, M>(message: &M, transport: &T, payload: &[u8]) -> anyhow::Result<()>
where
    T: Transport,
    M: TransportMessage,
{
    if let Some(subject) = message.reply_subject() {
        transport.publish(subject, payload).await?;
    }
    Ok(())
}

#[derive(Clone)]
pub struct NatsTransport {
    client: async_nats::Client,
    reconnect_notify: Arc<Notify>,
}

pub struct Subscriber {
    inner: async_nats::Subscriber,
}

pub struct NatsMessage {
    inner: async_nats::Message,
}

impl NatsTransport {
    pub async fn connect(url: &str) -> anyhow::Result<Self> {
        let reconnect_notify = Arc::new(Notify::new());

        let event_notify = reconnect_notify.clone();
        let slow_consumer_last_log_ms = Arc::new(AtomicU64::new(SLOW_CONSUMER_LOG_NEVER_MS));
        let options = async_nats::ConnectOptions::new().event_callback(move |event| {
            let notify = event_notify.clone();
            let slow_consumer_last_log_ms = slow_consumer_last_log_ms.clone();
            async move {
                match event {
                    async_nats::Event::Connected => {
                        info!("NATS reconnected");
                        notify.notify_waiters();
                    }
                    async_nats::Event::Disconnected => {
                        warn!("NATS disconnected");
                    }
                    async_nats::Event::LameDuckMode => {
                        warn!("NATS server entering lame duck mode");
                    }
                    async_nats::Event::SlowConsumer(sid) => {
                        if should_log_slow_consumer(
                            crate::metrics::now_ms().max(0) as u64,
                            &slow_consumer_last_log_ms,
                        ) {
                            warn!(subscription_id = sid, "NATS slow consumer detected");
                        }
                    }
                    other => {
                        info!(event = %other, "NATS event");
                    }
                }
            }
        });

        let client = options.connect(url).await?;
        info!(url, "connected to NATS");

        Ok(Self {
            client,
            reconnect_notify,
        })
    }

    pub async fn request(
        &self,
        subject: &str,
        payload: &[u8],
        timeout: Duration,
    ) -> anyhow::Result<Vec<u8>> {
        <Self as Transport>::request(self, subject, payload, timeout).await
    }

    pub async fn subscribe(&self, subject: &str) -> anyhow::Result<Subscriber> {
        <Self as Transport>::subscribe(self, subject).await
    }

    pub async fn subscribe_queue(
        &self,
        subject: &str,
        queue_group: &str,
    ) -> anyhow::Result<Subscriber> {
        <Self as Transport>::subscribe_queue(self, subject, queue_group).await
    }

    pub async fn publish(&self, subject: &str, payload: &[u8]) -> anyhow::Result<()> {
        <Self as Transport>::publish(self, subject, payload).await
    }

    pub async fn wait_for_reconnect(&self) {
        <Self as Transport>::wait_for_reconnect(self).await;
    }

    pub fn is_connected(&self) -> bool {
        <Self as Transport>::is_connected(self)
    }
}

fn should_log_slow_consumer(now_ms: u64, last_log_ms: &AtomicU64) -> bool {
    let mut last = last_log_ms.load(Ordering::Relaxed);
    loop {
        if last != SLOW_CONSUMER_LOG_NEVER_MS
            && now_ms.saturating_sub(last) < SLOW_CONSUMER_LOG_INTERVAL_MS
        {
            return false;
        }

        match last_log_ms.compare_exchange_weak(last, now_ms, Ordering::Relaxed, Ordering::Relaxed)
        {
            Ok(_) => return true,
            Err(actual) => last = actual,
        }
    }
}

#[async_trait]
impl Transport for NatsTransport {
    type Message = NatsMessage;
    type Subscriber = Subscriber;

    async fn request(
        &self,
        subject: &str,
        payload: &[u8],
        timeout: Duration,
    ) -> anyhow::Result<Vec<u8>> {
        let subject = async_nats::Subject::from(subject.to_owned());
        let response = tokio::time::timeout(
            timeout,
            self.client
                .request(subject, Bytes::copy_from_slice(payload)),
        )
        .await??;
        Ok(response.payload.to_vec())
    }

    async fn subscribe(&self, subject: &str) -> anyhow::Result<Subscriber> {
        let subject = async_nats::Subject::from(subject.to_owned());
        let inner = self.client.subscribe(subject).await?;
        Ok(Subscriber { inner })
    }

    async fn subscribe_queue(
        &self,
        subject: &str,
        queue_group: &str,
    ) -> anyhow::Result<Subscriber> {
        let subject = async_nats::Subject::from(subject.to_owned());
        let queue = queue_group.to_owned();
        let inner = self.client.queue_subscribe(subject, queue).await?;
        Ok(Subscriber { inner })
    }

    async fn publish(&self, subject: &str, payload: &[u8]) -> anyhow::Result<()> {
        let subject = async_nats::Subject::from(subject.to_owned());
        self.client
            .publish(subject, Bytes::copy_from_slice(payload))
            .await?;
        Ok(())
    }

    async fn wait_for_reconnect(&self) {
        self.reconnect_notify.notified().await;
    }

    fn is_connected(&self) -> bool {
        matches!(
            self.client.connection_state(),
            async_nats::connection::State::Connected
        )
    }
}

#[async_trait]
impl TransportSubscriber for Subscriber {
    type Message = NatsMessage;

    async fn next(&mut self) -> Option<NatsMessage> {
        use futures::StreamExt;
        self.inner.next().await.map(|inner| NatsMessage { inner })
    }
}

impl TransportMessage for NatsMessage {
    fn payload(&self) -> &[u8] {
        &self.inner.payload
    }

    fn subject(&self) -> &str {
        self.inner.subject.as_str()
    }

    fn reply_subject(&self) -> Option<&str> {
        self.inner.reply.as_ref().map(|subject| subject.as_str())
    }
}

impl NatsMessage {
    pub async fn reply(&self, transport: &NatsTransport, payload: &[u8]) -> anyhow::Result<()> {
        reply_message(self, transport, payload).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slow_consumer_logs_initial_event_then_throttles() {
        let last_log_ms = AtomicU64::new(SLOW_CONSUMER_LOG_NEVER_MS);

        assert!(should_log_slow_consumer(10, &last_log_ms));
        assert!(!should_log_slow_consumer(1_009, &last_log_ms));
        assert!(should_log_slow_consumer(1_010, &last_log_ms));
    }

    #[test]
    fn slow_consumer_throttle_allows_one_racing_logger() {
        let last_log_ms = Arc::new(AtomicU64::new(SLOW_CONSUMER_LOG_NEVER_MS));
        let logged = Arc::new(AtomicU64::new(0));

        std::thread::scope(|scope| {
            for _ in 0..8 {
                let last_log_ms = last_log_ms.clone();
                let logged = logged.clone();
                scope.spawn(move || {
                    if should_log_slow_consumer(42, &last_log_ms) {
                        logged.fetch_add(1, Ordering::Relaxed);
                    }
                });
            }
        });

        assert_eq!(logged.load(Ordering::Relaxed), 1);
    }
}

#[cfg(debug_assertions)]
mod in_memory {
    use super::{Transport, TransportMessage, TransportSubscriber};
    use async_trait::async_trait;
    use std::collections::HashMap;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::Duration;
    use tokio::sync::{Mutex, mpsc};

    #[derive(Clone, Default)]
    pub struct InMemoryTransport {
        bus: Arc<InMemoryBus>,
    }

    #[derive(Default)]
    struct InMemoryBus {
        next_inbox: AtomicU64,
        subscribers: Mutex<InMemorySubscribers>,
    }

    #[derive(Default)]
    struct InMemorySubscribers {
        plain: HashMap<String, Vec<mpsc::UnboundedSender<InMemoryMessage>>>,
        queue: HashMap<QueueKey, QueueSubscribers>,
    }

    #[derive(Clone, Debug, Eq, Hash, PartialEq)]
    struct QueueKey {
        subject: String,
        queue_group: String,
    }

    #[derive(Default)]
    struct QueueSubscribers {
        next: usize,
        senders: Vec<mpsc::UnboundedSender<InMemoryMessage>>,
    }

    pub struct InMemorySubscriber {
        receiver: mpsc::UnboundedReceiver<InMemoryMessage>,
    }

    #[derive(Clone)]
    pub struct InMemoryMessage {
        subject: String,
        payload: Vec<u8>,
        reply_subject: Option<String>,
    }

    impl InMemoryTransport {
        pub fn new() -> Self {
            Self::default()
        }

        async fn publish_with_reply(
            &self,
            subject: &str,
            payload: &[u8],
            reply_subject: Option<String>,
        ) -> anyhow::Result<()> {
            let message = InMemoryMessage {
                subject: subject.to_owned(),
                payload: payload.to_vec(),
                reply_subject,
            };
            let mut subscribers = self.bus.subscribers.lock().await;

            for (pattern, senders) in subscribers.plain.iter_mut() {
                if !matches_subject(pattern, subject) {
                    continue;
                }
                senders.retain(|sender| sender.send(message.clone()).is_ok());
            }

            for (key, queue) in subscribers.queue.iter_mut() {
                if !matches_subject(&key.subject, subject) {
                    continue;
                }
                send_one_queue_subscriber(queue, message.clone());
            }

            Ok(())
        }
    }

    #[async_trait]
    impl Transport for InMemoryTransport {
        type Message = InMemoryMessage;
        type Subscriber = InMemorySubscriber;

        async fn request(
            &self,
            subject: &str,
            payload: &[u8],
            timeout: Duration,
        ) -> anyhow::Result<Vec<u8>> {
            let inbox = format!(
                "_INBOX.{}",
                self.bus.next_inbox.fetch_add(1, Ordering::Relaxed)
            );
            let mut subscriber = self.subscribe(&inbox).await?;
            self.publish_with_reply(subject, payload, Some(inbox))
                .await?;
            let message = tokio::time::timeout(timeout, subscriber.next())
                .await?
                .ok_or_else(|| anyhow::anyhow!("in-memory request subscriber closed"))?;
            Ok(message.payload)
        }

        async fn subscribe(&self, subject: &str) -> anyhow::Result<Self::Subscriber> {
            let (sender, receiver) = mpsc::unbounded_channel();
            self.bus
                .subscribers
                .lock()
                .await
                .plain
                .entry(subject.to_owned())
                .or_default()
                .push(sender);
            Ok(InMemorySubscriber { receiver })
        }

        async fn subscribe_queue(
            &self,
            subject: &str,
            queue_group: &str,
        ) -> anyhow::Result<Self::Subscriber> {
            let (sender, receiver) = mpsc::unbounded_channel();
            self.bus
                .subscribers
                .lock()
                .await
                .queue
                .entry(QueueKey {
                    subject: subject.to_owned(),
                    queue_group: queue_group.to_owned(),
                })
                .or_default()
                .senders
                .push(sender);
            Ok(InMemorySubscriber { receiver })
        }

        async fn publish(&self, subject: &str, payload: &[u8]) -> anyhow::Result<()> {
            self.publish_with_reply(subject, payload, None).await
        }

        async fn wait_for_reconnect(&self) {
            std::future::pending::<()>().await;
        }

        fn is_connected(&self) -> bool {
            true
        }
    }

    #[async_trait]
    impl TransportSubscriber for InMemorySubscriber {
        type Message = InMemoryMessage;

        async fn next(&mut self) -> Option<Self::Message> {
            self.receiver.recv().await
        }
    }

    impl TransportMessage for InMemoryMessage {
        fn payload(&self) -> &[u8] {
            &self.payload
        }

        fn subject(&self) -> &str {
            &self.subject
        }

        fn reply_subject(&self) -> Option<&str> {
            self.reply_subject.as_deref()
        }
    }

    fn send_one_queue_subscriber(queue: &mut QueueSubscribers, message: InMemoryMessage) {
        while !queue.senders.is_empty() {
            let index = queue.next % queue.senders.len();
            queue.next = queue.next.wrapping_add(1);
            if queue.senders[index].send(message.clone()).is_ok() {
                return;
            }
            queue.senders.remove(index);
            if index < queue.next {
                queue.next = queue.next.saturating_sub(1);
            }
        }
    }

    fn matches_subject(pattern: &str, subject: &str) -> bool {
        if pattern == subject {
            return true;
        }
        pattern
            .strip_suffix(".>")
            .is_some_and(|prefix| subject.starts_with(&format!("{prefix}.")))
    }
}

#[cfg(debug_assertions)]
pub use in_memory::InMemoryTransport;
