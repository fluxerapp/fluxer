// SPDX-License-Identifier: AGPL-3.0-or-later

use async_trait::async_trait;
use bytes::Bytes;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;
use tokio::sync::Notify;
use tracing::{debug, info, warn};

const NATS_SUBSCRIPTION_CAPACITY: usize = 8_192;
const SLOW_CONSUMER_LOG_INTERVAL_MS: u64 = 1_000;
const SLOW_CONSUMER_LOG_NEVER_MS: u64 = u64::MAX;
const DRAIN_HEADROOM_MESSAGES: u64 = 32;

#[derive(Debug)]
pub struct NoResponders;

impl std::fmt::Display for NoResponders {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("no responders")
    }
}

impl std::error::Error for NoResponders {}

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
    async fn flush(&self) -> anyhow::Result<()>;
    async fn wait_for_reconnect(&self);
}

#[async_trait]
pub trait TransportSubscriber {
    type Message: TransportMessage + Send + Sync + 'static;

    async fn next(&mut self) -> Option<Self::Message>;
    async fn drain(&mut self) -> anyhow::Result<()>;
    async fn unsubscribe(&mut self) -> anyhow::Result<()>;
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

pub(crate) async fn reply_bytes(
    message: &impl TransportMessage,
    transport: &impl Transport,
    payload: &[u8],
) {
    if let Err(error) = reply_message(message, transport, payload).await {
        debug!(error = %error, subject = message.subject(), "failed to send service reply");
    }
}

pub(crate) async fn reply_json_error(
    message: &impl TransportMessage,
    transport: &impl Transport,
    code: &str,
) {
    if !message.has_reply() {
        return;
    }
    let payload = serde_json::to_vec(&serde_json::json!({ "error": code }))
        .expect("service error responses contain only a JSON-serializable string");
    reply_bytes(message, transport, &payload).await;
}

#[derive(Clone)]
pub struct NatsTransport {
    client: async_nats::Client,
    reconnect_notify: Arc<Notify>,
}

pub struct Subscriber {
    inner: async_nats::Subscriber,
    received: u64,
}

pub struct NatsMessage {
    inner: async_nats::Message,
}

impl NatsTransport {
    pub async fn connect(url: &str, auth_token: Option<&str>) -> anyhow::Result<Self> {
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

        let options = match auth_token {
            Some(token) => options.token(token.to_owned()),
            None => options,
        };

        let client = options
            .subscription_capacity(NATS_SUBSCRIPTION_CAPACITY)
            .connect(url)
            .await?;
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

    pub async fn subscribe_queue(
        &self,
        subject: &str,
        queue_group: &str,
    ) -> anyhow::Result<Subscriber> {
        <Self as Transport>::subscribe_queue(self, subject, queue_group).await
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
        .await?;
        match response {
            Ok(response) => Ok(response.payload.to_vec()),
            Err(error) if error.kind() == async_nats::RequestErrorKind::NoResponders => {
                Err(NoResponders.into())
            }
            Err(error) => Err(error.into()),
        }
    }

    async fn subscribe(&self, subject: &str) -> anyhow::Result<Subscriber> {
        let subject = async_nats::Subject::from(subject.to_owned());
        let inner = self.client.subscribe(subject).await?;
        Ok(Subscriber { inner, received: 0 })
    }

    async fn subscribe_queue(
        &self,
        subject: &str,
        queue_group: &str,
    ) -> anyhow::Result<Subscriber> {
        let subject = async_nats::Subject::from(subject.to_owned());
        let queue = queue_group.to_owned();
        let inner = self.client.queue_subscribe(subject, queue).await?;
        Ok(Subscriber { inner, received: 0 })
    }

    async fn publish(&self, subject: &str, payload: &[u8]) -> anyhow::Result<()> {
        let subject = async_nats::Subject::from(subject.to_owned());
        self.client
            .publish(subject, Bytes::copy_from_slice(payload))
            .await?;
        Ok(())
    }

    async fn flush(&self) -> anyhow::Result<()> {
        self.client.flush().await?;
        Ok(())
    }

    async fn wait_for_reconnect(&self) {
        self.reconnect_notify.notified().await;
    }
}

#[async_trait]
impl TransportSubscriber for Subscriber {
    type Message = NatsMessage;

    async fn next(&mut self) -> Option<NatsMessage> {
        use futures::StreamExt;
        let inner = self.inner.next().await?;
        self.received += 1;
        Some(NatsMessage { inner })
    }

    async fn drain(&mut self) -> anyhow::Result<()> {
        self.inner
            .unsubscribe_after(self.received + DRAIN_HEADROOM_MESSAGES)
            .await?;
        Ok(())
    }

    async fn unsubscribe(&mut self) -> anyhow::Result<()> {
        self.inner.unsubscribe().await?;
        Ok(())
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

#[cfg(any(test, feature = "mock"))]
mod in_memory {
    use super::{NoResponders, Transport, TransportMessage, TransportSubscriber};
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
        ) -> bool {
            let message = InMemoryMessage {
                subject: subject.to_owned(),
                payload: payload.to_vec(),
                reply_subject,
            };
            let mut subscribers = self.bus.subscribers.lock().await;
            let mut delivered = false;

            for (pattern, senders) in subscribers.plain.iter_mut() {
                if pattern != subject {
                    continue;
                }
                senders.retain(|sender| sender.send(message.clone()).is_ok());
                delivered |= !senders.is_empty();
            }

            for (key, queue) in subscribers.queue.iter_mut() {
                if key.subject != subject {
                    continue;
                }
                delivered |= send_one_queue_subscriber(queue, message.clone());
            }

            delivered
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
            if !self.publish_with_reply(subject, payload, Some(inbox)).await {
                return Err(NoResponders.into());
            }
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
            self.publish_with_reply(subject, payload, None).await;
            Ok(())
        }

        async fn flush(&self) -> anyhow::Result<()> {
            Ok(())
        }

        async fn wait_for_reconnect(&self) {
            std::future::pending::<()>().await;
        }
    }

    #[async_trait]
    impl TransportSubscriber for InMemorySubscriber {
        type Message = InMemoryMessage;

        async fn next(&mut self) -> Option<Self::Message> {
            self.receiver.recv().await
        }

        async fn drain(&mut self) -> anyhow::Result<()> {
            self.receiver.close();
            Ok(())
        }

        async fn unsubscribe(&mut self) -> anyhow::Result<()> {
            self.receiver.close();
            Ok(())
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

    fn send_one_queue_subscriber(queue: &mut QueueSubscribers, message: InMemoryMessage) -> bool {
        while !queue.senders.is_empty() {
            let index = queue.next % queue.senders.len();
            queue.next = queue.next.wrapping_add(1);
            if queue.senders[index].send(message.clone()).is_ok() {
                return true;
            }
            queue.senders.remove(index);
            if index < queue.next {
                queue.next = queue.next.saturating_sub(1);
            }
        }
        false
    }
}

#[cfg(any(test, feature = "mock"))]
pub use in_memory::InMemoryTransport;
