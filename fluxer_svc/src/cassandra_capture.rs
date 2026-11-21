// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context;
use async_nats::HeaderMap;
use async_nats::jetstream;
use fluxer_common::config::EnvMap;
use fluxer_common::metrics::render_counter;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::fmt::Write as _;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::Notify;

const PUBLISH_TIMEOUT: Duration = Duration::from_secs(2);
const OUTCOME_BATCH_SIZE: usize = 512;
const OUTCOME_FLUSH_DELAY: Duration = Duration::from_millis(5);
const OUTCOME_RETRY_DELAY: Duration = Duration::from_millis(100);
const CLOSE_POLL: Duration = Duration::from_millis(10);
const DEFAULT_STREAM: &str = "CASSANDRA_CAPTURE";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaptureMode {
    Off,
    Measure,
    On,
}

#[derive(Clone, Debug)]
pub struct CaptureConfig {
    pub mode: CaptureMode,
    pub nats_url: Option<String>,
    pub nats_token: Option<String>,
    pub stream: String,
    pub pod: String,
}

impl CaptureMode {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        Ok(
            match env
                .value("FLUXER_SVC_CASSANDRA_CAPTURE_MODE")
                .map(|value| value.trim().to_ascii_lowercase())
                .as_deref()
            {
                None | Some("off") => Self::Off,
                Some("measure") => Self::Measure,
                Some("on") => Self::On,
                Some(other) => {
                    anyhow::bail!("unsupported FLUXER_SVC_CASSANDRA_CAPTURE_MODE: {other}")
                }
            },
        )
    }
}

impl CaptureConfig {
    pub fn from_env(env: &EnvMap) -> anyhow::Result<Self> {
        let mode = CaptureMode::from_env(env)?;
        let nats_url = env.value("FLUXER_SVC_CASSANDRA_CAPTURE_NATS_URL");
        if mode == CaptureMode::On && nats_url.is_none() {
            anyhow::bail!(
                "FLUXER_SVC_CASSANDRA_CAPTURE_NATS_URL is required when FLUXER_SVC_CASSANDRA_CAPTURE_MODE is on"
            );
        }
        Ok(Self {
            mode,
            nats_url,
            nats_token: env.value("FLUXER_SVC_CASSANDRA_CAPTURE_NATS_TOKEN"),
            stream: env
                .value("FLUXER_SVC_CASSANDRA_CAPTURE_STREAM")
                .unwrap_or_else(|| DEFAULT_STREAM.to_owned()),
            pod: env
                .value("POD_NAME")
                .or_else(|| env.value("HOSTNAME"))
                .unwrap_or_else(|| "unknown".to_owned()),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct LwtRow {
    pub t: String,
    pub c: Map<String, Value>,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub insert: bool,
}

impl LwtRow {
    pub fn new(
        table: &str,
        key: impl IntoIterator<Item = (&'static str, Value)>,
        insert: bool,
    ) -> Self {
        Self {
            t: table.to_owned(),
            c: key
                .into_iter()
                .map(|(column, value)| (column.to_owned(), value))
                .collect(),
            insert,
        }
    }
}

pub fn bigint(value: i64) -> Value {
    json!({"$t": "i64", "v": value.to_string()})
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CaptureOutcome {
    Applied,
    Failed,
    Unknown,
    NotApplied,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum CaptureRecord {
    #[serde(rename = "l")]
    Lwt {
        producer: String,
        seq: u64,
        ts: i64,
        rows: Vec<LwtRow>,
    },
    #[serde(rename = "o")]
    Outcomes {
        producer: String,
        outcomes: Vec<(u64, CaptureOutcome)>,
    },
    #[serde(rename = "life")]
    Life {
        producer: String,
        state: String,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        last_seq: Option<u64>,
    },
}

pub fn classify_write_error(error: &anyhow::Error) -> CaptureOutcome {
    #[cfg(feature = "scylla")]
    if let Some(error) = error.downcast_ref::<scylla::errors::ExecutionError>() {
        return classify_execution_error(error);
    }
    let _ = error;
    CaptureOutcome::Unknown
}

#[cfg(feature = "scylla")]
fn classify_execution_error(error: &scylla::errors::ExecutionError) -> CaptureOutcome {
    use scylla::errors::{DbError, ExecutionError, RequestAttemptError};
    match error {
        ExecutionError::EmptyPlan | ExecutionError::ConnectionPoolError(_) => {
            CaptureOutcome::Failed
        }
        ExecutionError::LastAttemptError(RequestAttemptError::UnableToAllocStreamId) => {
            CaptureOutcome::Failed
        }
        ExecutionError::LastAttemptError(RequestAttemptError::DbError(
            DbError::Unavailable { .. }
            | DbError::Overloaded
            | DbError::IsBootstrapping
            | DbError::SyntaxError
            | DbError::Unauthorized
            | DbError::Invalid
            | DbError::ConfigError
            | DbError::Unprepared { .. },
            _,
        )) => CaptureOutcome::Failed,
        _ => CaptureOutcome::Unknown,
    }
}

#[derive(Clone, Copy, Debug)]
struct Limits {
    max_in_flight: usize,
    max_buffered_outcomes: usize,
    slot_wait: Duration,
}

const DEFAULT_LIMITS: Limits = Limits {
    max_in_flight: 4096,
    max_buffered_outcomes: 50_000,
    slot_wait: Duration::from_secs(2),
};

#[derive(Default)]
struct State {
    in_flight: usize,
    outcomes: Vec<(u64, CaptureOutcome)>,
    outcome_batches: u64,
    closing: bool,
    stopped: bool,
}

#[derive(Default)]
struct Shared {
    state: Mutex<State>,
    room: Notify,
    flush: Notify,
    publish_failures_total: AtomicU64,
    rejected_busy_total: AtomicU64,
    rejected_closed_total: AtomicU64,
}

impl Shared {
    fn lock(&self) -> std::sync::MutexGuard<'_, State> {
        self.state
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner())
    }
}

#[derive(Clone)]
enum Sink {
    JetStream(Box<jetstream::Context>),
    #[cfg(test)]
    Memory(Arc<tests::MemorySink>),
}

impl Sink {
    async fn publish_once(&self, subject: &str, msg_id: &str, body: &[u8]) -> anyhow::Result<()> {
        match self {
            Self::JetStream(context) => {
                let mut headers = HeaderMap::new();
                headers.insert("Nats-Msg-Id", msg_id);
                let attempt = async {
                    context
                        .publish_with_headers(subject.to_owned(), headers, body.to_vec().into())
                        .await?
                        .await
                        .map_err(anyhow::Error::from)
                };
                match tokio::time::timeout(PUBLISH_TIMEOUT, attempt).await {
                    Ok(result) => result.map(drop),
                    Err(_) => Err(anyhow::anyhow!("capture publish timed out")),
                }
            }
            #[cfg(test)]
            Self::Memory(sink) => sink.publish(subject, msg_id, body).await,
        }
    }
}

pub struct CassandraCapture {
    mode: CaptureMode,
    producer: String,
    subject_prefix: String,
    sink: Option<Sink>,
    limits: Limits,
    seq: AtomicU64,
    records_total: AtomicU64,
    shared: Arc<Shared>,
    flusher: Option<tokio::task::JoinHandle<()>>,
}

impl Drop for CassandraCapture {
    fn drop(&mut self) {
        if let Some(flusher) = &self.flusher {
            flusher.abort();
        }
    }
}

struct Slot<'a> {
    shared: &'a Shared,
    seq: u64,
    outcome: CaptureOutcome,
}

impl Drop for Slot<'_> {
    fn drop(&mut self) {
        let mut state = self.shared.lock();
        state.in_flight -= 1;
        state.outcomes.push((self.seq, self.outcome));
        drop(state);
        self.shared.flush.notify_one();
        self.shared.room.notify_waiters();
    }
}

fn now_micros() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|elapsed| i64::try_from(elapsed.as_micros()).unwrap_or(i64::MAX))
        .unwrap_or_default()
}

impl CassandraCapture {
    pub fn new(mode: CaptureMode, process: &str, producer: String) -> Self {
        Self {
            mode,
            producer,
            subject_prefix: format!("cassandra.capture.{process}"),
            sink: None,
            limits: DEFAULT_LIMITS,
            seq: AtomicU64::new(0),
            records_total: AtomicU64::new(0),
            shared: Arc::default(),
            flusher: None,
        }
    }

    fn with_sink(mut self, sink: Sink) -> Self {
        let flusher = OutcomeFlusher {
            shared: self.shared.clone(),
            sink: sink.clone(),
            subject: format!("{}.o", self.subject_prefix),
            producer: self.producer.clone(),
        };
        self.flusher = Some(tokio::spawn(flusher.run()));
        self.sink = Some(sink);
        self
    }

    pub async fn start(config: &CaptureConfig, process: &str) -> anyhow::Result<Self> {
        let producer = format!("{}:{}-{}", config.pod, std::process::id(), now_micros());
        let capture = Self::new(config.mode, process, producer);
        if config.mode != CaptureMode::On {
            return Ok(capture);
        }
        let url = config.nats_url.as_deref().unwrap_or_default();
        let mut options = async_nats::ConnectOptions::new().name("cassandra-capture");
        if let Some(token) = &config.nats_token {
            options = options.token(token.clone());
        }
        let client = options
            .connect(url)
            .await
            .context("connect to the Cassandra capture stream")?;
        let context = jetstream::new(client);
        context
            .get_stream(&config.stream)
            .await
            .with_context(|| format!("capture stream {} is missing", config.stream))?;
        let capture = capture.with_sink(Sink::JetStream(Box::new(context)));
        capture.publish_life("open", None).await?;
        tracing::info!(
            producer = capture.producer,
            "cassandra write capture started"
        );
        Ok(capture)
    }

    pub fn mode(&self) -> CaptureMode {
        self.mode
    }

    fn lwt_intent(&self, seq: u64, rows: Vec<LwtRow>) -> CaptureRecord {
        CaptureRecord::Lwt {
            producer: self.producer.clone(),
            seq,
            ts: now_micros(),
            rows,
        }
    }

    pub async fn conditional<T>(
        &self,
        rows: Vec<LwtRow>,
        send: impl Future<Output = anyhow::Result<T>>,
        applied_of: impl FnOnce(&T) -> Option<bool>,
    ) -> anyhow::Result<T> {
        if self.mode == CaptureMode::Off {
            return send.await;
        }
        let Some(sink) = &self.sink else {
            let seq = self.seq.fetch_add(1, Ordering::Relaxed) + 1;
            let _ = self.lwt_intent(seq, rows);
            self.records_total.fetch_add(1, Ordering::Relaxed);
            return send.await;
        };
        let mut slot = self.acquire().await?;
        let record = self.lwt_intent(slot.seq, rows);
        self.records_total.fetch_add(1, Ordering::Relaxed);
        self.publish(
            sink,
            "l",
            &format!("{}:{}:l", self.producer, slot.seq),
            &record,
        )
        .await?;
        slot.outcome = CaptureOutcome::Unknown;
        let result = send.await;
        slot.outcome = match &result {
            Ok(value) => match applied_of(value) {
                Some(true) => CaptureOutcome::Applied,
                Some(false) => CaptureOutcome::NotApplied,
                None => CaptureOutcome::Unknown,
            },
            Err(error) => classify_write_error(error),
        };
        result
    }

    async fn acquire(&self) -> anyhow::Result<Slot<'_>> {
        let deadline = tokio::time::Instant::now() + self.limits.slot_wait;
        loop {
            let room = self.shared.room.notified();
            tokio::pin!(room);
            room.as_mut().enable();
            {
                let mut state = self.shared.lock();
                if state.closing {
                    self.shared
                        .rejected_closed_total
                        .fetch_add(1, Ordering::Relaxed);
                    anyhow::bail!("cassandra write capture is closed");
                }
                if state.in_flight < self.limits.max_in_flight
                    && state.outcomes.len() < self.limits.max_buffered_outcomes
                {
                    state.in_flight += 1;
                    return Ok(Slot {
                        shared: &self.shared,
                        seq: self.seq.fetch_add(1, Ordering::Relaxed) + 1,
                        outcome: CaptureOutcome::Failed,
                    });
                }
            }
            if tokio::time::timeout_at(deadline, room).await.is_err() {
                self.shared
                    .rejected_busy_total
                    .fetch_add(1, Ordering::Relaxed);
                anyhow::bail!("cassandra write capture is busy");
            }
        }
    }

    pub async fn close(&self, timeout: Duration) {
        let Some(sink) = &self.sink else {
            return;
        };
        {
            let mut state = self.shared.lock();
            if state.closing {
                return;
            }
            state.closing = true;
        }
        let deadline = tokio::time::Instant::now() + timeout;
        loop {
            let (in_flight, outcomes) = {
                let state = self.shared.lock();
                (state.in_flight, state.outcomes.len())
            };
            if in_flight == 0 && outcomes == 0 {
                break;
            }
            if tokio::time::Instant::now() >= deadline {
                break;
            }
            self.shared.flush.notify_one();
            tokio::time::sleep(CLOSE_POLL).await;
        }
        let (in_flight, outcomes) = {
            let mut state = self.shared.lock();
            state.stopped = true;
            (state.in_flight, state.outcomes.len())
        };
        self.shared.flush.notify_one();
        if in_flight > 0 || outcomes > 0 {
            tracing::error!(
                in_flight,
                outcomes,
                "cassandra write capture closed with unpublished outcomes"
            );
            return;
        }
        let last_seq = self.seq.load(Ordering::Relaxed);
        let record = CaptureRecord::Life {
            producer: self.producer.clone(),
            state: "closed".to_owned(),
            last_seq: Some(last_seq),
        };
        if let Err(error) = self
            .publish(sink, "life", &format!("{}:closed", self.producer), &record)
            .await
        {
            tracing::error!(%error, "cassandra write capture could not publish its closed record");
        }
    }

    pub fn render_metrics(&self, output: &mut String) {
        let _ = render_counter(
            output,
            "fluxer_cassandra_capture_records_total",
            &self.records_total,
        );
        let _ = render_counter(
            output,
            "fluxer_cassandra_capture_publish_failures_total",
            &self.shared.publish_failures_total,
        );
        let _ = writeln!(
            output,
            "# TYPE fluxer_cassandra_capture_rejected_writes_total counter"
        );
        for (reason, counter) in [
            ("busy", &self.shared.rejected_busy_total),
            ("closed", &self.shared.rejected_closed_total),
        ] {
            let _ = writeln!(
                output,
                "fluxer_cassandra_capture_rejected_writes_total{{reason=\"{reason}\"}} {}",
                counter.load(Ordering::Relaxed)
            );
        }
    }

    async fn publish_life(&self, state: &str, last_seq: Option<u64>) -> anyhow::Result<()> {
        let Some(sink) = &self.sink else {
            return Ok(());
        };
        let record = CaptureRecord::Life {
            producer: self.producer.clone(),
            state: state.to_owned(),
            last_seq,
        };
        let msg_id = format!("{}:{state}", self.producer);
        self.publish(sink, "life", &msg_id, &record).await
    }

    async fn publish(
        &self,
        sink: &Sink,
        kind: &str,
        msg_id: &str,
        record: &CaptureRecord,
    ) -> anyhow::Result<()> {
        let subject = format!("{}.{kind}", self.subject_prefix);
        let body = serde_json::to_vec(record)?;
        let mut last_error = None;
        for _ in 0..2 {
            match sink.publish_once(&subject, msg_id, &body).await {
                Ok(()) => return Ok(()),
                Err(error) => last_error = Some(error),
            }
        }
        self.shared
            .publish_failures_total
            .fetch_add(1, Ordering::Relaxed);
        Err(last_error
            .unwrap_or_else(|| anyhow::anyhow!("capture publish failed"))
            .context("publish a Cassandra capture record"))
    }
}

struct OutcomeFlusher {
    shared: Arc<Shared>,
    sink: Sink,
    subject: String,
    producer: String,
}

impl OutcomeFlusher {
    async fn run(self) {
        loop {
            let flush = self.shared.flush.notified();
            tokio::pin!(flush);
            flush.as_mut().enable();
            let (queued, closing) = {
                let state = self.shared.lock();
                if state.stopped {
                    return;
                }
                (state.outcomes.len(), state.closing)
            };
            if queued == 0 {
                flush.await;
                continue;
            }
            if queued < OUTCOME_BATCH_SIZE && !closing {
                tokio::time::sleep(OUTCOME_FLUSH_DELAY).await;
            }
            self.flush().await;
        }
    }

    async fn flush(&self) {
        loop {
            let (batch, msg_id) = {
                let mut state = self.shared.lock();
                if state.stopped || state.outcomes.is_empty() {
                    return;
                }
                let batch = state.outcomes[..state.outcomes.len().min(OUTCOME_BATCH_SIZE)].to_vec();
                state.outcome_batches += 1;
                (
                    batch,
                    format!("{}:{}:o", self.producer, state.outcome_batches),
                )
            };
            let published = batch.len();
            let record = CaptureRecord::Outcomes {
                producer: self.producer.clone(),
                outcomes: batch,
            };
            let body = match serde_json::to_vec(&record) {
                Ok(body) => body,
                Err(error) => {
                    tracing::error!(%error, "cassandra write capture could not encode outcomes");
                    return;
                }
            };
            loop {
                match self.sink.publish_once(&self.subject, &msg_id, &body).await {
                    Ok(()) => {
                        self.shared.lock().outcomes.drain(..published);
                        break;
                    }
                    Err(_) => {
                        self.shared
                            .publish_failures_total
                            .fetch_add(1, Ordering::Relaxed);
                        if self.shared.lock().stopped {
                            return;
                        }
                        tokio::time::sleep(OUTCOME_RETRY_DELAY).await;
                    }
                }
            }
            self.shared.room.notify_waiters();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const FIXTURE: &str =
        include_str!("../../fluxer_api/src/api/database/capture/fixtures/records.json");

    fn fixture() -> Vec<Value> {
        serde_json::from_str(FIXTURE).expect("the shared capture fixture is JSON")
    }

    #[derive(Default)]
    pub(super) struct MemorySink {
        events: Mutex<Vec<String>>,
        published: Mutex<Vec<(String, String, Value)>>,
        failing: Mutex<Vec<&'static str>>,
        outcome_gate: Option<tokio::sync::Semaphore>,
    }

    impl MemorySink {
        pub(super) async fn publish(
            &self,
            subject: &str,
            msg_id: &str,
            body: &[u8],
        ) -> anyhow::Result<()> {
            if let (true, Some(gate)) = (subject.ends_with(".o"), &self.outcome_gate) {
                gate.acquire().await?.forget();
            }
            if self
                .failing
                .lock()
                .unwrap()
                .iter()
                .any(|suffix| subject.ends_with(suffix))
            {
                anyhow::bail!("capture stream down");
            }
            self.events.lock().unwrap().push(subject.to_owned());
            self.published.lock().unwrap().push((
                subject.to_owned(),
                msg_id.to_owned(),
                serde_json::from_slice(body)?,
            ));
            Ok(())
        }

        fn event(&self, event: &str) {
            self.events.lock().unwrap().push(event.to_owned());
        }

        fn outcomes(&self) -> Vec<Value> {
            self.published
                .lock()
                .unwrap()
                .iter()
                .filter(|(_, _, record)| record["kind"] == "o")
                .flat_map(|(_, _, record)| record["outcomes"].as_array().unwrap().clone())
                .collect()
        }

        fn last(&self) -> Value {
            self.published.lock().unwrap().last().unwrap().2.clone()
        }
    }

    fn capture_on(sink: &Arc<MemorySink>, limits: Limits) -> CassandraCapture {
        let mut capture =
            CassandraCapture::new(CaptureMode::On, "users", "users-0:boot".to_owned());
        capture.limits = limits;
        capture.with_sink(Sink::Memory(sink.clone()))
    }

    fn challenge() -> Vec<LwtRow> {
        vec![LwtRow::new(
            "app_attest_challenges",
            [("challenge", Value::from("abc"))],
            true,
        )]
    }

    #[test]
    fn decodes_and_re_encodes_every_shared_lwt_outcome_and_life_record() {
        let records = fixture()
            .into_iter()
            .filter(|record| matches!(record["kind"].as_str(), Some("l" | "o" | "life")))
            .collect::<Vec<_>>();
        assert!(records.iter().any(|record| record["kind"] == "o"));
        assert!(records.len() >= 5);
        for record in records {
            let parsed: CaptureRecord = serde_json::from_value(record.clone()).unwrap();
            assert_eq!(record, serde_json::to_value(parsed).unwrap());
        }
    }

    #[test]
    fn builds_the_shared_app_attest_intent() {
        let capture = CassandraCapture::new(
            CaptureMode::Measure,
            "users",
            "users-0:00000000-0000-0000-0000-000000000001".to_owned(),
        );
        let record = capture.lwt_intent(
            1,
            vec![LwtRow::new(
                "app_attest_keys",
                [
                    ("user_id", bigint(1_309_547_563_201_765_376)),
                    ("key_id", Value::from("a2V5")),
                ],
                false,
            )],
        );
        let mut built = serde_json::to_value(record).unwrap();
        built["ts"] = Value::from(1_763_750_000_000_000_i64);
        assert_eq!(Some(&built), fixture().last());
    }

    #[tokio::test]
    async fn publishes_the_intent_before_the_write_and_its_outcome_after() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_on(&sink, DEFAULT_LIMITS);
        let applied = capture
            .conditional(
                challenge(),
                async {
                    sink.event("send");
                    Ok(true)
                },
                |applied| Some(*applied),
            )
            .await
            .unwrap();
        assert!(applied);
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(
            *sink.events.lock().unwrap(),
            [
                "cassandra.capture.users.l",
                "send",
                "cassandra.capture.users.o",
                "cassandra.capture.users.life",
            ]
        );
        let published = sink.published.lock().unwrap().clone();
        let ids = published
            .iter()
            .map(|(_, id, _)| id.as_str())
            .collect::<Vec<_>>();
        assert_eq!(
            ids,
            [
                "users-0:boot:1:l",
                "users-0:boot:1:o",
                "users-0:boot:closed"
            ]
        );
        assert_eq!(
            published[1].2,
            json!({"kind": "o", "producer": "users-0:boot", "outcomes": [[1, "applied"]]})
        );
        assert_eq!(
            sink.last(),
            json!({"kind": "life", "producer": "users-0:boot", "state": "closed", "last_seq": 1})
        );
    }

    #[tokio::test]
    async fn records_whether_a_conditional_write_applied_and_rethrows_driver_errors() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_on(&sink, DEFAULT_LIMITS);
        for applied in [Some(true), Some(false), None] {
            capture
                .conditional(challenge(), async { Ok(applied) }, |applied| *applied)
                .await
                .unwrap();
        }
        let error = capture
            .conditional(
                challenge(),
                async { Err::<(), _>(anyhow::anyhow!("socket hang up")) },
                |_| Some(true),
            )
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "socket hang up");
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(
            sink.outcomes(),
            [
                json!([1, "applied"]),
                json!([2, "not_applied"]),
                json!([3, "unknown"]),
                json!([4, "unknown"]),
            ]
        );
    }

    #[tokio::test]
    async fn refuses_the_write_when_the_intent_cannot_be_stored_and_records_it_as_failed() {
        let sink = Arc::new(MemorySink::default());
        sink.failing.lock().unwrap().push(".l");
        let capture = capture_on(&sink, DEFAULT_LIMITS);
        let sent = AtomicU64::new(0);
        let result = capture
            .conditional(
                challenge(),
                async {
                    sent.fetch_add(1, Ordering::Relaxed);
                    Ok(())
                },
                |_| Some(true),
            )
            .await;
        capture.close(Duration::from_secs(1)).await;
        assert!(result.is_err());
        assert_eq!(sent.load(Ordering::Relaxed), 0);
        assert_eq!(
            capture
                .shared
                .publish_failures_total
                .load(Ordering::Relaxed),
            1
        );
        assert_eq!(sink.outcomes(), [json!([1, "failed"])]);
    }

    #[tokio::test]
    async fn records_a_write_cancelled_during_the_driver_call_as_unknown() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_on(&sink, DEFAULT_LIMITS);
        let write = capture.conditional(
            challenge(),
            std::future::pending::<anyhow::Result<()>>(),
            |_| Some(true),
        );
        assert!(
            tokio::time::timeout(Duration::from_millis(20), write)
                .await
                .is_err()
        );
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(sink.outcomes(), [json!([1, "unknown"])]);
    }

    #[tokio::test]
    async fn holds_new_writes_while_the_outcome_buffer_is_full_and_closes_once_it_drains() {
        let sink = Arc::new(MemorySink {
            outcome_gate: Some(tokio::sync::Semaphore::new(0)),
            ..MemorySink::default()
        });
        let capture = capture_on(
            &sink,
            Limits {
                max_in_flight: 8,
                max_buffered_outcomes: 1,
                slot_wait: Duration::from_millis(20),
            },
        );
        let write = || capture.conditional(challenge(), async { Ok(()) }, |_| Some(true));
        write().await.unwrap();
        assert!(write().await.is_err());
        tokio::join!(capture.close(Duration::from_secs(1)), async {
            sink.outcome_gate.as_ref().unwrap().add_permits(16);
        });
        assert_eq!(sink.outcomes(), [json!([1, "applied"])]);
        assert_eq!(sink.last()["state"], "closed");
        assert_eq!(sink.last()["last_seq"], 1);
        assert!(write().await.is_err());
        let mut metrics = String::new();
        capture.render_metrics(&mut metrics);
        assert!(
            metrics.contains("fluxer_cassandra_capture_rejected_writes_total{reason=\"busy\"} 1")
        );
        assert!(
            metrics.contains("fluxer_cassandra_capture_rejected_writes_total{reason=\"closed\"} 1")
        );
    }

    #[tokio::test]
    async fn a_failed_jetstream_publish_fails_the_write_before_it_is_sent() {
        let client = async_nats::ConnectOptions::new()
            .retry_on_initial_connect()
            .connect("nats://127.0.0.1:1")
            .await
            .unwrap();
        let capture = CassandraCapture::new(CaptureMode::On, "users", "users-0:boot".to_owned())
            .with_sink(Sink::JetStream(Box::new(jetstream::new(client))));
        let sent = AtomicU64::new(0);
        let result = capture
            .conditional(
                challenge(),
                async {
                    sent.fetch_add(1, Ordering::Relaxed);
                    Ok(())
                },
                |_| Some(true),
            )
            .await;
        assert!(result.is_err());
        assert_eq!(sent.load(Ordering::Relaxed), 0);
        assert!(
            capture
                .shared
                .publish_failures_total
                .load(Ordering::Relaxed)
                >= 1
        );
    }

    #[tokio::test]
    async fn mode_off_connects_to_nothing_and_lets_every_write_through() {
        let config =
            CaptureConfig::from_env(&EnvMap::from_pairs(std::iter::empty::<(String, String)>()))
                .unwrap();
        let capture = CassandraCapture::start(&config, "users").await.unwrap();
        assert!(capture.sink.is_none());
        assert!(capture.flusher.is_none());
        let applied = capture
            .conditional(challenge(), async { Ok(false) }, |_| {
                panic!("mode off never inspects the result")
            })
            .await
            .unwrap();
        assert!(!applied);
        assert_eq!(capture.records_total.load(Ordering::Relaxed), 0);
        assert_eq!(capture.seq.load(Ordering::Relaxed), 0);
        assert!(capture.shared.lock().outcomes.is_empty());
        capture.close(Duration::from_secs(1)).await;
    }

    #[tokio::test]
    async fn mode_measure_counts_records_and_publishes_no_outcome() {
        let capture =
            CassandraCapture::new(CaptureMode::Measure, "users", "users-0:boot".to_owned());
        capture
            .conditional(challenge(), async { Ok(()) }, |_| Some(true))
            .await
            .unwrap();
        assert_eq!(capture.records_total.load(Ordering::Relaxed), 1);
        assert!(capture.shared.lock().outcomes.is_empty());
    }

    #[cfg(feature = "scylla")]
    #[test]
    fn classifies_driver_errors_like_the_api() {
        use scylla::errors::{DbError, ExecutionError, RequestAttemptError, WriteType};
        use scylla::statement::Consistency;
        let db = |error: DbError| {
            ExecutionError::LastAttemptError(RequestAttemptError::DbError(error, String::new()))
        };
        let cases = [
            (
                "unavailable",
                db(DbError::Unavailable {
                    consistency: Consistency::Quorum,
                    required: 2,
                    alive: 1,
                }),
                CaptureOutcome::Failed,
            ),
            (
                "overloaded",
                db(DbError::Overloaded),
                CaptureOutcome::Failed,
            ),
            (
                "bootstrapping",
                db(DbError::IsBootstrapping),
                CaptureOutcome::Failed,
            ),
            ("invalid", db(DbError::Invalid), CaptureOutcome::Failed),
            ("syntax", db(DbError::SyntaxError), CaptureOutcome::Failed),
            (
                "unauthorized",
                db(DbError::Unauthorized),
                CaptureOutcome::Failed,
            ),
            ("config", db(DbError::ConfigError), CaptureOutcome::Failed),
            (
                "unprepared",
                db(DbError::Unprepared {
                    statement_id: Default::default(),
                }),
                CaptureOutcome::Failed,
            ),
            (
                "write timeout",
                db(DbError::WriteTimeout {
                    consistency: Consistency::Serial,
                    received: 0,
                    required: 1,
                    write_type: WriteType::Cas,
                }),
                CaptureOutcome::Unknown,
            ),
            (
                "server error",
                db(DbError::ServerError),
                CaptureOutcome::Unknown,
            ),
            (
                "client timeout",
                ExecutionError::RequestTimeout(Duration::from_secs(12)),
                CaptureOutcome::Unknown,
            ),
            (
                "busy connection",
                ExecutionError::LastAttemptError(RequestAttemptError::UnableToAllocStreamId),
                CaptureOutcome::Failed,
            ),
            ("no host", ExecutionError::EmptyPlan, CaptureOutcome::Failed),
            (
                "no connection",
                ExecutionError::ConnectionPoolError(
                    scylla::errors::ConnectionPoolError::Initializing,
                ),
                CaptureOutcome::Failed,
            ),
        ];
        for (name, error, expected) in cases {
            assert_eq!(
                classify_write_error(&anyhow::Error::from(error)),
                expected,
                "{name}"
            );
        }
        assert_eq!(
            classify_write_error(&anyhow::anyhow!("socket hang up")),
            CaptureOutcome::Unknown
        );
    }

    #[cfg(feature = "scylla")]
    #[tokio::test]
    async fn records_a_definite_driver_error_as_failed() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_on(&sink, DEFAULT_LIMITS);
        let overloaded = scylla::errors::ExecutionError::LastAttemptError(
            scylla::errors::RequestAttemptError::DbError(
                scylla::errors::DbError::Overloaded,
                String::new(),
            ),
        );
        let result = capture
            .conditional(
                challenge(),
                async { Err::<(), _>(anyhow::Error::from(overloaded)) },
                |_| Some(true),
            )
            .await;
        assert!(result.is_err());
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(sink.outcomes(), [json!([1, "failed"])]);
    }

    #[test]
    fn reads_the_mode_and_requires_a_stream_address_when_on() {
        let env = |pairs: &[(&str, &str)]| {
            EnvMap::from_pairs(
                pairs
                    .iter()
                    .map(|(k, v)| ((*k).to_owned(), (*v).to_owned())),
            )
        };
        assert_eq!(
            CaptureConfig::from_env(&env(&[])).unwrap().mode,
            CaptureMode::Off
        );
        assert_eq!(
            CaptureConfig::from_env(&env(&[("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "measure")]))
                .unwrap()
                .mode,
            CaptureMode::Measure
        );
        assert!(
            CaptureConfig::from_env(&env(&[("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "on")])).is_err()
        );
        assert!(
            CaptureConfig::from_env(&env(&[("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "maybe")]))
                .is_err()
        );
    }
}
