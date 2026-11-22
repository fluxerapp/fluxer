// SPDX-License-Identifier: AGPL-3.0-or-later

use anyhow::Context;
use async_nats::HeaderMap;
use async_nats::jetstream;
use fluxer_common::config::EnvMap;
use fluxer_common::metrics::render_counter;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value, json};
use std::collections::{BTreeMap, VecDeque};
use std::fmt::Write as _;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use tokio::sync::Notify;

const PUBLISH_TIMEOUT: Duration = Duration::from_secs(2);
const CLOSE_POLL: Duration = Duration::from_millis(10);
const BEHIND_STREAM: &str = "CASSANDRA_CDC";

pub const SYNCED_TABLES: &[&str] = &[
    "app_attest_challenges",
    "app_attest_key_lookup",
    "app_attest_keys",
];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CaptureMode {
    Off,
    Behind,
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
                Some("behind") => Self::Behind,
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
        if mode == CaptureMode::Behind && nats_url.is_none() {
            anyhow::bail!(
                "FLUXER_SVC_CASSANDRA_CAPTURE_NATS_URL is required when FLUXER_SVC_CASSANDRA_CAPTURE_MODE is behind"
            );
        }
        Ok(Self {
            mode,
            nats_url,
            nats_token: env.value("FLUXER_SVC_CASSANDRA_CAPTURE_NATS_TOKEN"),
            stream: env
                .value("FLUXER_SVC_CASSANDRA_CAPTURE_STREAM")
                .unwrap_or_else(|| BEHIND_STREAM.to_owned()),
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum KeyScope {
    Row,
    Partition,
    Prefix,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct CaptureKey {
    pub t: String,
    pub s: KeyScope,
    pub c: Map<String, Value>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct KeyRecord {
    pub producer: String,
    pub seq: u64,
    pub ts: i64,
    pub out: CaptureOutcome,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub lwt: bool,
    pub keys: Vec<CaptureKey>,
}

#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind")]
pub enum CaptureRecord {
    #[serde(rename = "k")]
    Keys(KeyRecord),
    #[serde(rename = "life")]
    Life(LifeRecord),
}

#[derive(Clone, Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct LifeRecord {
    pub producer: String,
    pub state: String,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub pod: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub last_seq: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub dropped: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub from_seq: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub to_seq: Option<u64>,
}

impl LifeRecord {
    fn new(producer: &str, state: &str) -> Self {
        Self {
            producer: producer.to_owned(),
            state: state.to_owned(),
            ..Self::default()
        }
    }
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

fn outcome_of<T>(
    result: &anyhow::Result<T>,
    applied_of: impl FnOnce(&T) -> Option<bool>,
) -> CaptureOutcome {
    match result {
        Ok(value) => match applied_of(value) {
            Some(true) => CaptureOutcome::Applied,
            Some(false) => CaptureOutcome::NotApplied,
            None => CaptureOutcome::Unknown,
        },
        Err(error) => classify_write_error(error),
    }
}

#[derive(Clone, Copy, Debug)]
struct Limits {
    max_queued_keys: usize,
    key_retry_delay: Duration,
    alive_interval: Duration,
}

const DEFAULT_LIMITS: Limits = Limits {
    max_queued_keys: 20_000,
    key_retry_delay: Duration::from_millis(250),
    alive_interval: Duration::from_secs(300),
};

#[derive(Default)]
struct State {
    closing: bool,
    stopped: bool,
    opened: bool,
    key_seq: u64,
    queued_keys: VecDeque<KeyRecord>,
    lossy: Option<(u64, u64)>,
    dropped: u64,
    dropped_by_table: BTreeMap<String, u64>,
}

impl State {
    fn count_dropped(&mut self, keys: &[CaptureKey]) {
        self.dropped += 1;
        let mut tables = keys.iter().map(|key| key.t.as_str()).collect::<Vec<_>>();
        tables.sort_unstable();
        tables.dedup();
        for table in tables {
            *self.dropped_by_table.entry(table.to_owned()).or_default() += 1;
        }
    }

    fn drop_oldest_key(&mut self) {
        let Some(record) = self.queued_keys.pop_front() else {
            return;
        };
        self.lossy = Some(match self.lossy {
            Some((from_seq, _)) => (from_seq, record.seq),
            None => (record.seq, record.seq),
        });
        self.count_dropped(&record.keys);
    }

    fn drained(&self) -> bool {
        self.opened && self.lossy.is_none() && self.queued_keys.is_empty()
    }
}

#[derive(Default)]
struct Shared {
    state: Mutex<State>,
    flush: Notify,
    publish_failures_total: AtomicU64,
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
    pod: String,
    producer: String,
    subject_prefix: String,
    sink: Option<Sink>,
    limits: Limits,
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

struct BehindWrite<'a> {
    capture: &'a CassandraCapture,
    ts: i64,
    rows: Vec<LwtRow>,
    outcome: CaptureOutcome,
}

impl Drop for BehindWrite<'_> {
    fn drop(&mut self) {
        self.capture
            .enqueue_keys(self.ts, self.outcome, std::mem::take(&mut self.rows));
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
            pod: String::new(),
            producer,
            subject_prefix: format!("cassandra.capture.{process}"),
            sink: None,
            limits: DEFAULT_LIMITS,
            records_total: AtomicU64::new(0),
            shared: Arc::default(),
            flusher: None,
        }
    }

    fn with_sink(mut self, sink: Sink) -> Self {
        let publisher = KeyPublisher {
            shared: self.shared.clone(),
            sink: sink.clone(),
            subject_prefix: self.subject_prefix.clone(),
            pod: self.pod.clone(),
            producer: self.producer.clone(),
            limits: self.limits,
        };
        self.flusher = Some(tokio::spawn(publisher.run()));
        self.sink = Some(sink);
        self
    }

    pub async fn start(config: &CaptureConfig, process: &str) -> anyhow::Result<Self> {
        let producer = format!("{}:{}-{}", config.pod, std::process::id(), now_micros());
        let mut capture = Self::new(config.mode, process, producer);
        capture.pod.clone_from(&config.pod);
        if config.mode != CaptureMode::Behind {
            return Ok(capture);
        }
        let url = config.nats_url.as_deref().unwrap_or_default();
        let mut options = async_nats::ConnectOptions::new()
            .name("cassandra-capture")
            .retry_on_initial_connect();
        if let Some(token) = &config.nats_token {
            options = options.token(token.clone());
        }
        let client = options
            .connect(url)
            .await
            .context("connect to the Cassandra capture stream")?;
        let context = jetstream::new(client);
        if !matches!(
            tokio::time::timeout(PUBLISH_TIMEOUT, context.get_stream(&config.stream)).await,
            Ok(Ok(_))
        ) {
            capture
                .shared
                .publish_failures_total
                .fetch_add(1, Ordering::Relaxed);
            tracing::warn!(
                stream = config.stream,
                "cassandra capture stream is missing, key records stay queued until it exists"
            );
        }
        let capture = capture.with_sink(Sink::JetStream(Box::new(context)));
        tracing::info!(
            producer = capture.producer,
            "cassandra write capture started behind the writes"
        );
        Ok(capture)
    }

    pub fn mode(&self) -> CaptureMode {
        self.mode
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
        let mut write = BehindWrite {
            capture: self,
            ts: now_micros(),
            rows,
            outcome: CaptureOutcome::Unknown,
        };
        let result = send.await;
        write.outcome = outcome_of(&result, applied_of);
        result
    }

    fn enqueue_keys(&self, ts: i64, out: CaptureOutcome, rows: Vec<LwtRow>) {
        if !matches!(out, CaptureOutcome::Applied | CaptureOutcome::Unknown) {
            return;
        }
        let keys = rows
            .into_iter()
            .filter(|row| SYNCED_TABLES.contains(&row.t.as_str()))
            .map(|row| CaptureKey {
                t: row.t,
                s: KeyScope::Row,
                c: row.c,
            })
            .collect::<Vec<_>>();
        if keys.is_empty() {
            return;
        }
        self.records_total.fetch_add(1, Ordering::Relaxed);
        let mut state = self.shared.lock();
        if state.stopped {
            state.count_dropped(&keys);
            return;
        }
        state.key_seq += 1;
        let record = KeyRecord {
            producer: self.producer.clone(),
            seq: state.key_seq,
            ts,
            out,
            lwt: true,
            keys,
        };
        state.queued_keys.push_back(record);
        while state.queued_keys.len() > self.limits.max_queued_keys {
            state.drop_oldest_key();
        }
        drop(state);
        self.shared.flush.notify_one();
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
        while !self.shared.lock().drained() && tokio::time::Instant::now() < deadline {
            self.shared.flush.notify_one();
            tokio::time::sleep(CLOSE_POLL).await;
        }
        let (last_seq, dropped, unpublished) = {
            let mut state = self.shared.lock();
            state.stopped = true;
            let unpublished = std::mem::take(&mut state.queued_keys);
            for record in &unpublished {
                state.count_dropped(&record.keys);
            }
            (state.key_seq, state.dropped, unpublished.len())
        };
        self.shared.flush.notify_one();
        if unpublished > 0 {
            tracing::error!(
                unpublished,
                "cassandra write capture closed with unpublished key records"
            );
        }
        let record = CaptureRecord::Life(LifeRecord {
            last_seq: Some(last_seq),
            dropped: Some(dropped),
            ..LifeRecord::new(&self.producer, "closed")
        });
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
            "# TYPE fluxer_cassandra_capture_dropped_total counter"
        );
        for (table, dropped) in &self.shared.lock().dropped_by_table {
            let _ = writeln!(
                output,
                "fluxer_cassandra_capture_dropped_total{{table=\"{table}\"}} {dropped}"
            );
        }
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

enum Published {
    Open,
    Lossy(u64),
    Key(u64),
    Alive,
}

struct KeyPublisher {
    shared: Arc<Shared>,
    sink: Sink,
    subject_prefix: String,
    pod: String,
    producer: String,
    limits: Limits,
}

impl KeyPublisher {
    async fn run(self) {
        let mut alive_at = tokio::time::Instant::now() + self.limits.alive_interval;
        let mut alives = 0_u64;
        loop {
            let wake = self.shared.flush.notified();
            tokio::pin!(wake);
            wake.as_mut().enable();
            let next = {
                let state = self.shared.lock();
                if state.stopped {
                    return;
                }
                self.next(&state, tokio::time::Instant::now() >= alive_at, alives + 1)
            };
            let Some((kind, msg_id, record, published)) = next else {
                tokio::select! {
                    () = &mut wake => {}
                    () = tokio::time::sleep_until(alive_at) => {}
                }
                continue;
            };
            let body = match serde_json::to_vec(&record) {
                Ok(body) => body,
                Err(error) => {
                    tracing::error!(%error, "cassandra write capture could not encode a record");
                    return;
                }
            };
            let subject = format!("{}.{kind}", self.subject_prefix);
            if self
                .sink
                .publish_once(&subject, &msg_id, &body)
                .await
                .is_err()
            {
                self.shared
                    .publish_failures_total
                    .fetch_add(1, Ordering::Relaxed);
                tokio::time::sleep(self.limits.key_retry_delay).await;
                continue;
            }
            let mut state = self.shared.lock();
            match published {
                Published::Open => state.opened = true,
                Published::Lossy(to_seq) => {
                    state.lossy = state
                        .lossy
                        .filter(|&(_, latest)| latest > to_seq)
                        .map(|(_, latest)| (to_seq + 1, latest));
                }
                Published::Key(seq) => {
                    if state.queued_keys.front().map(|record| record.seq) == Some(seq) {
                        state.queued_keys.pop_front();
                    }
                }
                Published::Alive => {
                    alives += 1;
                    alive_at = tokio::time::Instant::now() + self.limits.alive_interval;
                }
            }
        }
    }

    fn next(
        &self,
        state: &State,
        alive_due: bool,
        alive: u64,
    ) -> Option<(&'static str, String, CaptureRecord, Published)> {
        let producer = &self.producer;
        if !state.opened {
            return Some((
                "life",
                format!("{producer}:open"),
                CaptureRecord::Life(LifeRecord {
                    pod: Some(self.pod.clone()),
                    ..LifeRecord::new(producer, "open")
                }),
                Published::Open,
            ));
        }
        if let Some((from_seq, to_seq)) = state.lossy {
            let record = CaptureRecord::Life(LifeRecord {
                from_seq: Some(from_seq),
                to_seq: Some(to_seq),
                ..LifeRecord::new(producer, "lossy")
            });
            return Some((
                "life",
                format!("{producer}:lossy:{from_seq}-{to_seq}"),
                record,
                Published::Lossy(to_seq),
            ));
        }
        if let Some(record) = state.queued_keys.front() {
            return Some((
                "k",
                format!("{producer}:{}:k", record.seq),
                CaptureRecord::Keys(record.clone()),
                Published::Key(record.seq),
            ));
        }
        if alive_due {
            let record = CaptureRecord::Life(LifeRecord {
                last_seq: Some(state.key_seq),
                ..LifeRecord::new(producer, "alive")
            });
            return Some((
                "life",
                format!("{producer}:alive:{alive}"),
                record,
                Published::Alive,
            ));
        }
        None
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
    }

    impl MemorySink {
        pub(super) async fn publish(
            &self,
            subject: &str,
            msg_id: &str,
            body: &[u8],
        ) -> anyhow::Result<()> {
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

        fn last(&self) -> Value {
            self.published.lock().unwrap().last().unwrap().2.clone()
        }
    }

    const QUICK: Limits = Limits {
        key_retry_delay: Duration::from_millis(2),
        ..DEFAULT_LIMITS
    };

    fn capture_behind(sink: &Arc<MemorySink>, limits: Limits) -> CassandraCapture {
        let mut capture =
            CassandraCapture::new(CaptureMode::Behind, "users", "users-0:boot".to_owned());
        capture.pod = "users-0".to_owned();
        capture.limits = limits;
        capture.with_sink(Sink::Memory(sink.clone()))
    }

    fn app_attest_key() -> Vec<LwtRow> {
        vec![LwtRow::new(
            "app_attest_keys",
            [
                ("user_id", bigint(1_309_547_563_201_765_376)),
                ("key_id", Value::from("a2V5")),
            ],
            false,
        )]
    }

    fn ids(sink: &MemorySink) -> Vec<String> {
        sink.published
            .lock()
            .unwrap()
            .iter()
            .map(|(_, id, _)| id.clone())
            .collect()
    }

    fn record(sink: &MemorySink, id: &str) -> Value {
        let published = sink.published.lock().unwrap();
        let (_, _, record) = published
            .iter()
            .find(|(_, published_id, _)| published_id == id)
            .unwrap_or_else(|| panic!("{id} was not published"));
        let mut record = record.clone();
        if record["kind"] == "k" {
            record["ts"] = Value::from(0);
        }
        record
    }

    async fn published(sink: &MemorySink, id: &str) {
        tokio::time::timeout(Duration::from_secs(2), async {
            while !ids(sink).iter().any(|published| published == id) {
                tokio::time::sleep(Duration::from_millis(1)).await;
            }
        })
        .await
        .unwrap_or_else(|_| panic!("{id} was not published"));
    }

    fn challenge() -> Vec<LwtRow> {
        vec![LwtRow::new(
            "app_attest_challenges",
            [("challenge", Value::from("abc"))],
            true,
        )]
    }

    #[test]
    fn decodes_and_re_encodes_every_shared_key_and_life_record() {
        let records = fixture()
            .into_iter()
            .filter(|record| matches!(record["kind"].as_str(), Some("k" | "life")))
            .collect::<Vec<_>>();
        for kind in ["k", "life"] {
            assert!(
                records.iter().any(|record| record["kind"] == kind),
                "{kind}"
            );
        }
        for state in ["open", "alive", "lossy", "closed"] {
            assert!(
                records
                    .iter()
                    .any(|record| record["kind"] == "life" && record["state"] == state),
                "{state}"
            );
        }
        for record in records {
            let parsed: CaptureRecord = serde_json::from_value(record.clone()).unwrap();
            assert_eq!(record, serde_json::to_value(parsed).unwrap());
        }
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
        assert!(capture.shared.lock().queued_keys.is_empty());
        capture.close(Duration::from_secs(1)).await;
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
    async fn behind_skips_a_write_the_driver_definitely_refused() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_behind(&sink, QUICK);
        let overloaded = scylla::errors::ExecutionError::LastAttemptError(
            scylla::errors::RequestAttemptError::DbError(
                scylla::errors::DbError::Overloaded,
                String::new(),
            ),
        );
        let result = capture
            .conditional(
                app_attest_key(),
                async { Err::<(), _>(anyhow::Error::from(overloaded)) },
                |_| Some(true),
            )
            .await;
        assert!(result.is_err());
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(ids(&sink), ["users-0:boot:open", "users-0:boot:closed"]);
    }

    #[tokio::test]
    async fn behind_publishes_a_key_record_after_each_write_that_applied_or_may_have() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_behind(&sink, QUICK);
        capture
            .conditional(
                app_attest_key(),
                async {
                    sink.event("send");
                    Ok(Some(true))
                },
                |applied| *applied,
            )
            .await
            .unwrap();
        for applied in [Some(false), None] {
            capture
                .conditional(app_attest_key(), async { Ok(applied) }, |applied| *applied)
                .await
                .unwrap();
        }
        let error = capture
            .conditional(
                app_attest_key(),
                async { Err::<Option<bool>, _>(anyhow::anyhow!("socket hang up")) },
                |applied| *applied,
            )
            .await
            .unwrap_err();
        assert_eq!(error.to_string(), "socket hang up");
        capture.close(Duration::from_secs(1)).await;
        let events = sink.events.lock().unwrap().clone();
        assert_eq!(events[0], "send");
        assert_eq!(
            ids(&sink),
            [
                "users-0:boot:open",
                "users-0:boot:1:k",
                "users-0:boot:2:k",
                "users-0:boot:3:k",
                "users-0:boot:closed",
            ]
        );
        assert_eq!(
            record(&sink, "users-0:boot:open"),
            json!({"kind": "life", "producer": "users-0:boot", "state": "open", "pod": "users-0"})
        );
        let keys = json!([{
            "t": "app_attest_keys",
            "s": "row",
            "c": {"user_id": {"$t": "i64", "v": "1309547563201765376"}, "key_id": "a2V5"},
        }]);
        for (id, seq, out) in [
            ("users-0:boot:1:k", 1, "applied"),
            ("users-0:boot:2:k", 2, "unknown"),
            ("users-0:boot:3:k", 3, "unknown"),
        ] {
            assert_eq!(
                record(&sink, id),
                json!({
                    "kind": "k",
                    "producer": "users-0:boot",
                    "seq": seq,
                    "ts": 0,
                    "out": out,
                    "lwt": true,
                    "keys": keys,
                })
            );
        }
        assert_eq!(
            sink.last(),
            json!({"kind": "life", "producer": "users-0:boot", "state": "closed", "last_seq": 3, "dropped": 0})
        );
    }

    #[tokio::test]
    async fn behind_spends_no_seq_on_failed_lost_or_unsynced_writes() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_behind(&sink, QUICK);
        capture.enqueue_keys(1, CaptureOutcome::Failed, app_attest_key());
        capture.enqueue_keys(1, CaptureOutcome::NotApplied, app_attest_key());
        capture.enqueue_keys(
            1,
            CaptureOutcome::Applied,
            vec![LwtRow::new("messages", [("channel_id", bigint(7))], false)],
        );
        capture.enqueue_keys(1, CaptureOutcome::Applied, Vec::new());
        capture.enqueue_keys(1, CaptureOutcome::Applied, app_attest_key());
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(
            ids(&sink),
            [
                "users-0:boot:open",
                "users-0:boot:1:k",
                "users-0:boot:closed"
            ]
        );
        assert_eq!(sink.last()["last_seq"], 1);
        assert_eq!(capture.records_total.load(Ordering::Relaxed), 1);
    }

    #[tokio::test]
    async fn behind_keeps_writing_while_the_stream_is_down_and_reports_what_it_dropped() {
        let sink = Arc::new(MemorySink::default());
        sink.failing.lock().unwrap().push("");
        let capture = capture_behind(
            &sink,
            Limits {
                max_queued_keys: 2,
                ..QUICK
            },
        );
        for _ in 0..5 {
            capture
                .conditional(challenge(), async { Ok(true) }, |applied| Some(*applied))
                .await
                .unwrap();
        }
        tokio::time::sleep(Duration::from_millis(10)).await;
        let mut metrics = String::new();
        capture.render_metrics(&mut metrics);
        assert!(
            metrics.contains(
                "fluxer_cassandra_capture_dropped_total{table=\"app_attest_challenges\"} 3"
            )
        );
        assert!(
            capture
                .shared
                .publish_failures_total
                .load(Ordering::Relaxed)
                >= 1
        );
        sink.failing.lock().unwrap().clear();
        published(&sink, "users-0:boot:5:k").await;
        assert_eq!(
            ids(&sink),
            [
                "users-0:boot:open",
                "users-0:boot:lossy:1-3",
                "users-0:boot:4:k",
                "users-0:boot:5:k",
            ]
        );
        assert_eq!(
            record(&sink, "users-0:boot:lossy:1-3"),
            json!({"kind": "life", "producer": "users-0:boot", "state": "lossy", "from_seq": 1, "to_seq": 3})
        );
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(
            sink.last(),
            json!({"kind": "life", "producer": "users-0:boot", "state": "closed", "last_seq": 5, "dropped": 3})
        );
    }

    #[tokio::test]
    async fn behind_counts_what_it_could_not_publish_before_the_close_deadline() {
        let sink = Arc::new(MemorySink::default());
        sink.failing.lock().unwrap().push(".k");
        let capture = capture_behind(&sink, QUICK);
        for _ in 0..2 {
            capture
                .conditional(challenge(), async { Ok(true) }, |applied| Some(*applied))
                .await
                .unwrap();
        }
        capture.close(Duration::from_millis(30)).await;
        assert_eq!(
            sink.last(),
            json!({"kind": "life", "producer": "users-0:boot", "state": "closed", "last_seq": 2, "dropped": 2})
        );
        assert!(
            capture
                .conditional(challenge(), async { Ok(true) }, |applied| Some(*applied))
                .await
                .unwrap()
        );
        let mut metrics = String::new();
        capture.render_metrics(&mut metrics);
        assert!(
            metrics.contains(
                "fluxer_cassandra_capture_dropped_total{table=\"app_attest_challenges\"} 3"
            )
        );
    }

    #[tokio::test]
    async fn behind_says_it_is_alive_with_its_last_seq() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_behind(
            &sink,
            Limits {
                alive_interval: Duration::from_millis(5),
                ..QUICK
            },
        );
        capture.enqueue_keys(1, CaptureOutcome::Applied, app_attest_key());
        published(&sink, "users-0:boot:alive:1").await;
        assert_eq!(
            record(&sink, "users-0:boot:alive:1"),
            json!({"kind": "life", "producer": "users-0:boot", "state": "alive", "last_seq": 1})
        );
        capture.close(Duration::from_secs(1)).await;
    }

    #[tokio::test]
    async fn behind_records_a_write_cancelled_during_the_driver_call_as_unknown() {
        let sink = Arc::new(MemorySink::default());
        let capture = capture_behind(&sink, QUICK);
        let write = capture.conditional(
            app_attest_key(),
            std::future::pending::<anyhow::Result<()>>(),
            |_| Some(true),
        );
        assert!(
            tokio::time::timeout(Duration::from_millis(20), write)
                .await
                .is_err()
        );
        capture.close(Duration::from_secs(1)).await;
        assert_eq!(record(&sink, "users-0:boot:1:k")["out"], "unknown");
    }

    #[tokio::test]
    async fn behind_starts_while_the_stream_cannot_be_found() {
        let config = CaptureConfig {
            mode: CaptureMode::Behind,
            nats_url: Some("nats://127.0.0.1:1".to_owned()),
            nats_token: None,
            stream: BEHIND_STREAM.to_owned(),
            pod: "users-0".to_owned(),
        };
        let capture = CassandraCapture::start(&config, "users").await.unwrap();
        assert!(capture.producer.starts_with("users-0:"));
        assert_eq!(capture.pod, "users-0");
        assert_eq!(
            capture
                .shared
                .publish_failures_total
                .load(Ordering::Relaxed),
            1
        );
        assert!(
            capture
                .conditional(challenge(), async { Ok(true) }, |applied| Some(*applied))
                .await
                .unwrap()
        );
    }

    #[test]
    fn every_synced_table_is_a_clickhouse_table_in_the_target_schema() {
        let schema: Value =
            serde_json::from_str(include_str!("../../tools/dev/cassandra_target_schema.json"))
                .unwrap();
        let cdc_tables = include_str!("../../fluxer_api/src/api/database/capture/CdcRecords.ts");
        let cdc_tables = &cdc_tables
            [cdc_tables.find("CDC_TABLES").unwrap()..cdc_tables.find("CDC_SKIPPED").unwrap()];
        for table in SYNCED_TABLES {
            assert!(
                schema["tables"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|candidate| candidate["name"] == *table),
                "{table}"
            );
            assert!(cdc_tables.contains(&format!("'{table}'")), "{table}");
        }
    }

    #[test]
    fn reads_the_mode_and_requires_a_stream_address_when_behind() {
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
        for retired in ["measure", "on"] {
            assert!(
                CaptureConfig::from_env(&env(&[
                    ("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", retired),
                    ("FLUXER_SVC_CASSANDRA_CAPTURE_NATS_URL", "nats://nats:4222"),
                ]))
                .is_err(),
                "{retired}"
            );
        }
        assert!(
            CaptureConfig::from_env(&env(&[("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "maybe")]))
                .is_err()
        );
        assert!(
            CaptureConfig::from_env(&env(&[("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "behind")]))
                .is_err()
        );
        let url = ("FLUXER_SVC_CASSANDRA_CAPTURE_NATS_URL", "nats://nats:4222");
        let behind = CaptureConfig::from_env(&env(&[
            ("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "behind"),
            url,
        ]))
        .unwrap();
        assert_eq!(behind.mode, CaptureMode::Behind);
        assert_eq!(behind.stream, "CASSANDRA_CDC");
        assert_eq!(
            CaptureConfig::from_env(&env(&[
                ("FLUXER_SVC_CASSANDRA_CAPTURE_MODE", "behind"),
                url,
                ("FLUXER_SVC_CASSANDRA_CAPTURE_STREAM", "CDC_PROOF"),
            ]))
            .unwrap()
            .stream,
            "CDC_PROOF"
        );
    }
}
