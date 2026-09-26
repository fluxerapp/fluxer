// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::dedupe::{Claim, DoneJobs, JobKey, Seen};
use crate::job::{
    self, ClearJob, JobError, MessageJob, QUEUE_GROUP, RingJob, SUBJECT_CLEAR, SUBJECT_MESSAGE,
    SUBJECT_RING,
};
use crate::metrics::{JobKind, JobRejection, Provider, elapsed_ms};
use crate::payload;
use crate::providers::{self, SendOutcome};
use crate::retry::{self, RETRY_DEADLINE};
use crate::rpc::RpcError;
use crate::server::AppState;
use crate::subscription::{Platform, Subscription};
use crate::unix_millis;
use fluxer_svc::metrics::now_ms;
use fluxer_svc::transport::{Transport, TransportMessage, TransportSubscriber};
use futures::future::join_all;
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::{Semaphore, TryAcquireError, watch};
use tokio::task::JoinSet;
use tracing::{error, info, warn};

const JOB_REPLY_DEADLINE: Duration = Duration::from_secs(90);
const RING_JOB_DEADLINE: Duration = Duration::from_secs(10);

const UNKNOWN_PROVIDER: &str = "unknown";
const OVERLOADED: &str = "overloaded";
const INVALID_JOB: &str = "invalid_job";
const RPC_UNAVAILABLE: &str = "rpc_unavailable";
const DEADLINE_EXCEEDED: &str = "deadline_exceeded";
const RUNNING: &str = "in_flight";

enum Job {
    Message(Box<MessageJob>),
    Clear(ClearJob),
    Ring(RingJob),
}

impl Job {
    fn kind(&self) -> JobKind {
        match self {
            Self::Message(_) => JobKind::Message,
            Self::Clear(_) => JobKind::Clear,
            Self::Ring(_) => JobKind::Ring,
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Audience {
    Standard,
    Ring,
}

impl Audience {
    fn admits(self, subscription: &Subscription) -> bool {
        match self {
            Self::Standard => subscription.platform() != Some(Platform::IosApnsVoip),
            Self::Ring => Self::rings(subscription),
        }
    }

    fn rings(subscription: &Subscription) -> bool {
        match subscription.platform() {
            Some(Platform::IosApnsVoip) => true,
            Some(Platform::AndroidFcm) => subscription.is_web_push_registration(),
            _ => false,
        }
    }
}

struct Claimed {
    job: Job,
    claims: Vec<Claim>,
    recipients_running: bool,
}

fn claim_recipients(done: &DoneJobs, job: Job) -> Claimed {
    match job {
        Job::Message(mut job) => {
            let mut claims = Vec::new();
            let mut recipients = Vec::new();
            let mut recipients_running = false;
            for user_id in std::mem::take(&mut job.user_ids) {
                match done.claim(JobKey::of_message(&job, &user_id)) {
                    Seen::New(claim) => {
                        claims.push(claim);
                        recipients.push(user_id);
                    }
                    Seen::Done => {}
                    Seen::Running => recipients_running = true,
                }
            }
            job.user_ids = recipients;
            Claimed {
                job: Job::Message(job),
                claims,
                recipients_running,
            }
        }
        Job::Clear(job) => {
            let (claims, recipients_running) = claim_one(done, JobKey::of_clear(&job));
            Claimed {
                job: Job::Clear(job),
                claims,
                recipients_running,
            }
        }
        Job::Ring(job) => {
            let (claims, recipients_running) = claim_one(done, JobKey::of_ring(&job));
            Claimed {
                job: Job::Ring(job),
                claims,
                recipients_running,
            }
        }
    }
}

fn claim_one(done: &DoneJobs, key: JobKey) -> (Vec<Claim>, bool) {
    match done.claim(key) {
        Seen::New(claim) => (vec![claim], false),
        Seen::Done => (Vec::new(), false),
        Seen::Running => (Vec::new(), true),
    }
}

enum Answer {
    Done,
    NotDone(&'static str),
}

impl Answer {
    fn body(&self) -> String {
        match self {
            Self::Done => r#"{"ok":true}"#.to_owned(),
            Self::NotDone(error) => format!(r#"{{"ok":false,"error":"{error}"}}"#),
        }
    }
}

struct Summary {
    subscriptions: u64,
    accepted: u64,
    deleted: u64,
}

struct Delivered {
    accepted: bool,
    deletion: Option<(String, String)>,
}

pub async fn run_job_subscribers<T: Transport>(transport: T, state: Arc<AppState>) {
    let admission = Arc::new(Semaphore::new(state.cfg.queue_capacity));
    let sends = Arc::new(Semaphore::new(state.cfg.send_concurrency));
    let done = DoneJobs::default();
    tokio::join!(
        run_subject(
            transport.clone(),
            Arc::clone(&state),
            Arc::clone(&admission),
            Arc::clone(&sends),
            done.clone(),
            JobKind::Message,
        ),
        run_subject(
            transport.clone(),
            Arc::clone(&state),
            Arc::clone(&admission),
            Arc::clone(&sends),
            done.clone(),
            JobKind::Clear,
        ),
        run_subject(transport, state, admission, sends, done, JobKind::Ring),
    );
}

async fn run_subject<T: Transport>(
    transport: T,
    state: Arc<AppState>,
    admission: Arc<Semaphore>,
    sends: Arc<Semaphore>,
    done: DoneJobs,
    kind: JobKind,
) {
    let subject = subject_of(kind);
    let capacity = state.cfg.queue_capacity;
    let mut draining = state.draining.subscribe();
    let mut running = JoinSet::new();
    'serving: while !is_draining(&draining) {
        let mut subscriber = match transport.subscribe_queue(subject, QUEUE_GROUP).await {
            Ok(subscriber) => subscriber,
            Err(error) => {
                warn!(
                    error = %error,
                    subject,
                    "push job subscribe failed"
                );
                tokio::select! {
                    () = drained(&mut draining) => {}
                    () = transport.wait_for_reconnect() => {}
                }
                continue;
            }
        };
        info!(
            subject,
            queue_group = QUEUE_GROUP,
            capacity,
            send_concurrency = state.cfg.send_concurrency,
            "listening for push jobs"
        );

        loop {
            let message = tokio::select! {
                biased;
                () = drained(&mut draining) => break 'serving,
                finished = running.join_next(), if !running.is_empty() => {
                    reap(finished.expect("a non-empty push job set yields a result"));
                    record_depth(&state, &admission, capacity);
                    continue;
                }
                message = subscriber.next() => {
                    let Some(message) = message else {
                        warn!(subject, "push job subscription ended, will re-subscribe");
                        break;
                    };
                    message
                }
            };

            while let Some(finished) = running.try_join_next() {
                reap(finished);
            }
            state.metrics.record_job_received(kind);
            let reply_to = message.reply_subject().map(str::to_owned);

            let job = match decode(kind, message.payload()) {
                Ok(job) => job,
                Err(error) => {
                    state.metrics.record_job_rejected(JobRejection::Decode);
                    warn!(error = %error, subject, "push job rejected");
                    answer(&transport, reply_to, Answer::NotDone(INVALID_JOB)).await;
                    continue;
                }
            };
            let permit = match Arc::clone(&admission).try_acquire_owned() {
                Ok(permit) => permit,
                Err(TryAcquireError::NoPermits) => {
                    state.metrics.record_job_rejected(JobRejection::QueueFull);
                    warn!(subject, capacity, "push job refused, no capacity left");
                    answer(&transport, reply_to, Answer::NotDone(OVERLOADED)).await;
                    continue;
                }
                Err(TryAcquireError::Closed) => return,
            };
            let Claimed {
                job,
                claims,
                recipients_running,
            } = claim_recipients(&done, job);
            if claims.is_empty() {
                let settled = if recipients_running {
                    Answer::NotDone(RUNNING)
                } else {
                    Answer::Done
                };
                answer(&transport, reply_to, settled).await;
                continue;
            }
            record_depth(&state, &admission, capacity);

            let job_state = Arc::clone(&state);
            let job_sends = Arc::clone(&sends);
            let job_transport = transport.clone();
            running.spawn(async move {
                let _permit = permit;
                let result = run_job(&job_state, &job_sends, job).await;
                if matches!(result, Answer::Done) {
                    for claim in claims {
                        claim.done();
                    }
                }
                answer(&job_transport, reply_to, result).await;
            });
        }
    }

    info!(
        subject,
        running = running.len(),
        "push job subscription stopped"
    );
    while let Some(finished) = running.join_next().await {
        reap(finished);
        record_depth(&state, &admission, capacity);
    }
}

fn is_draining(draining: &watch::Receiver<bool>) -> bool {
    *draining.borrow()
}

async fn drained(draining: &mut watch::Receiver<bool>) {
    let _ = draining.wait_for(|draining| *draining).await;
}

async fn run_job(state: &AppState, sends: &Semaphore, job: Job) -> Answer {
    let kind = job.kind();
    let work = async {
        match job {
            Job::Message(job) => run_message_job(state, sends, *job).await,
            Job::Clear(job) => run_clear_job(state, sends, job).await,
            Job::Ring(job) => run_ring_job(state, sends, job).await,
        }
    };
    match tokio::time::timeout(JOB_REPLY_DEADLINE, work).await {
        Ok(Ok(())) => Answer::Done,
        Ok(Err(error)) => {
            error!(kind = kind.label(), error = %error, "push job failed");
            Answer::NotDone(RPC_UNAVAILABLE)
        }
        Err(_) => {
            error!(
                kind = kind.label(),
                deadline_ms = JOB_REPLY_DEADLINE.as_millis() as u64,
                "push job ran past its reply deadline"
            );
            Answer::NotDone(DEADLINE_EXCEEDED)
        }
    }
}

async fn answer<T: Transport>(transport: &T, reply_to: Option<String>, answer: Answer) {
    let Some(reply_to) = reply_to else {
        return;
    };
    if let Err(error) = transport.publish(&reply_to, answer.body().as_bytes()).await {
        warn!(error = %error, "push job answer failed");
    }
}

async fn run_message_job(
    state: &AppState,
    sends: &Semaphore,
    job: MessageJob,
) -> anyhow::Result<()> {
    let started_ms = now_ms();
    let deadline = Instant::now() + RETRY_DEADLINE;
    let (badges, subscriptions) = lookup(deadline, || async {
        tokio::try_join!(
            state.rpc.badge_counts(&job.user_ids),
            state.rpc.push_subscriptions(&job.user_ids)
        )
    })
    .await?;
    state.metrics.record_recipients(job.user_ids.len() as u64);

    let envelopes = job
        .user_ids
        .iter()
        .filter(|user_id| subscribed(&subscriptions, user_id))
        .map(|user_id| {
            (
                user_id.as_str(),
                payload::web_push_message(&job, user_id, badge_of(&badges, user_id)),
            )
        })
        .collect();
    let summary = deliver(
        state,
        sends,
        &subscriptions,
        envelopes,
        deadline,
        Audience::Standard,
    )
    .await;
    let duration_ms = elapsed_ms(started_ms);

    info!(
        kind = JobKind::Message.label(),
        guild_id = %job.guild_id,
        channel_id = %job.channel_id,
        message_id = %job.message_id,
        config_version = job.config_version,
        recipients = job.user_ids.len(),
        subscriptions = summary.subscriptions,
        accepted = summary.accepted,
        deleted = summary.deleted,
        duration_ms,
        "push job"
    );
    state
        .metrics
        .record_job_completed(JobKind::Message, duration_ms);
    Ok(())
}

async fn run_clear_job(state: &AppState, sends: &Semaphore, job: ClearJob) -> anyhow::Result<()> {
    let started_ms = now_ms();
    let deadline = Instant::now() + RETRY_DEADLINE;
    let user_ids = std::slice::from_ref(&job.user_id);
    let (badges, subscriptions) = lookup(deadline, || async {
        tokio::try_join!(
            state.rpc.badge_counts(user_ids),
            state.rpc.push_subscriptions(user_ids)
        )
    })
    .await?;
    state.metrics.record_recipients(1);

    let envelope = payload::web_push_clear(&job, badge_of(&badges, &job.user_id));
    let envelopes = vec![(job.user_id.as_str(), envelope)];
    let summary = deliver(
        state,
        sends,
        &subscriptions,
        envelopes,
        deadline,
        Audience::Standard,
    )
    .await;
    let duration_ms = elapsed_ms(started_ms);

    info!(
        kind = JobKind::Clear.label(),
        user_id = %job.user_id,
        channel_id = %job.channel_id,
        message_id = %job.message_id,
        config_version = job.config_version,
        recipients = 1,
        subscriptions = summary.subscriptions,
        accepted = summary.accepted,
        deleted = summary.deleted,
        duration_ms,
        "push job"
    );
    state
        .metrics
        .record_job_completed(JobKind::Clear, duration_ms);
    Ok(())
}

async fn run_ring_job(state: &AppState, sends: &Semaphore, job: RingJob) -> anyhow::Result<()> {
    let started_ms = now_ms();
    if job.expires_at_ms <= unix_millis() {
        state.metrics.record_ring_suppressed();
        info!(
            kind = JobKind::Ring.label(),
            user_id = %job.user_id,
            channel_id = %job.channel_id,
            message_id = %job.message_id,
            config_version = job.config_version,
            expires_at_ms = job.expires_at_ms,
            "push ring dropped past its ring window"
        );
        state
            .metrics
            .record_job_completed(JobKind::Ring, elapsed_ms(started_ms));
        return Ok(());
    }

    let deadline = Instant::now() + RING_JOB_DEADLINE;
    let user_ids = std::slice::from_ref(&job.user_id);
    let subscriptions = lookup(deadline, || state.rpc.push_subscriptions(user_ids)).await?;
    state.metrics.record_recipients(1);

    let envelopes = vec![(job.user_id.as_str(), payload::web_push_call_ring(&job))];
    let summary = deliver(
        state,
        sends,
        &subscriptions,
        envelopes,
        deadline,
        Audience::Ring,
    )
    .await;
    let duration_ms = elapsed_ms(started_ms);

    info!(
        kind = JobKind::Ring.label(),
        user_id = %job.user_id,
        channel_id = %job.channel_id,
        message_id = %job.message_id,
        config_version = job.config_version,
        recipients = 1,
        subscriptions = summary.subscriptions,
        accepted = summary.accepted,
        deleted = summary.deleted,
        duration_ms,
        "push job"
    );
    state
        .metrics
        .record_job_completed(JobKind::Ring, duration_ms);
    Ok(())
}

async fn lookup<T, F, Fut>(deadline: Instant, mut call: F) -> Result<T, RpcError>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<T, RpcError>>,
{
    let mut attempt = 0;
    loop {
        let error = match call().await {
            Ok(found) => return Ok(found),
            Err(error) if error.is_retryable() => error,
            Err(error) => return Err(error),
        };
        let Some(at) = retry::next_attempt(attempt, Instant::now(), deadline) else {
            return Err(error);
        };
        warn!(attempt, error = %error, "push job lookup failed, retrying");
        tokio::time::sleep_until(at.into()).await;
        attempt += 1;
    }
}

async fn deliver(
    state: &AppState,
    sends: &Semaphore,
    subscriptions: &HashMap<String, Vec<Subscription>>,
    envelopes: Vec<(&str, Value)>,
    deadline: Instant,
    audience: Audience,
) -> Summary {
    let mut pending = Vec::new();
    for (user_id, envelope) in envelopes {
        let Some(subscriptions) = subscriptions.get(user_id) else {
            continue;
        };
        let envelope = Arc::new(envelope);
        for subscription in subscriptions
            .iter()
            .filter(|subscription| audience.admits(subscription))
        {
            pending.push(send_one(
                state,
                sends,
                user_id,
                subscription,
                Arc::clone(&envelope),
                deadline,
            ));
        }
    }

    let mut summary = Summary {
        subscriptions: pending.len() as u64,
        accepted: 0,
        deleted: 0,
    };
    state.metrics.record_subscriptions(summary.subscriptions);

    let mut deletions = Vec::new();
    for delivered in join_all(pending).await {
        if delivered.accepted {
            summary.accepted += 1;
        }
        if let Some(deletion) = delivered.deletion {
            deletions.push(deletion);
        }
    }
    summary.deleted = deletions.len() as u64;

    if !deletions.is_empty()
        && let Err(error) = state.rpc.delete_push_subscriptions(&deletions).await
    {
        warn!(
            error = %error,
            count = deletions.len(),
            "push subscription cleanup failed"
        );
    }
    summary
}

async fn send_one(
    state: &AppState,
    sends: &Semaphore,
    user_id: &str,
    subscription: &Subscription,
    envelope: Arc<Value>,
    deadline: Instant,
) -> Delivered {
    let provider = subscription.platform().map(providers::provider_of);
    let mut attempt = 0;
    let outcome = loop {
        let outcome = {
            let _permit = sends
                .acquire()
                .await
                .expect("the push send semaphore is never closed");
            providers::send(state, subscription, &envelope).await
        };
        if !matches!(outcome, SendOutcome::Transient { .. }) {
            break outcome;
        }
        let now = Instant::now();
        let Some(at) = retry::next_attempt(attempt, now, deadline) else {
            break outcome;
        };
        warn!(
            provider = provider.map_or(UNKNOWN_PROVIDER, Provider::label),
            subscription_id = %subscription.subscription_id,
            reason = outcome.reason(),
            attempt,
            "push delivery retrying"
        );
        tokio::time::sleep_until(at.into()).await;
        attempt += 1;
    };

    if !matches!(outcome, SendOutcome::Accepted) {
        error!(
            provider = provider.map_or(UNKNOWN_PROVIDER, Provider::label),
            subscription_id = %subscription.subscription_id,
            reason = outcome.reason(),
            attempts = attempt + 1,
            "push delivery failed"
        );
    }
    let mut deletion = None;
    if outcome.deletes_token() {
        if let Some(provider) = provider {
            state.metrics.record_token_deletion(provider);
        }
        deletion = Some((user_id.to_owned(), subscription.subscription_id.clone()));
    }
    Delivered {
        accepted: matches!(outcome, SendOutcome::Accepted),
        deletion,
    }
}

fn decode(kind: JobKind, payload: &[u8]) -> Result<Job, JobError> {
    match kind {
        JobKind::Message => job::decode_message(payload).map(Box::new).map(Job::Message),
        JobKind::Clear => job::decode_clear(payload).map(Job::Clear),
        JobKind::Ring => job::decode_ring(payload).map(Job::Ring),
    }
}

fn subject_of(kind: JobKind) -> &'static str {
    match kind {
        JobKind::Message => SUBJECT_MESSAGE,
        JobKind::Clear => SUBJECT_CLEAR,
        JobKind::Ring => SUBJECT_RING,
    }
}

fn subscribed(subscriptions: &HashMap<String, Vec<Subscription>>, user_id: &str) -> bool {
    subscriptions
        .get(user_id)
        .is_some_and(|subscriptions| !subscriptions.is_empty())
}

fn badge_of(badges: &HashMap<String, u32>, user_id: &str) -> u32 {
    badges.get(user_id).copied().unwrap_or(0)
}

fn record_depth(state: &AppState, admission: &Semaphore, capacity: usize) {
    state
        .metrics
        .set_queue_depth(capacity.saturating_sub(admission.available_permits()) as u64);
}

fn reap(result: Result<(), tokio::task::JoinError>) {
    if let Err(error) = result {
        warn!(error = %error, "push job task failed");
    }
}
