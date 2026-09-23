// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::job::{ClearJob, MessageJob, RingJob};
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

const DONE_TTL: Duration = Duration::from_secs(300);
const MAX_DONE: usize = 200_000;
const KEY_BYTES: usize = 16;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub struct JobKey([u8; KEY_BYTES]);

impl JobKey {
    pub fn of_message(job: &MessageJob, user_id: &str) -> Self {
        Self::digest(&["message", &job.message_id, &job.channel_id, user_id])
    }

    pub fn of_clear(job: &ClearJob) -> Self {
        Self::digest(&["clear", &job.user_id, &job.channel_id, &job.message_id])
    }

    pub fn of_ring(job: &RingJob) -> Self {
        Self::digest(&[
            "ring",
            &job.message_id,
            &job.channel_id,
            &job.user_id,
            &job.started_at_ms.to_string(),
        ])
    }

    fn digest(parts: &[&str]) -> Self {
        let mut hasher = Sha256::new();
        for part in parts {
            hasher.update(part.as_bytes());
            hasher.update([0]);
        }
        let mut key = [0u8; KEY_BYTES];
        key.copy_from_slice(&hasher.finalize()[..KEY_BYTES]);
        Self(key)
    }
}

pub enum Seen {
    New(Claim),
    Done,
    Running,
}

#[derive(Clone, Default)]
pub struct DoneJobs {
    inner: Arc<Mutex<Entries>>,
}

#[derive(Default)]
struct Entries {
    running: HashSet<JobKey>,
    done: HashMap<JobKey, Instant>,
    done_order: VecDeque<(Instant, JobKey)>,
}

impl Entries {
    fn prune(&mut self, now: Instant) {
        while let Some(&(at, key)) = self.done_order.front() {
            if now.duration_since(at) < DONE_TTL && self.done_order.len() <= MAX_DONE {
                break;
            }
            self.done_order.pop_front();
            if self.done.get(&key) == Some(&at) {
                self.done.remove(&key);
            }
        }
    }
}

impl DoneJobs {
    pub fn claim(&self, key: JobKey) -> Seen {
        let mut entries = self
            .inner
            .lock()
            .expect("the done job set is never poisoned");
        entries.prune(Instant::now());
        if entries.done.contains_key(&key) {
            return Seen::Done;
        }
        if !entries.running.insert(key) {
            return Seen::Running;
        }
        Seen::New(Claim {
            key,
            jobs: self.clone(),
            finished: false,
        })
    }

    fn finish(&self, key: JobKey, done: bool) {
        let mut entries = self
            .inner
            .lock()
            .expect("the done job set is never poisoned");
        entries.running.remove(&key);
        if done {
            let now = Instant::now();
            entries.done.insert(key, now);
            entries.done_order.push_back((now, key));
            entries.prune(now);
        }
    }
}

pub struct Claim {
    key: JobKey,
    jobs: DoneJobs,
    finished: bool,
}

impl Claim {
    pub fn done(mut self) {
        self.jobs.finish(self.key, true);
        self.finished = true;
    }
}

impl Drop for Claim {
    fn drop(&mut self) {
        if !self.finished {
            self.jobs.finish(self.key, false);
        }
    }
}
