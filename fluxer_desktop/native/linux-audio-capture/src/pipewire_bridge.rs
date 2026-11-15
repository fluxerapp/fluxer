// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use pipewire as pw;
use pw::channel::{Sender as PwSender, channel as pw_channel};

use fluxer_rt_thread::{MonotonicClock, SystemMonotonicClock};

use crate::audio_contract::DIRECT_CAPTURE_MAX_READ_SAMPLES;
use crate::direct_buffer::CapturedFrame;
use crate::direct_buffer::DirectAudioBuffer;
use crate::pipewire::common::{
    InventorySnapshot, READY_TIMEOUT_MS, daemon_reachable as common_daemon_reachable,
    next_direct_sink_name,
};
use crate::pipewire::event_loop::{
    BridgeCommand, DirectCommand, DirectWorkerInputs, run_bridge_worker, run_direct_worker,
};
use crate::routing::{PropMap, RoutingRule, SelfIdentity};

pub fn daemon_reachable() -> bool {
    common_daemon_reachable()
}

pub struct PipeWireBridge {
    snapshot: Arc<Mutex<InventorySnapshot>>,
    tx: PwSender<BridgeCommand>,
    thread: Mutex<Option<JoinHandle<()>>>,
}

impl PipeWireBridge {
    pub fn open() -> Option<Self> {
        if !daemon_reachable() {
            return None;
        }
        let snapshot = Arc::new(Mutex::new(InventorySnapshot::default()));
        let owned_link_snapshot = Arc::new(Mutex::new(Vec::new()));
        let (tx, rx) = pw_channel::<BridgeCommand>();
        let snap_for_thread = snapshot.clone();
        let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel::<bool>(1);
        let handle = thread::Builder::new()
            .name("fluxer-pipewire-bridge".into())
            .spawn(move || {
                run_bridge_worker(snap_for_thread, owned_link_snapshot, rx, ready_tx);
            })
            .ok()?;
        match ready_rx.recv_timeout(Duration::from_millis(READY_TIMEOUT_MS)) {
            Ok(true) => Some(Self {
                snapshot,
                tx,
                thread: Mutex::new(Some(handle)),
            }),
            _ => {
                let _ = tx.send(BridgeCommand::Shutdown);
                let _ = handle.join();
                None
            }
        }
    }
}

impl Drop for PipeWireBridge {
    fn drop(&mut self) {
        let _ = self.tx.send(BridgeCommand::Shutdown);
        if let Ok(mut thread) = self.thread.lock()
            && let Some(handle) = thread.take()
        {
            let _ = handle.join();
        }
    }
}

impl PipeWireBridge {
    pub fn inventory(&self) -> Vec<PropMap> {
        match self.snapshot.lock() {
            Ok(guard) => guard.enriched_node_values(),
            Err(_) => Vec::new(),
        }
    }

    pub fn apply(&self, rule: RoutingRule) -> bool {
        self.tx.send(BridgeCommand::Apply(rule)).is_ok()
    }

    pub fn release(&self) {
        let _ = self.tx.send(BridgeCommand::Release);
    }

    pub fn populate_self_identity(&self, identity: SelfIdentity) {
        let _ = self.tx.send(BridgeCommand::SetIdentity(identity));
    }
}

pub struct PipeWireDirectCapture {
    samples: Arc<Mutex<DirectAudioBuffer>>,
    tx: PwSender<DirectCommand>,
    thread: Mutex<Option<JoinHandle<()>>>,
    identity: Mutex<SelfIdentity>,
}

impl PipeWireDirectCapture {
    pub fn open() -> Option<Self> {
        Self::open_with_clock(Arc::new(SystemMonotonicClock::new()))
    }

    pub fn open_with_clock(clock: Arc<dyn MonotonicClock>) -> Option<Self> {
        if !daemon_reachable() {
            return None;
        }
        let samples = Arc::new(Mutex::new(DirectAudioBuffer::default_format()));
        let inventory = Arc::new(Mutex::new(InventorySnapshot::default()));
        let owned_link_snapshot = Arc::new(Mutex::new(Vec::new()));
        let (tx, rx) = pw_channel::<DirectCommand>();
        let sink_node_name = next_direct_sink_name();
        let inputs = DirectWorkerInputs {
            samples: samples.clone(),
            inventory,
            owned_link_snapshot,
            sink_node_name,
            clock,
        };
        let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel::<bool>(1);
        let handle = thread::Builder::new()
            .name("fluxer-pipewire-direct".into())
            .spawn(move || {
                run_direct_worker(inputs, rx, ready_tx);
            })
            .ok()?;
        match ready_rx.recv_timeout(Duration::from_millis(READY_TIMEOUT_MS)) {
            Ok(true) => Some(Self {
                samples,
                tx,
                thread: Mutex::new(Some(handle)),
                identity: Mutex::new(SelfIdentity::default()),
            }),
            _ => {
                let _ = tx.send(DirectCommand::Shutdown);
                let _ = handle.join();
                None
            }
        }
    }
}

impl Drop for PipeWireDirectCapture {
    fn drop(&mut self) {
        let _ = self.tx.send(DirectCommand::Shutdown);
        if let Ok(mut thread) = self.thread.lock()
            && let Some(handle) = thread.take()
        {
            let _ = handle.join();
        }
    }
}

impl PipeWireDirectCapture {
    pub fn start(&self, rule: RoutingRule) -> bool {
        let Ok(identity) = self.identity.lock().map(|guard| guard.clone()) else {
            return false;
        };
        self.tx
            .send(DirectCommand::Start {
                rule,
                identity: Box::new(identity),
            })
            .is_ok()
    }

    pub fn set_rule(&self, rule: RoutingRule) -> bool {
        self.tx.send(DirectCommand::UpdateRule { rule }).is_ok()
    }

    pub fn read(&self) -> Option<CapturedFrame> {
        let mut out = Vec::with_capacity(DIRECT_CAPTURE_MAX_READ_SAMPLES);
        let meta = {
            let Ok(mut guard) = self.samples.lock() else {
                return None;
            };
            guard.read_into(&mut out)?
        };
        Some(CapturedFrame {
            samples: out,
            sample_rate: meta.sample_rate,
            channels: meta.channels,
            timestamp_us: meta.timestamp_us,
        })
    }

    pub fn stop(&self) {
        let _ = self.tx.send(DirectCommand::Stop);
    }

    pub fn populate_self_identity(&self, identity: SelfIdentity) {
        if let Ok(mut guard) = self.identity.lock() {
            *guard = identity;
        }
    }
}
