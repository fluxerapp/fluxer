// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::AtomicBool;
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
    #[allow(dead_code)]
    running: Arc<AtomicBool>,
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
        let running = Arc::new(AtomicBool::new(false));
        let (tx, rx) = pw_channel::<DirectCommand>();
        let sink_node_name = next_direct_sink_name();
        let inputs = DirectWorkerInputs {
            samples: samples.clone(),
            inventory,
            owned_link_snapshot,
            running: running.clone(),
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
                running,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipewire::common::{
        MEDIA_CLASS_CAPTURE_STREAM, PortRecord, pick_node_ports, pick_source_output_ports,
    };
    use crate::pipewire::routing::{default_sink_target_id, matching_pinned_capture_nodes};
    use crate::pipewire::stream_ops::{build_direct_stream_props, direct_chunk_payload_range};
    use crate::routing::{MEDIA_CLASS_PLAYBACK_STREAM, should_route_node};
    use std::collections::{HashMap, HashSet};

    #[test]
    fn direct_stream_props_capture_private_sink_monitor() {
        let props = build_direct_stream_props(
            "fluxer-direct-capture-7-1",
            "fluxer-direct-capture-7-1-stream",
        );
        let dict = props.dict();
        assert_eq!(
            dict.get("node.name"),
            Some("fluxer-direct-capture-7-1-stream")
        );
        assert_eq!(dict.get("media.type"), Some("Audio"));
        assert_eq!(dict.get("media.category"), Some("Capture"));
        assert_eq!(dict.get("media.class"), Some("Stream/Input/Audio"));
        assert_eq!(
            dict.get("stream.capture.sink"),
            Some("true"),
            "must tap the sink monitor; tapping a Stream/Output/Audio target.object does not produce frames",
        );
        assert_eq!(dict.get("node.passive"), Some("true"));
        assert_eq!(dict.get("node.virtual"), Some("true"));
        assert_eq!(dict.get("node.hidden"), Some("true"));
        assert_eq!(dict.get("node.dont-fallback"), Some("true"));
        assert_eq!(dict.get("node.dont-move"), Some("true"));
        assert_eq!(dict.get("node.dont-reconnect"), Some("true"));
        assert_eq!(dict.get("stream.dont-remix"), Some("true"));
        assert_eq!(dict.get("node.latency"), Some("4096/48000"));
        assert_eq!(dict.get("audio.rate"), Some("48000"));
        assert_eq!(dict.get("audio.channels"), Some("2"));
        assert_eq!(dict.get("audio.position"), Some("[FL,FR]"));
        assert_eq!(dict.get("target.object"), Some("fluxer-direct-capture-7-1"));
    }

    #[test]
    fn direct_chunk_payload_range_respects_pipewire_chunk_offset() {
        assert_eq!(direct_chunk_payload_range(64, 8, 16), Some(8..24));
        assert_eq!(direct_chunk_payload_range(18, 4, 16), Some(4..16));
        assert_eq!(direct_chunk_payload_range(64, 64, 16), None);
        assert_eq!(direct_chunk_payload_range(64, 8, 0), None);
        assert_eq!(direct_chunk_payload_range(10, 8, 2), None);
    }

    #[test]
    fn default_sink_target_id_uses_object_serial_for_matching_node_name() {
        let nodes = HashMap::from([
            (
                1,
                PropMap::from([
                    ("node.name".to_string(), "alsa_output.foo".to_string()),
                    ("object.serial".to_string(), "1234".to_string()),
                ]),
            ),
            (
                2,
                PropMap::from([
                    ("node.name".to_string(), "alsa_output.bar".to_string()),
                    ("object.serial".to_string(), "5678".to_string()),
                ]),
            ),
        ]);
        assert_eq!("1234", default_sink_target_id(&nodes, "alsa_output.foo"));
        assert_eq!("", default_sink_target_id(&nodes, "missing"));
    }

    #[test]
    fn matching_pinned_capture_nodes_only_matches_record_stream_inputs() {
        let nodes = HashMap::from([
            (
                10,
                PropMap::from([
                    (
                        "media.class".to_string(),
                        MEDIA_CLASS_CAPTURE_STREAM.to_string(),
                    ),
                    ("application.process.id".to_string(), "4242".to_string()),
                    ("media.name".to_string(), "RecordStream".to_string()),
                ]),
            ),
            (
                11,
                PropMap::from([
                    (
                        "media.class".to_string(),
                        MEDIA_CLASS_PLAYBACK_STREAM.to_string(),
                    ),
                    ("application.process.id".to_string(), "4242".to_string()),
                    ("media.name".to_string(), "RecordStream".to_string()),
                ]),
            ),
            (
                12,
                PropMap::from([
                    (
                        "media.class".to_string(),
                        MEDIA_CLASS_CAPTURE_STREAM.to_string(),
                    ),
                    ("application.process.id".to_string(), "4242".to_string()),
                    ("media.name".to_string(), "OtherCapture".to_string()),
                ]),
            ),
            (
                13,
                PropMap::from([
                    (
                        "media.class".to_string(),
                        MEDIA_CLASS_CAPTURE_STREAM.to_string(),
                    ),
                    ("application.process.id".to_string(), "9999".to_string()),
                    ("media.name".to_string(), "RecordStream".to_string()),
                ]),
            ),
        ]);
        let rule = RoutingRule {
            pin_target_for: vec![PropMap::from([
                ("application.process.id".to_string(), "4242".to_string()),
                ("media.name".to_string(), "RecordStream".to_string()),
            ])],
            ..Default::default()
        };
        assert_eq!(
            matching_pinned_capture_nodes(&nodes, &rule, 99),
            HashSet::from([10])
        );
        assert!(
            matching_pinned_capture_nodes(&nodes, &rule, 10).is_empty(),
            "the bridge's own sink id must never be pinned"
        );
    }

    #[test]
    fn inventory_enriches_nodes_with_owning_client_identity() {
        let mut inventory = InventorySnapshot::default();
        inventory.clients.insert(
            77,
            PropMap::from([
                ("application.name".to_string(), "Firefox".to_string()),
                ("application.process.id".to_string(), "4242".to_string()),
                (
                    "application.process.binary".to_string(),
                    "firefox".to_string(),
                ),
            ]),
        );
        inventory.nodes.insert(
            88,
            PropMap::from([
                ("client.id".to_string(), "77".to_string()),
                (
                    "media.class".to_string(),
                    MEDIA_CLASS_PLAYBACK_STREAM.to_string(),
                ),
                ("node.name".to_string(), "Firefox output".to_string()),
            ]),
        );
        let enriched = inventory.enriched_nodes();
        let node = enriched.get(&88).expect("enriched node");
        assert_eq!(
            node.get("application.process.id"),
            Some(&"4242".to_string())
        );
        assert_eq!(
            node.get("application.process.binary"),
            Some(&"firefox".to_string())
        );
        assert_eq!(node.get("node.name"), Some(&"Firefox output".to_string()));
        let rule = RoutingRule {
            include_when: vec![PropMap::from([(
                "application.process.id".to_string(),
                "4242".to_string(),
            )])],
            ..Default::default()
        };
        assert!(should_route_node(
            88,
            node,
            &rule,
            "",
            "",
            0,
            &SelfIdentity::default(),
        ));
    }

    #[test]
    fn inventory_keeps_node_properties_authoritative_over_client_props() {
        let mut inventory = InventorySnapshot::default();
        inventory.clients.insert(
            77,
            PropMap::from([("application.name".to_string(), "Client Name".to_string())]),
        );
        inventory.nodes.insert(
            88,
            PropMap::from([
                ("client.id".to_string(), "77".to_string()),
                ("application.name".to_string(), "Stream Name".to_string()),
            ]),
        );
        let enriched = inventory.enriched_nodes();
        let node = enriched.get(&88).expect("enriched node");
        assert_eq!(
            node.get("application.name"),
            Some(&"Stream Name".to_string())
        );
    }

    #[test]
    fn inventory_falls_back_to_pipewire_security_pid() {
        let mut inventory = InventorySnapshot::default();
        inventory.clients.insert(
            77,
            PropMap::from([("pipewire.sec.pid".to_string(), "5150".to_string())]),
        );
        inventory.nodes.insert(
            88,
            PropMap::from([("client.id".to_string(), "77".to_string())]),
        );
        let enriched = inventory.enriched_nodes();
        let node = enriched.get(&88).expect("enriched node");
        assert_eq!(
            node.get("application.process.id"),
            Some(&"5150".to_string())
        );
    }

    #[test]
    fn inventory_does_not_inherit_client_object_serial_as_node_target() {
        let mut inventory = InventorySnapshot::default();
        inventory.clients.insert(
            77,
            PropMap::from([
                ("application.process.id".to_string(), "4242".to_string()),
                ("object.serial".to_string(), "client-serial".to_string()),
            ]),
        );
        inventory.nodes.insert(
            88,
            PropMap::from([
                ("client.id".to_string(), "77".to_string()),
                ("node.name".to_string(), "Playback Stream".to_string()),
            ]),
        );
        let enriched = inventory.enriched_nodes();
        let node = enriched.get(&88).expect("enriched node");
        assert_eq!(
            node.get("application.process.id"),
            Some(&"4242".to_string())
        );
        assert_eq!(node.get("object.serial"), None);
        assert_eq!(node.get("node.name"), Some(&"Playback Stream".to_string()));
    }

    fn port(node_id: u32, dir: &str, ch: &str) -> PortRecord {
        PortRecord {
            node_id,
            direction: dir.into(),
            channel: ch.into(),
            props: PropMap::new(),
        }
    }

    #[test]
    fn pick_source_output_ports_prefers_stereo_pair() {
        let mut ports = HashMap::new();
        ports.insert(1, port(42, "out", "fl"));
        ports.insert(2, port(42, "out", "fr"));
        ports.insert(3, port(42, "in", "FL"));
        ports.insert(4, port(99, "out", "FL"));
        let (l, r) = pick_source_output_ports(42, &ports).expect("stereo pair");
        assert_eq!(l, 1);
        assert_eq!(r, 2);
    }

    #[test]
    fn pick_source_output_ports_falls_back_to_first_two_jack_style_ports() {
        let mut ports = HashMap::new();
        ports.insert(30, port(42, "out", "AUX1"));
        ports.insert(20, port(42, "out", "AUX0"));
        ports.insert(10, port(42, "in", "AUX0"));
        let (l, r) = pick_source_output_ports(42, &ports).expect("jack-style stereo fallback");
        assert_eq!(l, 20);
        assert_eq!(r, 30);
    }

    #[test]
    fn pick_node_ports_applies_same_fallback_to_private_capture_inputs() {
        let mut ports = HashMap::new();
        ports.insert(8, port(7, "in", "1"));
        ports.insert(9, port(7, "in", "2"));
        let (l, r) = pick_node_ports(7, "in", &ports).expect("input stereo fallback");
        assert_eq!(l, 8);
        assert_eq!(r, 9);
    }

    #[test]
    fn pick_source_output_ports_fans_mono_to_both_inputs() {
        let mut ports = HashMap::new();
        ports.insert(7, port(42, "out", "MONO"));
        let (l, r) = pick_source_output_ports(42, &ports).expect("mono fan-out");
        assert_eq!(l, 7);
        assert_eq!(r, 7);
    }

    #[test]
    fn pick_source_output_ports_treats_blank_channel_as_mono() {
        let mut ports = HashMap::new();
        ports.insert(11, port(42, "out", ""));
        let (l, r) = pick_source_output_ports(42, &ports).expect("blank-channel fallback");
        assert_eq!(l, 11);
        assert_eq!(r, 11);
    }

    #[test]
    fn pick_source_output_ports_returns_none_when_no_outputs() {
        let ports = HashMap::new();
        assert!(pick_source_output_ports(42, &ports).is_none());
    }

    #[test]
    fn pick_source_output_ports_returns_none_when_only_one_side_present() {
        let mut ports = HashMap::new();
        ports.insert(1, port(42, "out", "FL"));
        assert!(pick_source_output_ports(42, &ports).is_none());
    }

    use crate::pipewire::stream_ops::{build_test_user_data, process_audio_chunk};
    use fluxer_rt_thread::MonotonicClock;
    use std::sync::atomic::{AtomicU64, Ordering};

    #[derive(Debug)]
    struct FakeClock {
        value_ns: AtomicU64,
    }

    impl FakeClock {
        fn new(initial_ns: u64) -> Self {
            Self {
                value_ns: AtomicU64::new(initial_ns),
            }
        }
        fn set(&self, value_ns: u64) {
            self.value_ns.store(value_ns, Ordering::Release);
        }
    }

    impl MonotonicClock for FakeClock {
        fn now_ns(&self) -> u64 {
            self.value_ns.load(Ordering::Acquire)
        }
    }

    fn make_f32_payload(samples: &[f32]) -> Vec<u8> {
        let mut out = Vec::with_capacity(samples.len() * 4);
        for sample in samples {
            out.extend_from_slice(&sample.to_ne_bytes());
        }
        out
    }

    #[test]
    fn callback_path_does_not_allocate_in_steady_state() {
        let clock = Arc::new(FakeClock::new(1_000));
        let mut data = build_test_user_data(Arc::clone(&clock) as Arc<dyn MonotonicClock>);
        let frame = vec![0.05_f32; 960];
        let payload = make_f32_payload(&frame);
        for _ in 0..400 {
            clock.set(clock.now_ns() + 10_000_000);
            process_audio_chunk(&mut data, &payload);
        }
        let allocs_before = crate::test_alloc::ALLOC_PROBE.load(Ordering::Relaxed);
        crate::test_alloc::begin_thread_alloc_probe();
        clock.set(clock.now_ns() + 10_000_000);
        process_audio_chunk(&mut data, &payload);
        let probed = crate::test_alloc::end_thread_alloc_probe();
        let allocs_after = crate::test_alloc::ALLOC_PROBE.load(Ordering::Relaxed);
        assert_eq!(
            probed,
            0,
            "steady-state callback allocated {probed} times (global delta {})",
            allocs_after.saturating_sub(allocs_before)
        );
    }
}
