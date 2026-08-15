// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicBool, AtomicU64};
use std::sync::{Arc, Mutex, RwLock};
use std::thread::{self, JoinHandle};
use std::time::Duration;

use fluxer_screen_frame_bus::NativeScreenFrameSinkHandleRef;
use pipewire as pw;
use pw::channel::{Sender as PwSender, channel as pw_channel};

use fluxer_rt_thread::{MonotonicClock, SystemMonotonicClock};

use crate::audio_contract::DIRECT_CAPTURE_MAX_READ_SAMPLES;
use crate::backend::{CaptureBridge, CapturedFrame, DirectCapture, RoutingGraphSnapshot};
use crate::direct_buffer::DirectAudioBuffer;
use crate::pipewire::common::{
    InventorySnapshot, LinkKey, READY_TIMEOUT_MS, build_routing_graph_snapshot,
    daemon_reachable as common_daemon_reachable, next_direct_sink_name,
};
use crate::pipewire::event_loop::{
    BridgeCommand, DirectCommand, DirectWorkerInputs, run_bridge_worker, run_direct_worker,
};
use crate::pipewire::stream_ops::ScreenAudioSinkSlot;
use crate::routing::{PropMap, RoutingRule, SelfIdentity};

pub fn daemon_reachable() -> bool {
    common_daemon_reachable()
}

pub struct PipeWireBridge {
    snapshot: Arc<Mutex<InventorySnapshot>>,
    owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
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
        let links_for_thread = owned_link_snapshot.clone();
        let (ready_tx, ready_rx) = std::sync::mpsc::sync_channel::<bool>(1);
        let handle = thread::Builder::new()
            .name("fluxer-pipewire-bridge".into())
            .spawn(move || {
                run_bridge_worker(snap_for_thread, links_for_thread, rx, ready_tx);
            })
            .ok()?;
        match ready_rx.recv_timeout(Duration::from_millis(READY_TIMEOUT_MS)) {
            Ok(true) => Some(Self {
                snapshot,
                owned_link_snapshot,
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

impl CaptureBridge for PipeWireBridge {
    fn inventory(&self) -> Vec<PropMap> {
        match self.snapshot.lock() {
            Ok(guard) => guard.enriched_node_values(),
            Err(_) => Vec::new(),
        }
    }

    fn apply(&self, rule: RoutingRule) -> bool {
        self.tx.send(BridgeCommand::Apply(rule)).is_ok()
    }

    fn release(&self) {
        let _ = self.tx.send(BridgeCommand::Release);
    }

    fn populate_self_identity(&self, identity: SelfIdentity) {
        let _ = self.tx.send(BridgeCommand::SetIdentity(identity));
    }

    fn backend_name(&self) -> &'static str {
        "pipewire"
    }

    fn routing_graph(&self) -> RoutingGraphSnapshot {
        build_routing_graph_snapshot("pipewire", &self.snapshot, &self.owned_link_snapshot)
    }
}

pub struct PipeWireDirectCapture {
    samples: Arc<Mutex<DirectAudioBuffer>>,
    inventory: Arc<Mutex<InventorySnapshot>>,
    owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    tx: PwSender<DirectCommand>,
    #[allow(dead_code)]
    running: Arc<AtomicBool>,
    thread: Mutex<Option<JoinHandle<()>>>,
    identity: Mutex<SelfIdentity>,
    #[allow(dead_code)]
    last_push_ns: Arc<AtomicU64>,
    screen_audio_sink: ScreenAudioSinkSlot,
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
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let screen_audio_sink: ScreenAudioSinkSlot = Arc::new(RwLock::new(None));
        let (tx, rx) = pw_channel::<DirectCommand>();
        let sink_node_name = next_direct_sink_name();
        let inputs = DirectWorkerInputs {
            samples: samples.clone(),
            inventory: inventory.clone(),
            owned_link_snapshot: owned_link_snapshot.clone(),
            running: running.clone(),
            sink_node_name,
            last_push_ns: last_push_ns.clone(),
            clock,
            screen_audio_sink: screen_audio_sink.clone(),
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
                inventory,
                owned_link_snapshot,
                tx,
                running,
                thread: Mutex::new(Some(handle)),
                identity: Mutex::new(SelfIdentity::default()),
                last_push_ns,
                screen_audio_sink,
            }),
            _ => {
                let _ = tx.send(DirectCommand::Shutdown);
                let _ = handle.join();
                None
            }
        }
    }

    #[allow(dead_code)]
    pub fn last_push_ns(&self) -> u64 {
        self.last_push_ns.load(std::sync::atomic::Ordering::Acquire)
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

impl DirectCapture for PipeWireDirectCapture {
    fn start(&self, rule: RoutingRule) -> bool {
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

    fn set_rule(&self, rule: RoutingRule) -> bool {
        self.tx.send(DirectCommand::UpdateRule { rule }).is_ok()
    }

    fn read(&self) -> Option<CapturedFrame> {
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

    fn stop(&self) {
        let _ = self.tx.send(DirectCommand::Stop);
    }

    fn set_screen_audio_sink(&self, sink: Arc<NativeScreenFrameSinkHandleRef>) {
        if let Ok(mut guard) = self.screen_audio_sink.write() {
            *guard = Some(sink);
        }
    }

    fn clear_screen_audio_sink(&self) {
        if let Ok(mut guard) = self.screen_audio_sink.write() {
            *guard = None;
        }
    }

    fn populate_self_identity(&self, identity: SelfIdentity) {
        if let Ok(mut guard) = self.identity.lock() {
            *guard = identity;
        }
    }

    fn routing_graph(&self) -> RoutingGraphSnapshot {
        build_routing_graph_snapshot("pipewire", &self.inventory, &self.owned_link_snapshot)
    }

    fn last_push_ns_arc(&self) -> Option<Arc<AtomicU64>> {
        Some(Arc::clone(&self.last_push_ns))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pipewire::common::{
        DIRECT_SINK_PREFIX, MEDIA_CLASS_CAPTURE_STREAM, PortRecord, SINK_NODE_DESCRIPTION,
        SINK_NODE_NAME, VirtualSinkKind, build_link_props, build_virtual_sink_props,
        build_virtual_sink_props_for, is_routable_media_class, pick_node_ports,
        pick_source_output_ports,
    };
    use crate::pipewire::routing::{default_sink_target_id, matching_pinned_capture_nodes};
    use crate::pipewire::stream_ops::{
        DIRECT_CAPTURE_APM_FRAME_SAMPLES, DirectCaptureApm, build_direct_audio_info,
        build_direct_stream_props, direct_chunk_payload_range, f32_sample_to_i16,
        i16_sample_to_f32,
    };
    use crate::routing::MEDIA_CLASS_PLAYBACK_STREAM;
    use std::collections::{HashMap, HashSet};

    use crate::audio_contract::{DIRECT_CAPTURE_CHANNELS, DIRECT_CAPTURE_SAMPLE_RATE};
    use crate::routing::should_route_node;
    use pipewire::spa::sys as spa_sys;

    #[test]
    fn is_routable_media_class_matches_audio_node_classes() {
        assert!(is_routable_media_class(MEDIA_CLASS_PLAYBACK_STREAM));
        assert!(is_routable_media_class(MEDIA_CLASS_CAPTURE_STREAM));
        assert!(is_routable_media_class("Audio/Source"));
        assert!(is_routable_media_class("Audio/Sink"));
        assert!(!is_routable_media_class("Video/Source"));
        assert!(!is_routable_media_class(""));
    }

    #[test]
    fn virtual_sink_props_match_legacy_contract() {
        let props = build_virtual_sink_props();
        let dict = props.dict();
        assert_eq!(dict.get("factory.name"), Some("support.null-audio-sink"));
        assert_eq!(dict.get("node.name"), Some(SINK_NODE_NAME));
        assert_eq!(dict.get("node.nick"), Some(SINK_NODE_NAME));
        assert_eq!(dict.get("node.description"), Some(SINK_NODE_DESCRIPTION));
        assert_eq!(dict.get("media.class"), Some("Audio/Source/Virtual"));
        assert_eq!(dict.get("node.virtual"), Some("true"));
        assert_eq!(dict.get("node.passive"), Some("true"));
        assert_eq!(dict.get("node.dont-move"), Some("true"));
        assert_eq!(dict.get("node.dont-reconnect"), Some("true"));
        assert_eq!(dict.get("node.latency"), Some("4096/48000"));
        assert_eq!(dict.get("audio.rate"), Some("48000"));
        assert_eq!(dict.get("audio.channels"), Some("2"));
        assert_eq!(dict.get("audio.position"), Some("[FL,FR]"));
        assert_eq!(dict.get("monitor.channel-volumes"), Some("true"));
    }

    #[test]
    fn link_props_carry_per_port_routing() {
        let props = build_link_props(101, 7, 202, 13);
        let dict = props.dict();
        assert_eq!(dict.get("link.output.node"), Some("101"));
        assert_eq!(dict.get("link.output.port"), Some("7"));
        assert_eq!(dict.get("link.input.node"), Some("202"));
        assert_eq!(dict.get("link.input.port"), Some("13"));
        assert_eq!(dict.get("object.linger"), Some("false"));
        assert_eq!(dict.get("link.passive"), Some("true"));
    }

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
    fn direct_audio_info_advertises_stereo_fl_fr() {
        let info = build_direct_audio_info();
        assert_eq!(
            info.format(),
            pipewire::spa::param::audio::AudioFormat::F32LE
        );
        assert_eq!(info.rate(), DIRECT_CAPTURE_SAMPLE_RATE);
        assert_eq!(info.channels(), DIRECT_CAPTURE_CHANNELS);
        assert_eq!(info.position()[0], spa_sys::SPA_AUDIO_CHANNEL_FL);
        assert_eq!(info.position()[1], spa_sys::SPA_AUDIO_CHANNEL_FR);
    }

    #[test]
    fn next_direct_sink_name_is_unique_per_call() {
        let a = next_direct_sink_name();
        let b = next_direct_sink_name();
        assert_ne!(a, b, "concurrent captures must not collide on sink names");
        assert!(a.starts_with(DIRECT_SINK_PREFIX));
        assert!(b.starts_with(DIRECT_SINK_PREFIX));
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
    fn private_sink_props_advertise_hidden_audio_sink() {
        let private = build_virtual_sink_props_for(
            "fluxer-direct-capture-7-1",
            crate::pipewire::common::DIRECT_SINK_DESCRIPTION,
            VirtualSinkKind::PrivateAudioSink,
        );
        let legacy = build_virtual_sink_props_for(
            SINK_NODE_NAME,
            SINK_NODE_DESCRIPTION,
            VirtualSinkKind::LegacyVirtualSource,
        );
        assert_eq!(private.dict().get("media.class"), Some("Audio/Sink"));
        assert_eq!(private.dict().get("node.hidden"), Some("true"));
        assert_eq!(
            legacy.dict().get("media.class"),
            Some("Audio/Source/Virtual")
        );
        assert_eq!(legacy.dict().get("node.hidden"), None);
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

    #[test]
    fn direct_capture_apm_processes_one_full_frame_increments_counter() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        assert_eq!(apm.apm_frames_processed(), 0);
        let frame_len = DIRECT_CAPTURE_APM_FRAME_SAMPLES;
        let mut samples = vec![0.5_f32; frame_len];
        let processed = apm.process_in_place(&mut samples).expect("process");
        assert_eq!(processed, frame_len);
        assert_eq!(apm.apm_frames_processed(), 1);
        assert_eq!(apm.pending_accumulator_len(), 0);
    }

    #[test]
    fn direct_capture_apm_accumulates_partial_frames_across_calls() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        let half = DIRECT_CAPTURE_APM_FRAME_SAMPLES / 2;
        let mut first = vec![0.1_f32; half];
        let processed1 = apm.process_in_place(&mut first).expect("first");
        assert_eq!(processed1, 0);
        assert_eq!(apm.apm_frames_processed(), 0);
        assert_eq!(apm.pending_accumulator_len(), half);
        let mut second = vec![0.2_f32; half];
        let processed2 = apm.process_in_place(&mut second).expect("second");
        assert_eq!(processed2, half);
        assert_eq!(apm.apm_frames_processed(), 1);
        assert_eq!(apm.pending_accumulator_len(), 0);
    }

    #[test]
    fn direct_capture_apm_handles_many_frames_in_one_call() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        let frame_len = DIRECT_CAPTURE_APM_FRAME_SAMPLES;
        let mut samples = vec![0.25_f32; frame_len * 5];
        let processed = apm.process_in_place(&mut samples).expect("process");
        assert_eq!(processed, frame_len * 5);
        assert_eq!(apm.apm_frames_processed(), 5);
        assert_eq!(apm.pending_accumulator_len(), 0);
    }

    #[test]
    fn direct_capture_apm_stub_preserves_samples_within_tolerance() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        let frame_len = DIRECT_CAPTURE_APM_FRAME_SAMPLES;
        let mut samples = vec![0.0_f32; frame_len];
        for n in 0..frame_len {
            samples[n] = ((n as f32) / (frame_len as f32) - 0.5) * 0.5;
        }
        let original = samples.clone();
        let _ = apm.process_in_place(&mut samples).expect("process");
        for (after, before) in samples.iter().zip(original.iter()) {
            let diff = (after - before).abs();
            assert!(diff < 1e-3);
        }
    }

    #[test]
    fn direct_capture_apm_reconfigure_is_noop_when_format_unchanged() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        let half = DIRECT_CAPTURE_APM_FRAME_SAMPLES / 2;
        let mut samples = vec![0.3_f32; half];
        let _ = apm.process_in_place(&mut samples).expect("first");
        assert_eq!(apm.pending_accumulator_len(), half);
        apm.reconfigure(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
            .expect("noop");
        assert_eq!(apm.pending_accumulator_len(), half);
    }

    #[test]
    fn direct_capture_apm_reconfigure_changes_format_and_resets_accum() {
        let mut apm =
            DirectCaptureApm::new(DIRECT_CAPTURE_SAMPLE_RATE, DIRECT_CAPTURE_CHANNELS as u16)
                .expect("apm");
        let half = DIRECT_CAPTURE_APM_FRAME_SAMPLES / 2;
        let mut samples = vec![0.3_f32; half];
        let _ = apm.process_in_place(&mut samples).expect("first");
        assert!(apm.pending_accumulator_len() > 0);
        apm.reconfigure(16_000, 1).expect("reconfigure");
        assert_eq!(apm.pending_accumulator_len(), 0);
    }

    #[test]
    fn f32_to_i16_clamps_above_one() {
        assert_eq!(f32_sample_to_i16(2.0), i16::MAX);
        assert_eq!(f32_sample_to_i16(-2.0), i16::MIN);
        assert_eq!(f32_sample_to_i16(0.0), 0);
    }

    #[test]
    fn i16_to_f32_round_trip_is_bounded() {
        for sample in [-32_768_i16, -1, 0, 1, 32_767] {
            let f = i16_sample_to_f32(sample);
            assert!((-1.001..=1.001).contains(&f));
        }
    }

    use crate::ignore_audio_runtime::SOURCE_STALE_AFTER_NS;
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
    fn production_callback_marks_freshness_with_monotonic_clock() {
        let clock = Arc::new(FakeClock::new(7_500_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut data = build_test_user_data(
            last_push_ns.clone(),
            Arc::clone(&clock) as Arc<dyn MonotonicClock>,
        );
        assert_eq!(last_push_ns.load(Ordering::Acquire), u64::MAX);
        let frame: Vec<f32> = (0..960).map(|n| (n as f32) * 0.0001).collect();
        let payload = make_f32_payload(&frame);
        process_audio_chunk(&mut data, &payload);
        let observed = last_push_ns.load(Ordering::Acquire);
        assert_eq!(observed, 7_500_000);
        assert_ne!(observed, u64::MAX);
    }

    #[test]
    fn freshness_age_grows_to_signal_stale_source_after_threshold() {
        let clock = Arc::new(FakeClock::new(1_000_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut data = build_test_user_data(
            last_push_ns.clone(),
            Arc::clone(&clock) as Arc<dyn MonotonicClock>,
        );
        let frame = vec![0.1_f32; DIRECT_CAPTURE_APM_FRAME_SAMPLES];
        let payload = make_f32_payload(&frame);
        process_audio_chunk(&mut data, &payload);
        let after_first = last_push_ns.load(Ordering::Acquire);
        assert_eq!(after_first, 1_000_000);
        clock.set(1_000_000 + SOURCE_STALE_AFTER_NS + 1);
        let age = clock.now_ns() - after_first;
        assert!(age > SOURCE_STALE_AFTER_NS);
        clock.set(2_000_000 + SOURCE_STALE_AFTER_NS + 1);
        let payload2 = make_f32_payload(&frame);
        process_audio_chunk(&mut data, &payload2);
        let after_second = last_push_ns.load(Ordering::Acquire);
        assert!(after_second > after_first);
        assert_eq!(after_second, 2_000_000 + SOURCE_STALE_AFTER_NS + 1);
        let fresh_age = clock.now_ns() - after_second;
        assert_eq!(fresh_age, 0);
    }

    #[test]
    fn callback_path_does_not_allocate_in_steady_state() {
        let clock = Arc::new(FakeClock::new(1_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut data = build_test_user_data(
            last_push_ns.clone(),
            Arc::clone(&clock) as Arc<dyn MonotonicClock>,
        );
        let frame = vec![0.05_f32; DIRECT_CAPTURE_APM_FRAME_SAMPLES];
        let payload = make_f32_payload(&frame);
        for _ in 0..400 {
            clock.set(clock.now_ns() + 10_000_000);
            process_audio_chunk(&mut data, &payload);
        }
        let allocs_before = crate::audio_mix_runtime::ALLOC_PROBE.load(Ordering::Relaxed);
        crate::audio_mix_runtime::begin_thread_alloc_probe();
        clock.set(clock.now_ns() + 10_000_000);
        process_audio_chunk(&mut data, &payload);
        let probed = crate::audio_mix_runtime::end_thread_alloc_probe();
        let allocs_after = crate::audio_mix_runtime::ALLOC_PROBE.load(Ordering::Relaxed);
        assert_eq!(
            probed,
            0,
            "steady-state callback allocated {probed} times (global delta {})",
            allocs_after.saturating_sub(allocs_before)
        );
    }

    #[test]
    fn production_callback_freshness_drives_audio_mix_runtime_mark_pushed() {
        use crate::audio_mix_runtime::{
            AudioMixRuntimeBuilder, CaptureSource, MIX_CHANNELS, MIX_SAMPLE_RATE_HZ,
            NullMixOutputSink,
        };
        let clock = Arc::new(FakeClock::new(9_000_000));
        let last_push_ns = Arc::new(AtomicU64::new(u64::MAX));
        let mut data = build_test_user_data(
            last_push_ns.clone(),
            Arc::clone(&clock) as Arc<dyn MonotonicClock>,
        );
        let source_id: u64 = 1;
        let (_source, consumer) =
            CaptureSource::create(source_id, MIX_SAMPLE_RATE_HZ, MIX_CHANNELS).expect("source");
        let mut runtime = AudioMixRuntimeBuilder::new()
            .with_clock(Arc::clone(&clock) as Arc<dyn MonotonicClock>)
            .add_source_with_freshness(source_id, consumer, Arc::clone(&last_push_ns))
            .build(NullMixOutputSink)
            .expect("build");
        assert_eq!(runtime.mark_pushed_total(), 0);
        assert_eq!(last_push_ns.load(Ordering::Acquire), u64::MAX);
        let frame: Vec<f32> = (0..DIRECT_CAPTURE_APM_FRAME_SAMPLES)
            .map(|n| (n as f32) * 0.0001)
            .collect();
        let payload = make_f32_payload(&frame);
        process_audio_chunk(&mut data, &payload);
        let observed = last_push_ns.load(Ordering::Acquire);
        assert_eq!(observed, 9_000_000);
        let _ = runtime.run_one_tick_blocking(observed).expect("frame");
        assert!(
            runtime.mark_pushed_total() >= 1,
            "AudioMixRuntime.tick() did not invoke StaleSourceTracker::mark_pushed",
        );
        let not_stale = !runtime.is_source_stale(0, observed + 1_000_000, 5_000_000_000);
        assert!(
            not_stale,
            "source must not be stale immediately after a fresh push"
        );
    }
}
