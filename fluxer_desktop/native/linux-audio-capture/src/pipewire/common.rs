// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use pipewire as pw;
use pw::keys;
use pw::metadata::{Metadata, MetadataListener};
use pw::properties::{PropertiesBox, properties};

use fluxer_rt_thread::{PriorityProfile, RealtimePriorityGuard, RtError, RtOutcome};

use crate::audio_contract::{self, DIRECT_CAPTURE_SAMPLE_RATE};
use crate::routing::PropMap;

pub(crate) const READY_TIMEOUT_MS: u64 = 2_000;

pub(crate) const SINK_NODE_NAME: &str = "fluxer-screen-share";
pub(crate) const SINK_NODE_DESCRIPTION: &str = "Fluxer Screen Share Audio";
pub(crate) const DIRECT_SINK_PREFIX: &str = "fluxer-direct-capture";
pub(crate) const DIRECT_SINK_DESCRIPTION: &str = "Fluxer Direct Capture Audio";
pub(crate) const MEDIA_CLASS_CAPTURE_STREAM: &str = "Stream/Input/Audio";
pub(crate) const PIN_TARGET_METADATA_KEY: &str = "target.object";
pub(crate) const PIN_TARGET_METADATA_TYPE: &str = "Spa:String";

pub(crate) const CH_FRONT_LEFT: &str = "FL";
pub(crate) const CH_FRONT_RIGHT: &str = "FR";
pub(crate) const CH_MONO: &str = "MONO";

pub const MAX_FRAME_SAMPLES: usize = 1_920;

const _: () = assert!(MAX_FRAME_SAMPLES > 0);
const _: () = assert!(MAX_FRAME_SAMPLES <= 8_192);

pub(crate) static DIRECT_SINK_COUNTER: AtomicU64 = AtomicU64::new(1);

#[derive(Copy, Clone)]
pub(crate) enum VirtualSinkKind {
    LegacyVirtualSource,
    PrivateAudioSink,
}

pub(crate) fn next_direct_sink_name() -> String {
    let seq = DIRECT_SINK_COUNTER.fetch_add(1, Ordering::Relaxed);
    assert!(seq > 0);
    format!("{DIRECT_SINK_PREFIX}-{}-{seq}", std::process::id())
}

pub(crate) fn acquire_audio_rt_guard() -> Option<RealtimePriorityGuard> {
    match RealtimePriorityGuard::acquire(PriorityProfile::Audio) {
        Ok(guard) => {
            log_rt_outcome(guard.outcome());
            Some(guard)
        }
        Err(RtError::PlatformDenied(errno)) => {
            eprintln!(
                "[fluxer-linux-audio] RT priority denied (errno={errno}); continuing without elevation",
            );
            None
        }
        Err(other) => {
            eprintln!("[fluxer-linux-audio] RT priority error: {other}");
            None
        }
    }
}

fn log_rt_outcome(outcome: &RtOutcome) {
    match outcome {
        RtOutcome::Acquired => {}
        RtOutcome::PartialFallback => {
            eprintln!(
                "[fluxer-linux-audio] RT priority partial fallback engaged (Linux EPERM path)",
            );
        }
    }
}

#[derive(Default, Clone)]
pub(crate) struct PortRecord {
    pub(crate) node_id: u32,
    pub(crate) direction: String,
    pub(crate) channel: String,
    pub(crate) props: PropMap,
}

#[derive(Default)]
pub(crate) struct InventorySnapshot {
    pub(crate) nodes: HashMap<u32, PropMap>,
    pub(crate) clients: HashMap<u32, PropMap>,
    pub(crate) ports: HashMap<u32, PortRecord>,
}

impl InventorySnapshot {
    pub(crate) fn enriched_node_props(&self, props: &PropMap) -> PropMap {
        let mut enriched = props
            .get("client.id")
            .and_then(|client_id| client_id.parse::<u32>().ok())
            .and_then(|client_id| self.clients.get(&client_id))
            .map(client_identity_props)
            .unwrap_or_default();

        for (key, value) in props {
            enriched.insert(key.clone(), value.clone());
        }

        if !enriched.contains_key("application.process.id")
            && let Some(pid) = enriched.get("pipewire.sec.pid").cloned()
        {
            enriched.insert("application.process.id".to_string(), pid);
        }

        enriched
    }

    pub(crate) fn enriched_nodes(&self) -> HashMap<u32, PropMap> {
        self.nodes
            .iter()
            .map(|(id, props)| (*id, self.enriched_node_props(props)))
            .collect()
    }

    pub(crate) fn enriched_node_values(&self) -> Vec<PropMap> {
        self.nodes
            .values()
            .map(|props| self.enriched_node_props(props))
            .collect()
    }
}

fn client_identity_props(client: &PropMap) -> PropMap {
    client
        .iter()
        .filter(|(key, _)| is_client_identity_key(key))
        .map(|(key, value)| (key.clone(), value.clone()))
        .collect()
}

fn is_client_identity_key(key: &str) -> bool {
    key.starts_with("application.") || key.starts_with("pipewire.sec.")
}

pub(crate) struct MetadataWatch {
    pub(crate) metadata: Metadata,
    pub(crate) is_default: bool,
    pub(crate) _listener: MetadataListener,
}

pub(crate) fn collect_props(dict: Option<&pw::spa::utils::dict::DictRef>) -> PropMap {
    let mut props = PropMap::new();
    if let Some(d) = dict {
        for (k, v) in d.iter() {
            props.insert(k.to_string(), v.to_string());
        }
    }
    props
}

pub(crate) fn is_routable_media_class(class: &str) -> bool {
    matches!(
        class,
        crate::routing::MEDIA_CLASS_PLAYBACK_STREAM
            | MEDIA_CLASS_CAPTURE_STREAM
            | "Audio/Source"
            | "Audio/Sink"
    )
}

#[cfg(test)]
pub(crate) fn build_virtual_sink_props() -> PropertiesBox {
    build_virtual_sink_props_for(
        SINK_NODE_NAME,
        SINK_NODE_DESCRIPTION,
        VirtualSinkKind::LegacyVirtualSource,
    )
}

pub(crate) fn build_virtual_sink_props_for(
    node_name: &str,
    description: &str,
    kind: VirtualSinkKind,
) -> PropertiesBox {
    let media_class = match kind {
        VirtualSinkKind::LegacyVirtualSource => "Audio/Source/Virtual",
        VirtualSinkKind::PrivateAudioSink => "Audio/Sink",
    };
    let mut props = properties! {
        "factory.name" => "support.null-audio-sink",
        "node.name" => node_name,
        "node.nick" => node_name,
        "node.description" => description,
        "media.class" => media_class,
        "node.virtual" => "true",
        "node.passive" => "true",
        "node.dont-move" => "true",
        "node.dont-reconnect" => "true",
        "node.latency" => audio_contract::direct_capture_latency_fraction(),
        "audio.rate" => DIRECT_CAPTURE_SAMPLE_RATE.to_string(),
        "audio.position" => "[FL,FR]",
        "monitor.channel-volumes" => "true",
    };
    if matches!(kind, VirtualSinkKind::PrivateAudioSink) {
        props.insert("node.hidden", "true");
    }
    props.insert("audio.channels", "2");
    props
}

pub(crate) fn build_link_props(
    src_node: u32,
    src_port: u32,
    sink_node: u32,
    sink_port: u32,
) -> PropertiesBox {
    properties! {
        "object.linger" => "false",
        "link.passive" => "true",
        *keys::LINK_OUTPUT_NODE => src_node.to_string(),
        *keys::LINK_OUTPUT_PORT => src_port.to_string(),
        *keys::LINK_INPUT_NODE => sink_node.to_string(),
        *keys::LINK_INPUT_PORT => sink_port.to_string(),
    }
}

pub(crate) fn pick_node_ports(
    node_id: u32,
    direction: &str,
    ports: &HashMap<u32, PortRecord>,
) -> Option<(u32, u32)> {
    let mut fl = None;
    let mut fr = None;
    let mut mono = None;
    let mut candidates = Vec::new();
    for (port_id, rec) in ports.iter() {
        if rec.node_id != node_id || rec.direction != direction {
            continue;
        }
        candidates.push(*port_id);
        match rec.channel.to_ascii_uppercase().as_str() {
            CH_FRONT_LEFT => fl = Some(*port_id),
            CH_FRONT_RIGHT => fr = Some(*port_id),
            "" | CH_MONO => mono = Some(*port_id),
            _ => {}
        }
    }
    if let (Some(l), Some(r)) = (fl, fr) {
        return Some((l, r));
    }
    candidates.sort_unstable();
    if candidates.len() >= 2 {
        return Some((candidates[0], candidates[1]));
    }
    mono.map(|m| (m, m))
}

pub(crate) fn pick_source_output_ports(
    node_id: u32,
    ports: &HashMap<u32, PortRecord>,
) -> Option<(u32, u32)> {
    pick_node_ports(node_id, "out", ports)
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct LinkKey {
    pub(crate) src_node: u32,
    pub(crate) src_port: u32,
    pub(crate) sink_node: u32,
    pub(crate) sink_port: u32,
}

impl LinkKey {
    pub(crate) fn new(src_node: u32, src_port: u32, sink_node: u32, sink_port: u32) -> Self {
        Self {
            src_node,
            src_port,
            sink_node,
            sink_port,
        }
    }
}

pub(crate) struct OwnedLink {
    pub(crate) key: LinkKey,
    pub(crate) link: pw::link::Link,
}

pub(crate) fn create_link(core: &pw::core::CoreRc, key: LinkKey) -> Option<OwnedLink> {
    let props = build_link_props(key.src_node, key.src_port, key.sink_node, key.sink_port);
    let link = core
        .create_object::<pw::link::Link>("link-factory", &props)
        .ok()?;
    Some(OwnedLink { key, link })
}

pub(crate) fn destroy_owned_links(
    core: &pw::core::CoreRc,
    owned_links: &std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    owned_link_snapshot: &Arc<Mutex<Vec<LinkKey>>>,
) {
    let links = std::mem::take(&mut *owned_links.borrow_mut());
    for owned in links {
        let link = owned.link;
        let _ = core.destroy_object(link);
    }
    replace_owned_link_snapshot(owned_link_snapshot, Vec::new());
}

pub(crate) fn sync_owned_links(
    core: &pw::core::CoreRc,
    owned_links: &std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    owned_link_snapshot: &Arc<Mutex<Vec<LinkKey>>>,
    desired_links: Vec<LinkKey>,
) {
    let desired: HashSet<LinkKey> = desired_links.into_iter().collect();
    let mut links = owned_links.borrow_mut();
    let mut index = 0;
    while index < links.len() {
        if desired.contains(&links[index].key) {
            index += 1;
            continue;
        }
        let removed = links.swap_remove(index);
        let _ = core.destroy_object(removed.link);
    }
    let existing: HashSet<LinkKey> = links.iter().map(|owned| owned.key).collect();
    for key in desired {
        if existing.contains(&key) {
            continue;
        }
        if let Some(link) = create_link(core, key) {
            links.push(link);
        }
    }
    let keys = links.iter().map(|owned| owned.key).collect();
    replace_owned_link_snapshot(owned_link_snapshot, keys);
}

pub(crate) fn replace_owned_link_snapshot(
    owned_link_snapshot: &Arc<Mutex<Vec<LinkKey>>>,
    mut keys: Vec<LinkKey>,
) {
    keys.sort_by_key(|key| (key.src_node, key.src_port, key.sink_node, key.sink_port));
    if let Ok(mut guard) = owned_link_snapshot.lock() {
        *guard = keys;
    }
}

pub(crate) fn ensure_virtual_sink(
    core: &pw::core::CoreRc,
    sink_proxy: &std::rc::Rc<std::cell::RefCell<Option<pw::node::Node>>>,
    node_name: &str,
    description: &str,
    kind: VirtualSinkKind,
) {
    if sink_proxy.borrow().is_some() {
        return;
    }
    let props = build_virtual_sink_props_for(node_name, description, kind);
    if let Ok(node) = core.create_object::<pw::node::Node>("adapter", &props) {
        *sink_proxy.borrow_mut() = Some(node);
    }
}

pub(crate) fn daemon_reachable() -> bool {
    pw::init();
    let Ok(mainloop) = pw::main_loop::MainLoopRc::new(None) else {
        return false;
    };
    let Ok(context) = pw::context::ContextRc::new(&mainloop, None) else {
        return false;
    };
    context.connect_rc(None).is_ok()
}
