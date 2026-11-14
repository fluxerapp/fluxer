// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::sync::{Arc, Mutex};

use pipewire as pw;
use pw::channel::Receiver as PwReceiver;
use pw::context::ContextRc;
use pw::main_loop::MainLoopRc;

use fluxer_rt_thread::MonotonicClock;

use crate::direct_buffer::DirectAudioBuffer;
use crate::routing::{RoutingRule, SelfIdentity};

use super::common::{
    DIRECT_SINK_DESCRIPTION, InventorySnapshot, LinkKey, MetadataWatch, OwnedLink,
    SINK_NODE_DESCRIPTION, SINK_NODE_NAME, VirtualSinkKind, acquire_audio_rt_guard,
    destroy_owned_links, ensure_virtual_sink,
};
use super::device_enum::{
    DirectGlobalAddedArgs, GlobalAddedContext, handle_direct_global_added,
    handle_direct_global_removed, handle_global_added, handle_global_removed,
};
use super::routing::{
    DirectRoutingState, RoutingState, clear_pinned_capture_targets, recompute_direct_links,
    recompute_routing, refresh_direct_sink_input_ports,
};
use super::stream_ops::{BuildDirectStreamArgs, DirectStreamRuntime, build_direct_stream};

pub(crate) enum BridgeCommand {
    Apply(RoutingRule),
    Release,
    SetIdentity(SelfIdentity),
    Shutdown,
}

struct BridgeWorkerState {
    snapshot: Arc<Mutex<InventorySnapshot>>,
    owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    state: std::rc::Rc<std::cell::RefCell<RoutingState>>,
    sink_proxy: std::rc::Rc<std::cell::RefCell<Option<pw::node::Node>>>,
    owned_links: std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    metadata_watchers: std::rc::Rc<std::cell::RefCell<Vec<MetadataWatch>>>,
}

fn init_bridge_state(
    snapshot: Arc<Mutex<InventorySnapshot>>,
    owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
) -> BridgeWorkerState {
    BridgeWorkerState {
        snapshot,
        owned_link_snapshot,
        state: std::rc::Rc::new(std::cell::RefCell::new(RoutingState::default())),
        sink_proxy: std::rc::Rc::new(std::cell::RefCell::new(None)),
        owned_links: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
        metadata_watchers: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
    }
}

fn install_bridge_registry(
    registry: &pw::registry::RegistryRc,
    core: &pw::core::CoreRc,
    worker: &BridgeWorkerState,
) -> pw::registry::Listener {
    let inv_for_global = worker.snapshot.clone();
    let link_snapshot_for_global = worker.owned_link_snapshot.clone();
    let state_for_global = worker.state.clone();
    let core_for_global = core.clone();
    let registry_for_global = registry.clone();
    let owned_for_global = worker.owned_links.clone();
    let metadata_for_global = worker.metadata_watchers.clone();
    let inv_rm = worker.snapshot.clone();
    let state_rm = worker.state.clone();
    let core_rm = core.clone();
    let owned_rm = worker.owned_links.clone();
    let link_snapshot_rm = worker.owned_link_snapshot.clone();
    let metadata_rm = worker.metadata_watchers.clone();
    registry
        .add_listener_local()
        .global(move |obj| {
            handle_global_added(
                obj,
                GlobalAddedContext {
                    registry: &registry_for_global,
                    inventory: &inv_for_global,
                    state: &state_for_global,
                    core: &core_for_global,
                    owned_links: &owned_for_global,
                    owned_link_snapshot: &link_snapshot_for_global,
                    metadata_watchers: &metadata_for_global,
                    sink_node_name: SINK_NODE_NAME,
                },
            );
        })
        .global_remove(move |id| {
            let changed = handle_global_removed(id, &inv_rm, &state_rm);
            if changed && state_rm.borrow().active_rule.is_some() {
                recompute_routing(
                    &inv_rm,
                    &state_rm,
                    &core_rm,
                    &owned_rm,
                    &link_snapshot_rm,
                    &metadata_rm,
                    SINK_NODE_NAME,
                );
            }
        })
        .register()
}

fn install_bridge_command_handler<'a>(
    mainloop: &'a MainLoopRc,
    rx: PwReceiver<BridgeCommand>,
    core: &pw::core::CoreRc,
    worker: &BridgeWorkerState,
) -> pw::channel::AttachedReceiver<'a, BridgeCommand> {
    let inv_for_cmd = worker.snapshot.clone();
    let link_snapshot_for_cmd = worker.owned_link_snapshot.clone();
    let state_for_cmd = worker.state.clone();
    let core_for_cmd = core.clone();
    let owned_for_cmd = worker.owned_links.clone();
    let metadata_for_cmd = worker.metadata_watchers.clone();
    let sink_for_cmd = worker.sink_proxy.clone();
    let mainloop_weak = mainloop.downgrade();
    rx.attach(mainloop.loop_(), move |cmd| match cmd {
        BridgeCommand::Apply(rule) => {
            state_for_cmd.borrow_mut().active_rule = Some(rule);
            ensure_virtual_sink(
                &core_for_cmd,
                &sink_for_cmd,
                SINK_NODE_NAME,
                SINK_NODE_DESCRIPTION,
                VirtualSinkKind::LegacyVirtualSource,
            );
            recompute_routing(
                &inv_for_cmd,
                &state_for_cmd,
                &core_for_cmd,
                &owned_for_cmd,
                &link_snapshot_for_cmd,
                &metadata_for_cmd,
                SINK_NODE_NAME,
            );
        }
        BridgeCommand::Release => {
            clear_pinned_capture_targets(&state_for_cmd, &metadata_for_cmd);
            state_for_cmd.borrow_mut().active_rule = None;
            destroy_owned_links(&core_for_cmd, &owned_for_cmd, &link_snapshot_for_cmd);
        }
        BridgeCommand::SetIdentity(id) => {
            state_for_cmd.borrow_mut().identity = id;
        }
        BridgeCommand::Shutdown => {
            clear_pinned_capture_targets(&state_for_cmd, &metadata_for_cmd);
            destroy_owned_links(&core_for_cmd, &owned_for_cmd, &link_snapshot_for_cmd);
            sink_for_cmd.borrow_mut().take();
            if let Some(ml) = mainloop_weak.upgrade() {
                ml.quit();
            }
        }
    })
}

pub(crate) fn run_bridge_worker(
    snapshot: Arc<Mutex<InventorySnapshot>>,
    owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    rx: PwReceiver<BridgeCommand>,
    ready_tx: std::sync::mpsc::SyncSender<bool>,
) {
    let _rt_guard = acquire_audio_rt_guard();
    pw::init();
    let Ok(mainloop) = MainLoopRc::new(None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(context) = ContextRc::new(&mainloop, None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(core) = context.connect_rc(None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(registry) = core.get_registry_rc() else {
        let _ = ready_tx.send(false);
        return;
    };

    let worker = init_bridge_state(snapshot, owned_link_snapshot);
    let _registry_listener = install_bridge_registry(&registry, &core, &worker);
    let _attached_rx = install_bridge_command_handler(&mainloop, rx, &core, &worker);

    let _ = ready_tx.send(true);
    mainloop.run();
}

pub(crate) enum DirectCommand {
    Start {
        rule: RoutingRule,
        identity: Box<SelfIdentity>,
    },
    UpdateRule {
        rule: RoutingRule,
    },
    Stop,
    Shutdown,
}

#[derive(Clone, Copy)]
pub(crate) enum DirectSinkRetention {
    Preserve,
    Drop,
}

pub(crate) fn stop_direct_streams(
    core: &pw::core::CoreRc,
    runtime: &DirectStreamRuntime,
    sink_retention: DirectSinkRetention,
) {
    *runtime.active_listener.borrow_mut() = None;
    *runtime.active_stream.borrow_mut() = None;
    destroy_owned_links(core, &runtime.owned_links, &runtime.owned_link_snapshot);
    if matches!(sink_retention, DirectSinkRetention::Drop) {
        runtime.sink_proxy.borrow_mut().take();
    }
}

pub(crate) fn clear_direct_samples(samples: &Arc<Mutex<DirectAudioBuffer>>) {
    if let Ok(mut guard) = samples.lock() {
        guard.clear();
    }
}

pub(crate) fn recompute_direct_streams(
    core: &pw::core::CoreRc,
    inventory: &Arc<Mutex<InventorySnapshot>>,
    state: &std::rc::Rc<std::cell::RefCell<DirectRoutingState>>,
    runtime: &DirectStreamRuntime,
) {
    if state.borrow().active_rule.is_none() {
        stop_direct_streams(core, runtime, DirectSinkRetention::Preserve);
        return;
    }
    let updated = recompute_direct_links(
        core,
        inventory,
        state,
        &runtime.owned_links,
        &runtime.owned_link_snapshot,
    );
    if !updated {
        return;
    }
    ensure_or_promote_direct_stream(core, runtime);
}

fn ensure_or_promote_direct_stream(core: &pw::core::CoreRc, runtime: &DirectStreamRuntime) {
    if runtime.active_stream.borrow().is_some() {
        return;
    }
    let args = BuildDirectStreamArgs {
        core,
        samples: runtime.samples.clone(),
        target_sink_name: &runtime.sink_node_name,
        stream_node_name: &runtime.stream_node_name,
        clock: runtime.clock.clone(),
    };
    if let Ok((stream, listener)) = build_direct_stream(args) {
        *runtime.active_stream.borrow_mut() = Some(stream);
        *runtime.active_listener.borrow_mut() = Some(listener);
    }
}

pub(crate) struct DirectWorkerInputs {
    pub(crate) samples: Arc<Mutex<DirectAudioBuffer>>,
    pub(crate) inventory: Arc<Mutex<InventorySnapshot>>,
    pub(crate) owned_link_snapshot: Arc<Mutex<Vec<LinkKey>>>,
    pub(crate) sink_node_name: String,
    pub(crate) clock: Arc<dyn MonotonicClock>,
}

fn build_direct_runtime(inputs: &DirectWorkerInputs) -> std::rc::Rc<DirectStreamRuntime> {
    let stream_node_name = format!("{}-stream", inputs.sink_node_name);
    std::rc::Rc::new(DirectStreamRuntime {
        active_stream: std::rc::Rc::new(std::cell::RefCell::new(None)),
        active_listener: std::rc::Rc::new(std::cell::RefCell::new(None)),
        owned_links: std::rc::Rc::new(std::cell::RefCell::new(Vec::new())),
        owned_link_snapshot: inputs.owned_link_snapshot.clone(),
        sink_proxy: std::rc::Rc::new(std::cell::RefCell::new(None)),
        samples: inputs.samples.clone(),
        sink_node_name: inputs.sink_node_name.clone(),
        stream_node_name,
        clock: inputs.clock.clone(),
    })
}

fn install_direct_registry(
    registry: &pw::registry::RegistryRc,
    core: &pw::core::CoreRc,
    inventory: &Arc<Mutex<InventorySnapshot>>,
    state: &std::rc::Rc<std::cell::RefCell<DirectRoutingState>>,
    runtime: &std::rc::Rc<DirectStreamRuntime>,
    metadata_watchers: &std::rc::Rc<std::cell::RefCell<Vec<MetadataWatch>>>,
) -> pw::registry::Listener {
    let inv_added = inventory.clone();
    let state_added = state.clone();
    let core_added = core.clone();
    let runtime_added = runtime.clone();
    let registry_added = registry.clone();
    let metadata_added = metadata_watchers.clone();
    let inv_rm = inventory.clone();
    let state_rm = state.clone();
    let core_rm = core.clone();
    let runtime_rm = runtime.clone();
    registry
        .add_listener_local()
        .global(move |obj| {
            let inv_for_cb = inv_added.clone();
            let state_for_cb = state_added.clone();
            let core_for_cb = core_added.clone();
            let runtime_for_cb = runtime_added.clone();
            let on_default_change = move || {
                recompute_direct_streams(&core_for_cb, &inv_for_cb, &state_for_cb, &runtime_for_cb);
            };
            let changed = handle_direct_global_added(
                obj,
                DirectGlobalAddedArgs {
                    registry: &registry_added,
                    inventory: &inv_added,
                    state: &state_added,
                    core: &core_added,
                    runtime: &runtime_added,
                    metadata_watchers: &metadata_added,
                },
                on_default_change,
            );
            if changed {
                recompute_direct_streams(&core_added, &inv_added, &state_added, &runtime_added);
            }
        })
        .global_remove(move |id| {
            let changed = handle_direct_global_removed(id, &inv_rm, &state_rm, &runtime_rm);
            if changed {
                recompute_direct_streams(&core_rm, &inv_rm, &state_rm, &runtime_rm);
            }
        })
        .register()
}

fn install_direct_command_handler<'a>(
    mainloop: &'a MainLoopRc,
    rx: PwReceiver<DirectCommand>,
    core: &pw::core::CoreRc,
    inventory: &Arc<Mutex<InventorySnapshot>>,
    state: &std::rc::Rc<std::cell::RefCell<DirectRoutingState>>,
    runtime: &std::rc::Rc<DirectStreamRuntime>,
) -> pw::channel::AttachedReceiver<'a, DirectCommand> {
    let core_for_cmd = core.clone();
    let inv_for_cmd = inventory.clone();
    let state_for_cmd = state.clone();
    let runtime_for_cmd = runtime.clone();
    let mainloop_weak = mainloop.downgrade();
    rx.attach(mainloop.loop_(), move |cmd| match cmd {
        DirectCommand::Start { rule, identity } => {
            clear_direct_samples(&runtime_for_cmd.samples);
            {
                let mut st = state_for_cmd.borrow_mut();
                st.identity = *identity;
                st.active_rule = Some(rule);
            }
            ensure_virtual_sink(
                &core_for_cmd,
                &runtime_for_cmd.sink_proxy,
                &runtime_for_cmd.sink_node_name,
                DIRECT_SINK_DESCRIPTION,
                VirtualSinkKind::PrivateAudioSink,
            );
            refresh_direct_sink_input_ports(&inv_for_cmd, &state_for_cmd);
            recompute_direct_streams(
                &core_for_cmd,
                &inv_for_cmd,
                &state_for_cmd,
                &runtime_for_cmd,
            );
        }
        DirectCommand::UpdateRule { rule } => {
            let active = state_for_cmd.borrow().active_rule.is_some();
            if !active {
                return;
            }
            state_for_cmd.borrow_mut().active_rule = Some(rule);
            recompute_direct_streams(
                &core_for_cmd,
                &inv_for_cmd,
                &state_for_cmd,
                &runtime_for_cmd,
            );
        }
        DirectCommand::Stop => {
            stop_direct_streams(
                &core_for_cmd,
                &runtime_for_cmd,
                DirectSinkRetention::Preserve,
            );
            {
                let mut st = state_for_cmd.borrow_mut();
                st.active_rule = None;
            }
            clear_direct_samples(&runtime_for_cmd.samples);
        }
        DirectCommand::Shutdown => {
            stop_direct_streams(&core_for_cmd, &runtime_for_cmd, DirectSinkRetention::Drop);
            clear_direct_samples(&runtime_for_cmd.samples);
            if let Some(ml) = mainloop_weak.upgrade() {
                ml.quit();
            }
        }
    })
}

pub(crate) fn run_direct_worker(
    inputs: DirectWorkerInputs,
    rx: PwReceiver<DirectCommand>,
    ready_tx: std::sync::mpsc::SyncSender<bool>,
) {
    let _rt_guard = acquire_audio_rt_guard();
    pw::init();
    let Ok(mainloop) = MainLoopRc::new(None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(context) = ContextRc::new(&mainloop, None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(core) = context.connect_rc(None) else {
        let _ = ready_tx.send(false);
        return;
    };
    let Ok(registry) = core.get_registry_rc() else {
        let _ = ready_tx.send(false);
        return;
    };

    let state: std::rc::Rc<std::cell::RefCell<DirectRoutingState>> =
        std::rc::Rc::new(std::cell::RefCell::new(DirectRoutingState::default()));
    let metadata_watchers: std::rc::Rc<std::cell::RefCell<Vec<MetadataWatch>>> =
        std::rc::Rc::new(std::cell::RefCell::new(Vec::new()));
    let runtime = build_direct_runtime(&inputs);
    let _registry_listener = install_direct_registry(
        &registry,
        &core,
        &inputs.inventory,
        &state,
        &runtime,
        &metadata_watchers,
    );
    let _attached_rx =
        install_direct_command_handler(&mainloop, rx, &core, &inputs.inventory, &state, &runtime);

    let _ = ready_tx.send(true);
    mainloop.run();
}
