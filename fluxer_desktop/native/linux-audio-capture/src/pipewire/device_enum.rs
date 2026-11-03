// SPDX-License-Identifier: AGPL-3.0-or-later

#![allow(dead_code)]

use std::sync::{Arc, Mutex};

use pipewire as pw;
use pw::metadata::Metadata;
use pw::types::ObjectType;

use super::common::{
    InventorySnapshot, LinkKey, MetadataWatch, OwnedLink, PortRecord, collect_props,
    is_routable_media_class, replace_owned_link_snapshot,
};
use super::routing::{
    DirectRoutingState, RoutingState, recompute_routing, refresh_direct_sink_input_ports,
    refresh_sink_input_ports,
};
use super::stream_ops::DirectStreamRuntime;

pub(crate) struct GlobalAddedContext<'a> {
    pub(crate) registry: &'a pw::registry::RegistryRc,
    pub(crate) inventory: &'a Arc<Mutex<InventorySnapshot>>,
    pub(crate) state: &'a std::rc::Rc<std::cell::RefCell<RoutingState>>,
    pub(crate) core: &'a pw::core::CoreRc,
    pub(crate) owned_links: &'a std::rc::Rc<std::cell::RefCell<Vec<OwnedLink>>>,
    pub(crate) owned_link_snapshot: &'a Arc<Mutex<Vec<LinkKey>>>,
    pub(crate) metadata_watchers: &'a std::rc::Rc<std::cell::RefCell<Vec<MetadataWatch>>>,
    pub(crate) sink_node_name: &'a str,
}

fn handle_global_added_client(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    ctx: &GlobalAddedContext<'_>,
) {
    let props = collect_props(obj.props);
    let Ok(mut snap) = ctx.inventory.lock() else {
        return;
    };
    snap.clients.insert(obj.id, props);
    drop(snap);
    if ctx.state.borrow().active_rule.is_some() {
        recompute_routing(
            ctx.inventory,
            ctx.state,
            ctx.core,
            ctx.owned_links,
            ctx.owned_link_snapshot,
            ctx.metadata_watchers,
            ctx.sink_node_name,
        );
    }
}

fn handle_global_added_node(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    ctx: &GlobalAddedContext<'_>,
) {
    let props = collect_props(obj.props);
    let class = props.get("media.class").cloned().unwrap_or_default();
    let node_name = props.get("node.name").cloned().unwrap_or_default();
    let is_our_sink = node_name == ctx.sink_node_name;
    if !is_our_sink && !is_routable_media_class(&class) {
        return;
    }
    let Ok(mut snap) = ctx.inventory.lock() else {
        return;
    };
    snap.nodes.insert(obj.id, props);
    drop(snap);
    if is_our_sink {
        ctx.state.borrow_mut().sink_global_id = obj.id;
        refresh_sink_input_ports(ctx.inventory, ctx.state);
        recompute_routing(
            ctx.inventory,
            ctx.state,
            ctx.core,
            ctx.owned_links,
            ctx.owned_link_snapshot,
            ctx.metadata_watchers,
            ctx.sink_node_name,
        );
        return;
    }
    if ctx.state.borrow().active_rule.is_some() {
        recompute_routing(
            ctx.inventory,
            ctx.state,
            ctx.core,
            ctx.owned_links,
            ctx.owned_link_snapshot,
            ctx.metadata_watchers,
            ctx.sink_node_name,
        );
    }
}

fn build_port_record(props: &crate::routing::PropMap) -> Option<PortRecord> {
    let node_id = props.get("node.id").and_then(|s| s.parse::<u32>().ok())?;
    if node_id == 0 {
        return None;
    }
    let direction = props
        .get("port.direction")
        .map(String::as_str)
        .unwrap_or("")
        .to_ascii_lowercase();
    let channel = props.get("audio.channel").cloned().unwrap_or_default();
    Some(PortRecord {
        node_id,
        direction,
        channel,
        props: props.clone(),
    })
}

fn handle_global_added_port(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    ctx: &GlobalAddedContext<'_>,
) {
    let props = collect_props(obj.props);
    let Some(record) = build_port_record(&props) else {
        return;
    };
    let node_id = record.node_id;
    let Ok(mut snap) = ctx.inventory.lock() else {
        return;
    };
    snap.ports.insert(obj.id, record);
    drop(snap);
    let sink_id = ctx.state.borrow().sink_global_id;
    if sink_id != 0 && node_id == sink_id {
        refresh_sink_input_ports(ctx.inventory, ctx.state);
    }
    if ctx.state.borrow().active_rule.is_some() {
        recompute_routing(
            ctx.inventory,
            ctx.state,
            ctx.core,
            ctx.owned_links,
            ctx.owned_link_snapshot,
            ctx.metadata_watchers,
            ctx.sink_node_name,
        );
    }
}

fn handle_global_added_metadata(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    ctx: &GlobalAddedContext<'_>,
) {
    let metadata_name = obj
        .props
        .and_then(|dict| dict.get("metadata.name"))
        .unwrap_or("")
        .to_string();
    let is_default = metadata_name == "default";
    let Ok(metadata) = ctx.registry.bind::<Metadata, _>(obj) else {
        return;
    };
    let inv = ctx.inventory.clone();
    let st = ctx.state.clone();
    let core_for_listener = ctx.core.clone();
    let owned_for_listener = ctx.owned_links.clone();
    let link_snapshot_for_listener = ctx.owned_link_snapshot.clone();
    let metadata_watchers_for_listener = ctx.metadata_watchers.clone();
    let sink_node_name_owned = ctx.sink_node_name.to_string();
    let listener = metadata
        .add_listener_local()
        .property(move |_subject, key, _type_, value| {
            if key == Some("default.audio.sink") {
                let name = value
                    .map(crate::routing::parse_default_sink_name)
                    .unwrap_or_default();
                st.borrow_mut().default_sink_name = name;
                recompute_routing(
                    &inv,
                    &st,
                    &core_for_listener,
                    &owned_for_listener,
                    &link_snapshot_for_listener,
                    &metadata_watchers_for_listener,
                    &sink_node_name_owned,
                );
            }
            0
        })
        .register();
    ctx.metadata_watchers.borrow_mut().push(MetadataWatch {
        metadata,
        is_default,
        _listener: listener,
    });
    if ctx.state.borrow().active_rule.is_some() {
        recompute_routing(
            ctx.inventory,
            ctx.state,
            ctx.core,
            ctx.owned_links,
            ctx.owned_link_snapshot,
            ctx.metadata_watchers,
            ctx.sink_node_name,
        );
    }
}

pub(crate) fn handle_global_added(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    ctx: GlobalAddedContext<'_>,
) {
    match obj.type_ {
        ObjectType::Client => handle_global_added_client(obj, &ctx),
        ObjectType::Node => handle_global_added_node(obj, &ctx),
        ObjectType::Port => handle_global_added_port(obj, &ctx),
        ObjectType::Metadata => handle_global_added_metadata(obj, &ctx),
        _ => {}
    }
}

pub(crate) fn handle_global_removed(
    id: u32,
    inventory: &Arc<Mutex<InventorySnapshot>>,
    state: &std::rc::Rc<std::cell::RefCell<RoutingState>>,
) -> bool {
    let mut changed = false;
    if let Ok(mut snap) = inventory.lock() {
        changed |= snap.nodes.remove(&id).is_some();
        changed |= snap.clients.remove(&id).is_some();
        changed |= snap.ports.remove(&id).is_some();
    }
    let mut st = state.borrow_mut();
    let removed_sink = st.sink_global_id == id;
    if st.sink_global_id == id {
        st.sink_global_id = 0;
        st.sink_input_fl = None;
        st.sink_input_fr = None;
    }
    if st.sink_input_fl == Some(id) {
        st.sink_input_fl = None;
    }
    if st.sink_input_fr == Some(id) {
        st.sink_input_fr = None;
    }
    changed |= st.pinned_capture_nodes.remove(&id);
    changed || removed_sink
}

pub(crate) struct DirectGlobalAddedArgs<'a> {
    pub(crate) registry: &'a pw::registry::RegistryRc,
    pub(crate) inventory: &'a Arc<Mutex<InventorySnapshot>>,
    pub(crate) state: &'a std::rc::Rc<std::cell::RefCell<DirectRoutingState>>,
    pub(crate) core: &'a pw::core::CoreRc,
    pub(crate) runtime: &'a DirectStreamRuntime,
    pub(crate) metadata_watchers: &'a std::rc::Rc<std::cell::RefCell<Vec<MetadataWatch>>>,
}

fn direct_added_client(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    args: &DirectGlobalAddedArgs<'_>,
) -> bool {
    let props = collect_props(obj.props);
    let Ok(mut snap) = args.inventory.lock() else {
        return false;
    };
    snap.clients.insert(obj.id, props);
    true
}

fn direct_added_node(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    args: &DirectGlobalAddedArgs<'_>,
) -> bool {
    let props = collect_props(obj.props);
    let class = props.get("media.class").cloned().unwrap_or_default();
    let node_name = props.get("node.name").cloned().unwrap_or_default();
    let is_our_sink = node_name == args.runtime.sink_node_name;
    if !is_our_sink && !is_routable_media_class(&class) {
        return false;
    }
    {
        let Ok(mut snap) = args.inventory.lock() else {
            return false;
        };
        snap.nodes.insert(obj.id, props);
    }
    if is_our_sink {
        args.state.borrow_mut().sink_global_id = obj.id;
        refresh_direct_sink_input_ports(args.inventory, args.state);
    }
    true
}

fn direct_added_port(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    args: &DirectGlobalAddedArgs<'_>,
) -> bool {
    let props = collect_props(obj.props);
    let Some(record) = build_port_record(&props) else {
        return false;
    };
    let node_id = record.node_id;
    {
        let Ok(mut snap) = args.inventory.lock() else {
            return false;
        };
        snap.ports.insert(obj.id, record);
    }
    let sink_id = args.state.borrow().sink_global_id;
    if sink_id != 0 && node_id == sink_id {
        refresh_direct_sink_input_ports(args.inventory, args.state);
    }
    true
}

fn direct_added_metadata(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    args: &DirectGlobalAddedArgs<'_>,
    on_default_change: impl Fn() + 'static,
) -> bool {
    let metadata_name = obj
        .props
        .and_then(|dict| dict.get("metadata.name"))
        .unwrap_or("")
        .to_string();
    let is_default = metadata_name == "default";
    let Ok(metadata) = args.registry.bind::<Metadata, _>(obj) else {
        return false;
    };
    let st = args.state.clone();
    let listener = metadata
        .add_listener_local()
        .property(move |_subject, key, _type_, value| {
            if key == Some("default.audio.sink") {
                let name = value
                    .map(crate::routing::parse_default_sink_name)
                    .unwrap_or_default();
                st.borrow_mut().default_sink_name = name;
                on_default_change();
            }
            0
        })
        .register();
    args.metadata_watchers.borrow_mut().push(MetadataWatch {
        metadata,
        is_default,
        _listener: listener,
    });
    false
}

pub(crate) fn handle_direct_global_added(
    obj: &pw::registry::GlobalObject<&pw::spa::utils::dict::DictRef>,
    args: DirectGlobalAddedArgs<'_>,
    on_default_change: impl Fn() + 'static,
) -> bool {
    match obj.type_ {
        ObjectType::Client => direct_added_client(obj, &args),
        ObjectType::Node => direct_added_node(obj, &args),
        ObjectType::Port => direct_added_port(obj, &args),
        ObjectType::Metadata => direct_added_metadata(obj, &args, on_default_change),
        _ => false,
    }
}

pub(crate) fn handle_direct_global_removed(
    id: u32,
    inventory: &Arc<Mutex<InventorySnapshot>>,
    state: &std::rc::Rc<std::cell::RefCell<DirectRoutingState>>,
    runtime: &DirectStreamRuntime,
) -> bool {
    let mut changed = false;
    if let Ok(mut snap) = inventory.lock() {
        changed |= snap.nodes.remove(&id).is_some();
        changed |= snap.clients.remove(&id).is_some();
        changed |= snap.ports.remove(&id).is_some();
    }
    {
        let mut st = state.borrow_mut();
        if st.sink_global_id == id {
            st.sink_global_id = 0;
            st.sink_input_fl = None;
            st.sink_input_fr = None;
            runtime.sink_proxy.borrow_mut().take();
            runtime.owned_links.borrow_mut().clear();
            replace_owned_link_snapshot(&runtime.owned_link_snapshot, Vec::new());
            *runtime.active_stream.borrow_mut() = None;
            *runtime.active_listener.borrow_mut() = None;
            changed = true;
        }
        if st.sink_input_fl == Some(id) {
            st.sink_input_fl = None;
            changed = true;
        }
        if st.sink_input_fr == Some(id) {
            st.sink_input_fr = None;
            changed = true;
        }
    }
    changed
}
