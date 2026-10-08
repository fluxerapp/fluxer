// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    collections::{BTreeMap, BTreeSet},
    time::Duration,
};

pub const PORTAL_DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PORTAL_PATH: &str = "/org/freedesktop/portal/desktop";
pub const GLOBAL_SHORTCUTS_INTERFACE: &str = "org.freedesktop.portal.GlobalShortcuts";
pub const SESSION_INTERFACE: &str = "org.freedesktop.portal.Session";
pub const SESSION_PATH_NAMESPACE: &str = "/org/freedesktop/portal/desktop/session";
pub const REGISTRY_INTERFACE: &str = "org.freedesktop.host.portal.Registry";
pub const PROPERTIES_INTERFACE: &str = "org.freedesktop.DBus.Properties";
pub const BUS_NAME: &str = "org.freedesktop.DBus";
pub const BUS_PATH: &str = "/org/freedesktop/DBus";
pub const DEFAULT_SESSION_TOKEN: &str = "fluxer_global_shortcuts";
pub const SETUP_TIMEOUT: Duration = Duration::from_secs(30);
pub const TEARDOWN_TIMEOUT: Duration = Duration::from_secs(2);
pub const REGISTER_RETRY_DELAYS: [Duration; 3] = [
    Duration::from_millis(250),
    Duration::from_millis(750),
    Duration::from_secs(2),
];
pub const ESTABLISH_ATTEMPTS: usize = 3;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Desktop {
    Kde,
    Gnome,
    Hyprland,
    Other,
}

impl Desktop {
    pub fn parse(value: &str) -> Self {
        match value {
            "kde" => Self::Kde,
            "gnome" => Self::Gnome,
            "hyprland" => Self::Hyprland,
            _ => Self::Other,
        }
    }

    pub fn backend_name(self) -> Option<&'static str> {
        match self {
            Self::Kde => Some("org.freedesktop.impl.portal.desktop.kde"),
            Self::Gnome => Some("org.freedesktop.impl.portal.desktop.gnome"),
            Self::Hyprland => Some("org.freedesktop.impl.portal.desktop.hyprland"),
            Self::Other => None,
        }
    }
}

pub fn is_valid_token(token: &str) -> bool {
    !token.is_empty()
        && token
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
}

pub fn cgroup_is_snap(cgroups: &str) -> bool {
    cgroups.lines().any(|line| {
        let Some((hierarchy, rest)) = line.split_once(':') else {
            return false;
        };
        if hierarchy.parse::<u32>().is_err() {
            return false;
        }
        let unit = match rest.split_once(':') {
            Some(("" | "freezer" | "name=systemd", unit)) => unit,
            _ => return false,
        };
        unit.rsplit('/')
            .next()
            .is_some_and(|scope| scope.starts_with("snap."))
    })
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortcutDefinition {
    pub id: String,
    pub description: String,
    pub preferred_trigger: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DefinitionError {
    Empty,
    EmptyId,
    DuplicateId,
    EmptyDescription,
}

impl DefinitionError {
    pub fn message(self) -> &'static str {
        match self {
            Self::Empty => "shortcuts must not be empty",
            Self::EmptyId => "shortcut.id must not be empty",
            Self::DuplicateId => "shortcut ids must be unique",
            Self::EmptyDescription => "shortcut.description must not be empty",
        }
    }
}

pub fn validate_definitions(shortcuts: &[ShortcutDefinition]) -> Result<(), DefinitionError> {
    if shortcuts.is_empty() {
        return Err(DefinitionError::Empty);
    }
    let mut ids = BTreeSet::new();
    for shortcut in shortcuts {
        if shortcut.id.is_empty() {
            return Err(DefinitionError::EmptyId);
        }
        if shortcut.description.trim().is_empty() {
            return Err(DefinitionError::EmptyDescription);
        }
        if !ids.insert(shortcut.id.as_str()) {
            return Err(DefinitionError::DuplicateId);
        }
    }
    Ok(())
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShortcutBinding {
    pub id: String,
    pub description: Option<String>,
    pub trigger_description: Option<String>,
}

impl ShortcutBinding {
    pub fn new(
        id: String,
        description: Option<String>,
        trigger_description: Option<String>,
    ) -> Self {
        Self {
            id,
            description: description.filter(|value| !value.is_empty()),
            trigger_description: trigger_description.filter(|value| !value.is_empty()),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ShortcutTable {
    entries: BTreeMap<String, ShortcutBinding>,
}

impl ShortcutTable {
    pub fn from_definitions(shortcuts: &[ShortcutDefinition]) -> Self {
        Self {
            entries: shortcuts
                .iter()
                .map(|shortcut| {
                    (
                        shortcut.id.clone(),
                        ShortcutBinding::new(
                            shortcut.id.clone(),
                            Some(shortcut.description.clone()),
                            None,
                        ),
                    )
                })
                .collect(),
        }
    }

    pub fn from_listed(listed: &[ShortcutBinding]) -> Self {
        Self {
            entries: listed
                .iter()
                .map(|binding| (binding.id.clone(), binding.clone()))
                .collect(),
        }
    }

    pub fn contains(&self, id: &str) -> bool {
        self.entries.contains_key(id)
    }

    pub fn known(&self, incoming: Vec<ShortcutBinding>) -> Vec<ShortcutBinding> {
        incoming
            .into_iter()
            .filter(|binding| self.contains(&binding.id))
            .collect()
    }

    pub fn merge(&mut self, incoming: Vec<ShortcutBinding>) -> Vec<String> {
        let mut changed = Vec::new();
        for binding in incoming {
            let Some(entry) = self.entries.get_mut(&binding.id) else {
                continue;
            };
            if entry.trigger_description != binding.trigger_description {
                changed.push(binding.id.clone());
            }
            entry.trigger_description = binding.trigger_description;
            if binding.description.is_some() {
                entry.description = binding.description;
            }
        }
        changed
    }

    pub fn bindings(&self) -> Vec<ShortcutBinding> {
        self.entries.values().cloned().collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AppIdSource {
    Sandbox,
    Registered,
    Unregistered,
}

impl AppIdSource {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Sandbox => "sandbox",
            Self::Registered => "registered",
            Self::Unregistered => "unregistered",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenResult {
    pub version: u32,
    pub app_id_source: AppIdSource,
    pub unique_name: String,
    pub listed: Vec<ShortcutBinding>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindOutcome {
    Bound(Vec<ShortcutBinding>),
    Cancelled,
    Denied,
    Failed(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BindStep {
    Done(BindOutcome),
    List,
}

pub fn bind_step(code: u32, shortcuts: Vec<ShortcutBinding>) -> BindStep {
    if !shortcuts.is_empty() {
        return BindStep::Done(BindOutcome::Bound(shortcuts));
    }
    match code {
        0 => BindStep::List,
        1 => BindStep::Done(BindOutcome::Cancelled),
        2 => BindStep::Done(BindOutcome::Denied),
        other => BindStep::Done(BindOutcome::Failed(other)),
    }
}

pub fn listed_outcome(listed: Vec<ShortcutBinding>) -> BindOutcome {
    if listed.is_empty() {
        BindOutcome::Denied
    } else {
        BindOutcome::Bound(listed)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SessionLostReason {
    Closed,
    PortalRestarted,
    BusError,
}

impl SessionLostReason {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Closed => "closed",
            Self::PortalRestarted => "portal-restarted",
            Self::BusError => "bus-error",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortalEvent {
    Activated { id: String },
    Deactivated { id: String },
    ShortcutsChanged { shortcuts: Vec<ShortcutBinding> },
    SessionLost { reason: SessionLostReason },
    PortalAvailable,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PortalError {
    NoPortal,
    Interface,
    Version,
    Response(u32),
    AppIdRequired,
    Timeout,
    Bus,
    InvalidReply,
    NoSession,
    SessionLost,
    Closed,
}

impl std::fmt::Display for PortalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::NoPortal => f.write_str("unsupported:no-portal"),
            Self::Interface => f.write_str("unsupported:interface"),
            Self::Version => f.write_str("unsupported:version"),
            Self::Response(code) => write!(f, "unsupported:{code}"),
            Self::AppIdRequired => f.write_str("identity:app-id-required"),
            Self::Timeout => f.write_str("error:timeout"),
            Self::Bus => f.write_str("error:bus"),
            Self::InvalidReply => f.write_str("error:invalid-reply"),
            Self::NoSession => f.write_str("error:no-session"),
            Self::SessionLost => f.write_str("error:session-lost"),
            Self::Closed => f.write_str("error:closed"),
        }
    }
}

const UNKNOWN_METHOD: &str = "org.freedesktop.DBus.Error.UnknownMethod";
const UNKNOWN_INTERFACE: &str = "org.freedesktop.DBus.Error.UnknownInterface";
const UNKNOWN_PROPERTY: &str = "org.freedesktop.DBus.Error.UnknownProperty";
const INVALID_ARGS: &str = "org.freedesktop.DBus.Error.InvalidArgs";
const SERVICE_UNKNOWN: &str = "org.freedesktop.DBus.Error.ServiceUnknown";
const NAME_HAS_NO_OWNER: &str = "org.freedesktop.DBus.Error.NameHasNoOwner";
const NO_REPLY: &str = "org.freedesktop.DBus.Error.NoReply";
const TIMED_OUT: &str = "org.freedesktop.DBus.Error.TimedOut";
const PORTAL_FAILED: &str = "org.freedesktop.portal.Error.Failed";
const PORTAL_NOT_ALLOWED: &str = "org.freedesktop.portal.Error.NotAllowed";
const ALREADY_ASSOCIATED: &str = "already associated";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterFailure {
    Unregistered,
    AlreadyAssociated,
    Retry,
    Fatal(PortalError),
}

pub fn classify_register_error(name: &str, message: &str) -> RegisterFailure {
    match name {
        SERVICE_UNKNOWN | NAME_HAS_NO_OWNER | NO_REPLY | TIMED_OUT => {
            RegisterFailure::Fatal(PortalError::NoPortal)
        }
        PORTAL_FAILED if message.contains(ALREADY_ASSOCIATED) => RegisterFailure::AlreadyAssociated,
        PORTAL_FAILED => RegisterFailure::Retry,
        _ => RegisterFailure::Unregistered,
    }
}

pub fn classify_interface_error(name: &str) -> PortalError {
    match name {
        UNKNOWN_METHOD | UNKNOWN_INTERFACE | UNKNOWN_PROPERTY | INVALID_ARGS => {
            PortalError::Interface
        }
        SERVICE_UNKNOWN | NAME_HAS_NO_OWNER => PortalError::NoPortal,
        NO_REPLY | TIMED_OUT => PortalError::Timeout,
        _ => PortalError::Bus,
    }
}

pub fn classify_create_session_error(name: &str) -> PortalError {
    match name {
        PORTAL_NOT_ALLOWED => PortalError::AppIdRequired,
        other => classify_interface_error(other),
    }
}

pub fn classify_configure_error(name: &str) -> PortalError {
    match name {
        UNKNOWN_METHOD | PORTAL_FAILED => PortalError::Version,
        other => classify_interface_error(other),
    }
}

pub fn classify_owner_error(name: &str) -> PortalError {
    match name {
        NAME_HAS_NO_OWNER => PortalError::NoPortal,
        other => classify_interface_error(other),
    }
}

pub fn portal_owner_lost(stored: &str, old_owner: &str, new_owner: &str) -> bool {
    old_owner == stored || (!new_owner.is_empty() && new_owner != stored)
}

pub fn portal_appeared(old_owner: &str, new_owner: &str) -> bool {
    old_owner.is_empty() && !new_owner.is_empty()
}

pub fn backend_owner_lost(old_owner: &str) -> bool {
    !old_owner.is_empty()
}

#[derive(Debug, Default)]
pub struct ActiveShortcuts {
    ids: BTreeSet<String>,
}

impl ActiveShortcuts {
    pub fn activate(&mut self, id: &str) {
        self.ids.insert(id.to_string());
    }

    pub fn deactivate(&mut self, id: &str) -> bool {
        self.ids.remove(id)
    }

    pub fn release_all(&mut self) -> Vec<String> {
        std::mem::take(&mut self.ids).into_iter().collect()
    }
}

type Reply<T> = Box<dyn FnOnce(Result<T, PortalError>) + Send>;

pub struct Responder<T> {
    send: Option<Reply<T>>,
}

impl<T> Responder<T> {
    pub fn new(send: impl FnOnce(Result<T, PortalError>) + Send + 'static) -> Self {
        Self {
            send: Some(Box::new(send)),
        }
    }

    pub fn respond(mut self, result: Result<T, PortalError>) {
        if let Some(send) = self.send.take() {
            send(result);
        }
    }
}

impl<T> Drop for Responder<T> {
    fn drop(&mut self) {
        if let Some(send) = self.send.take() {
            send(Err(PortalError::Closed));
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PortalOptions {
    pub portal_app_id: Option<String>,
    pub sandboxed: bool,
    pub session_token: String,
    pub desktop: Desktop,
}

#[cfg(target_os = "linux")]
pub use worker::GlobalShortcutsClient;

#[cfg(target_os = "linux")]
mod worker {
    use std::{cell::RefCell, collections::HashMap, rc::Rc, sync::Arc, thread};

    use async_channel::{Receiver, Sender};
    use async_executor::{LocalExecutor, Task};
    use async_io::Timer;
    use futures_lite::{FutureExt, StreamExt, future};
    use zbus::{
        Connection, MatchRule, Message, MessageStream,
        message::Type as MessageType,
        names::OwnedUniqueName,
        zvariant::{DynamicType, OwnedObjectPath, OwnedValue, Value},
    };

    use super::*;
    use crate::portal::{REQUEST_INTERFACE, mint_token, request_path};

    type EventSink = Arc<dyn Fn(PortalEvent) + Send + Sync + 'static>;
    type Results = HashMap<String, OwnedValue>;
    type SignalHandler = fn(&RefCell<State>, u64, &Message);

    enum Command {
        Open(Responder<OpenResult>),
        Bind {
            shortcuts: Vec<ShortcutDefinition>,
            parent_window: String,
            reply: Responder<BindOutcome>,
        },
        Configure {
            parent_window: String,
            reply: Responder<()>,
        },
    }

    pub struct GlobalShortcutsClient {
        commands: Sender<Command>,
        shutdown: Sender<()>,
    }

    impl GlobalShortcutsClient {
        pub fn spawn(
            options: PortalOptions,
            sink: Arc<dyn Fn(PortalEvent) + Send + Sync + 'static>,
        ) -> std::io::Result<Self> {
            let (commands, commands_rx) = async_channel::unbounded();
            let (shutdown, shutdown_rx) = async_channel::bounded(1);
            thread::Builder::new()
                .name("fluxer-linux-portals-global-shortcuts".to_string())
                .spawn(move || run(options, sink, commands_rx, shutdown_rx))?;
            Ok(Self { commands, shutdown })
        }

        pub fn open(&self, reply: Responder<OpenResult>) {
            let _ = self.commands.try_send(Command::Open(reply));
        }

        pub fn bind(
            &self,
            shortcuts: Vec<ShortcutDefinition>,
            parent_window: String,
            reply: Responder<BindOutcome>,
        ) {
            let _ = self.commands.try_send(Command::Bind {
                shortcuts,
                parent_window,
                reply,
            });
        }

        pub fn configure(&self, parent_window: String, reply: Responder<()>) {
            let _ = self.commands.try_send(Command::Configure {
                parent_window,
                reply,
            });
        }

        pub fn close(&self) {
            self.commands.close();
            self.shutdown.close();
        }
    }

    impl Drop for GlobalShortcutsClient {
        fn drop(&mut self) {
            self.close();
        }
    }

    struct State {
        options: PortalOptions,
        sink: EventSink,
        link: Option<Link>,
        session: Option<SessionState>,
        announced: bool,
        active: ActiveShortcuts,
        pending_request: Option<OwnedObjectPath>,
        next_generation: u64,
    }

    struct Link {
        conn: Connection,
        generation: u64,
        info: Option<LinkInfo>,
        failed: bool,
        lost: Sender<()>,
        lost_rx: Receiver<()>,
        _watchers: Vec<Task<()>>,
    }

    #[derive(Clone)]
    struct LinkInfo {
        owner: OwnedUniqueName,
        version: u32,
        app_id_source: AppIdSource,
    }

    #[derive(Clone)]
    struct LinkHandle {
        conn: Connection,
        generation: u64,
        info: LinkInfo,
        lost: Receiver<()>,
    }

    struct SessionState {
        handle: OwnedObjectPath,
        bind_attempted: bool,
        shortcuts: ShortcutTable,
        closed: Sender<()>,
        closed_rx: Receiver<()>,
    }

    struct PortalResponse {
        code: u32,
        results: Results,
    }

    struct PendingRequest {
        token: String,
        predicted: OwnedObjectPath,
        stream: MessageStream,
    }

    enum CallError {
        Method { name: String, message: String },
        Timeout,
        Bus,
    }

    impl CallError {
        fn classify(self, method: fn(&str) -> PortalError) -> PortalError {
            match self {
                Self::Method { name, .. } => method(&name),
                Self::Timeout => PortalError::Timeout,
                Self::Bus => PortalError::Bus,
            }
        }
    }

    impl State {
        fn emit(&self, event: PortalEvent) {
            (self.sink)(event);
        }

        fn current_link(&self) -> Option<LinkHandle> {
            let link = self.link.as_ref()?;
            Some(LinkHandle {
                conn: link.conn.clone(),
                generation: link.generation,
                info: link.info.clone()?,
                lost: link.lost_rx.clone(),
            })
        }

        fn link_for(&self, generation: u64) -> Option<&Link> {
            self.link
                .as_ref()
                .filter(|link| link.generation == generation)
        }

        fn release_active(&mut self) {
            for id in self.active.release_all() {
                self.emit(PortalEvent::Deactivated { id });
            }
        }

        fn drop_session(&mut self) -> Option<OwnedObjectPath> {
            let session = self.session.take()?;
            session.closed.close();
            self.release_active();
            Some(session.handle)
        }

        fn lose_session(&mut self, reason: SessionLostReason) {
            self.drop_session();
            if std::mem::take(&mut self.announced) {
                self.emit(PortalEvent::SessionLost { reason });
            }
        }

        fn fail_link(&mut self, generation: u64) {
            if let Some(link) = self
                .link
                .as_mut()
                .filter(|link| link.generation == generation)
            {
                link.info = None;
                link.failed = true;
            }
        }

        fn announce_portal(&mut self, generation: u64) {
            let Some(link) = self
                .link
                .as_mut()
                .filter(|link| link.generation == generation && link.failed)
            else {
                return;
            };
            if self.session.is_some() || self.announced {
                return;
            }
            link.failed = false;
            self.emit(PortalEvent::PortalAvailable);
        }

        fn lose_link(&mut self, generation: u64, reason: SessionLostReason) -> Option<Link> {
            self.link_for(generation)?;
            self.lose_session(reason);
            self.pending_request = None;
            let link = self.link.take()?;
            link.lost.close();
            Some(link)
        }
    }

    fn run(
        options: PortalOptions,
        sink: EventSink,
        commands: Receiver<Command>,
        shutdown: Receiver<()>,
    ) {
        let executor = Rc::new(LocalExecutor::new());
        let state = Rc::new(RefCell::new(State {
            options,
            sink,
            link: None,
            session: None,
            announced: false,
            active: ActiveShortcuts::default(),
            pending_request: None,
            next_generation: 1,
        }));
        let operations = process_commands(executor.clone(), state.clone(), commands.clone());
        async_io::block_on(executor.run(operations.or(async {
            let _ = shutdown.recv().await;
        })));
        commands.close();
        while commands.try_recv().is_ok() {}
        async_io::block_on(executor.run(teardown(state)));
    }

    async fn process_commands(
        executor: Rc<LocalExecutor<'static>>,
        state: Rc<RefCell<State>>,
        commands: Receiver<Command>,
    ) {
        while let Ok(command) = commands.recv().await {
            match command {
                Command::Open(reply) => reply.respond(open(&executor, &state).await),
                Command::Configure {
                    parent_window,
                    reply,
                } => reply.respond(configure(&state, &parent_window).await),
                Command::Bind {
                    shortcuts,
                    parent_window,
                    reply,
                } => reply.respond(bind(&state, &shortcuts, &parent_window).await),
            }
        }
    }

    async fn teardown(state: Rc<RefCell<State>>) {
        let (link, session, request) = {
            let mut state = state.borrow_mut();
            let session = state.drop_session();
            let request = state.pending_request.take();
            (state.link.take(), session, request)
        };
        let Some(link) = link else {
            return;
        };
        if let Some(request) = request {
            let _ = close_object(&link.conn, &request, REQUEST_INTERFACE, TEARDOWN_TIMEOUT).await;
        }
        if let Some(session) = session {
            let _ = close_object(&link.conn, &session, SESSION_INTERFACE, TEARDOWN_TIMEOUT).await;
        }
        link.lost.close();
    }

    async fn close_object(
        conn: &Connection,
        path: &OwnedObjectPath,
        interface: &str,
        timeout: Duration,
    ) -> Result<Message, CallError> {
        call(
            conn,
            PORTAL_DESTINATION,
            path.as_str(),
            interface,
            "Close",
            &(),
            Some(timeout),
        )
        .await
    }

    async fn call<B>(
        conn: &Connection,
        destination: &str,
        path: &str,
        interface: &str,
        method: &str,
        body: &B,
        timeout: Option<Duration>,
    ) -> Result<Message, CallError>
    where
        B: zbus::export::serde::Serialize + DynamicType,
    {
        let request = async {
            conn.call_method(Some(destination), path, Some(interface), method, body)
                .await
                .map_err(|err| match err {
                    zbus::Error::MethodError(name, message, _) => CallError::Method {
                        name: name.to_string(),
                        message: message.unwrap_or_default(),
                    },
                    _ => CallError::Bus,
                })
        };
        match timeout {
            Some(limit) => {
                request
                    .or(async {
                        Timer::after(limit).await;
                        Err(CallError::Timeout)
                    })
                    .await
            }
            None => request.await,
        }
    }

    async fn open(
        executor: &Rc<LocalExecutor<'static>>,
        state: &Rc<RefCell<State>>,
    ) -> Result<OpenResult, PortalError> {
        let previous = {
            let mut state = state.borrow_mut();
            state.announced = false;
            state.drop_session();
            state.pending_request = None;
            state.link.take()
        };
        drop(previous);
        let link = connect(executor, state).await?;
        if let Err(err) = create_session(state, &link).await {
            state.borrow_mut().fail_link(link.generation);
            return Err(err);
        }
        let listed = match list_shortcuts(state, &link).await {
            Ok(listed) => listed,
            Err(err) => {
                let session = {
                    let mut state = state.borrow_mut();
                    state.fail_link(link.generation);
                    state.drop_session()
                };
                if let Some(session) = session {
                    let _ = close_object(&link.conn, &session, SESSION_INTERFACE, TEARDOWN_TIMEOUT)
                        .await;
                }
                return Err(err);
            }
        };
        let mut state = state.borrow_mut();
        let session = state.session.as_mut().ok_or(PortalError::SessionLost)?;
        session.shortcuts = ShortcutTable::from_listed(&listed);
        state.announced = true;
        Ok(OpenResult {
            version: link.info.version,
            app_id_source: link.info.app_id_source,
            unique_name: link.info.owner.to_string(),
            listed,
        })
    }

    async fn connect(
        executor: &Rc<LocalExecutor<'static>>,
        state: &Rc<RefCell<State>>,
    ) -> Result<LinkHandle, PortalError> {
        let conn = zbus::connection::Builder::session()
            .map_err(|_| PortalError::Bus)?
            .build()
            .await
            .map_err(|_| PortalError::Bus)?;
        let (generation, options) = {
            let mut state = state.borrow_mut();
            let generation = state.next_generation;
            state.next_generation += 1;
            (generation, state.options.clone())
        };
        let watchers = watch_signals(executor, state, &conn, generation, &options)
            .await
            .map_err(|_| PortalError::Bus)?;
        let (lost, lost_rx) = async_channel::bounded(1);
        state.borrow_mut().link = Some(Link {
            conn: conn.clone(),
            generation,
            info: None,
            failed: false,
            lost,
            lost_rx,
            _watchers: watchers,
        });
        let setup = establish(&conn, &options).await;
        let mut state = state.borrow_mut();
        let Some(link) = state
            .link
            .as_mut()
            .filter(|link| link.generation == generation)
        else {
            return Err(PortalError::SessionLost);
        };
        match setup {
            Ok(info) => link.info = Some(info),
            Err(err) => {
                link.failed = true;
                return Err(err);
            }
        }
        state.current_link().ok_or(PortalError::SessionLost)
    }

    async fn establish(
        conn: &Connection,
        options: &PortalOptions,
    ) -> Result<LinkInfo, PortalError> {
        let mut owner_changes =
            MessageStream::for_match_rule(owner_rule(PORTAL_DESTINATION)?, conn, None)
                .await
                .map_err(|_| PortalError::Bus)?;
        let mut registered = false;
        for _ in 0..ESTABLISH_ATTEMPTS {
            let app_id_source = register(conn, options, registered).await?;
            registered = app_id_source == AppIdSource::Registered;
            let version = read_version(conn).await?;
            let owner = portal_owner(conn).await?;
            if !portal_restarted(&mut owner_changes).await {
                return Ok(LinkInfo {
                    owner,
                    version,
                    app_id_source,
                });
            }
        }
        Err(PortalError::SessionLost)
    }

    async fn portal_restarted(changes: &mut MessageStream) -> bool {
        let mut restarted = false;
        while let Some(Some(Ok(message))) = future::poll_once(changes.next()).await {
            if let Some((_, old_owner, _)) = owner_change(&message) {
                restarted |= !old_owner.is_empty();
            }
        }
        restarted
    }

    fn running_as_snap() -> bool {
        std::fs::read_to_string("/proc/self/cgroup").is_ok_and(|cgroups| cgroup_is_snap(&cgroups))
    }

    async fn register(
        conn: &Connection,
        options: &PortalOptions,
        registered: bool,
    ) -> Result<AppIdSource, PortalError> {
        if options.sandboxed {
            return Ok(AppIdSource::Sandbox);
        }
        let Some(app_id) = options.portal_app_id.as_deref() else {
            return Ok(AppIdSource::Unregistered);
        };
        if running_as_snap() {
            return Ok(AppIdSource::Unregistered);
        }
        let mut retries = REGISTER_RETRY_DELAYS.iter();
        loop {
            let registry_options: HashMap<&str, Value<'_>> = HashMap::new();
            let result = call(
                conn,
                PORTAL_DESTINATION,
                PORTAL_PATH,
                REGISTRY_INTERFACE,
                "Register",
                &(app_id, registry_options),
                Some(SETUP_TIMEOUT),
            )
            .await;
            let failure = match result {
                Ok(_) => return Ok(AppIdSource::Registered),
                Err(CallError::Timeout) => return Err(PortalError::NoPortal),
                Err(CallError::Bus) => return Err(PortalError::Bus),
                Err(CallError::Method { name, message }) => {
                    classify_register_error(&name, &message)
                }
            };
            match failure {
                RegisterFailure::Unregistered => return Ok(AppIdSource::Unregistered),
                RegisterFailure::AlreadyAssociated if registered => {
                    return Ok(AppIdSource::Registered);
                }
                RegisterFailure::AlreadyAssociated => return Ok(AppIdSource::Unregistered),
                RegisterFailure::Fatal(err) => return Err(err),
                RegisterFailure::Retry => match retries.next() {
                    Some(delay) => {
                        Timer::after(*delay).await;
                    }
                    None => return Ok(AppIdSource::Unregistered),
                },
            }
        }
    }

    async fn read_version(conn: &Connection) -> Result<u32, PortalError> {
        let reply = call(
            conn,
            PORTAL_DESTINATION,
            PORTAL_PATH,
            PROPERTIES_INTERFACE,
            "Get",
            &(GLOBAL_SHORTCUTS_INTERFACE, "version"),
            Some(SETUP_TIMEOUT),
        )
        .await
        .map_err(|err| err.classify(classify_interface_error))?;
        let value: OwnedValue = reply
            .body()
            .deserialize()
            .map_err(|_| PortalError::Interface)?;
        crate::kwin::integer_from_variant(crate::kwin::value_of_owned(&value))
            .ok_or(PortalError::Interface)
    }

    async fn portal_owner(conn: &Connection) -> Result<OwnedUniqueName, PortalError> {
        let reply = call(
            conn,
            BUS_NAME,
            BUS_PATH,
            BUS_NAME,
            "GetNameOwner",
            &(PORTAL_DESTINATION,),
            Some(SETUP_TIMEOUT),
        )
        .await
        .map_err(|err| err.classify(classify_owner_error))?;
        reply
            .body()
            .deserialize::<OwnedUniqueName>()
            .map_err(|_| PortalError::InvalidReply)
    }

    async fn watch_signals(
        executor: &Rc<LocalExecutor<'static>>,
        state: &Rc<RefCell<State>>,
        conn: &Connection,
        generation: u64,
        options: &PortalOptions,
    ) -> Result<Vec<Task<()>>, PortalError> {
        let mut watches: Vec<(MatchRule<'static>, SignalHandler)> = vec![
            (global_shortcuts_rule()?, on_global_shortcuts_signal),
            (session_closed_rule()?, on_session_closed),
            (owner_rule(PORTAL_DESTINATION)?, on_portal_owner_changed),
        ];
        let backend = options
            .desktop
            .backend_name()
            .filter(|_| !options.sandboxed);
        if let Some(backend) = backend {
            watches.push((owner_rule(backend)?, on_backend_owner_changed));
        }
        let mut tasks = Vec::with_capacity(watches.len());
        for (rule, handler) in watches {
            let stream = MessageStream::for_match_rule(rule, conn, None)
                .await
                .map_err(|_| PortalError::Bus)?;
            tasks.push(executor.spawn(drain(state.clone(), generation, stream, handler)));
        }
        Ok(tasks)
    }

    fn global_shortcuts_rule() -> Result<MatchRule<'static>, PortalError> {
        MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(PORTAL_DESTINATION)
            .and_then(|rule| rule.interface(GLOBAL_SHORTCUTS_INTERFACE))
            .and_then(|rule| rule.path(PORTAL_PATH))
            .map(|rule| rule.build())
            .map_err(|_| PortalError::Bus)
    }

    fn session_closed_rule() -> Result<MatchRule<'static>, PortalError> {
        MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(PORTAL_DESTINATION)
            .and_then(|rule| rule.interface(SESSION_INTERFACE))
            .and_then(|rule| rule.member("Closed"))
            .and_then(|rule| rule.path_namespace(SESSION_PATH_NAMESPACE))
            .map(|rule| rule.build())
            .map_err(|_| PortalError::Bus)
    }

    fn owner_rule(name: &'static str) -> Result<MatchRule<'static>, PortalError> {
        MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(BUS_NAME)
            .and_then(|rule| rule.interface(BUS_NAME))
            .and_then(|rule| rule.member("NameOwnerChanged"))
            .and_then(|rule| rule.path(BUS_PATH))
            .and_then(|rule| rule.arg(0, name))
            .map(|rule| rule.build())
            .map_err(|_| PortalError::Bus)
    }

    async fn drain(
        state: Rc<RefCell<State>>,
        generation: u64,
        mut stream: MessageStream,
        handler: SignalHandler,
    ) {
        while let Some(Ok(message)) = stream.next().await {
            handler(&state, generation, &message);
        }
        let lost = state
            .borrow_mut()
            .lose_link(generation, SessionLostReason::BusError);
        drop(lost);
    }

    fn sent_by_portal(state: &State, generation: u64, message: &Message) -> bool {
        let Some(owner) = state
            .link_for(generation)
            .and_then(|link| link.info.as_ref())
            .map(|info| &info.owner)
        else {
            return false;
        };
        message.header().sender() == Some(owner.inner())
    }

    fn current_session<'a>(state: &'a State, session: &str) -> Option<&'a SessionState> {
        state
            .session
            .as_ref()
            .filter(|current| current.handle.as_str() == session)
    }

    fn sent_by_bus(message: &Message) -> bool {
        message
            .header()
            .sender()
            .is_some_and(|sender| sender.as_str() == BUS_NAME)
    }

    fn on_global_shortcuts_signal(state: &RefCell<State>, generation: u64, message: &Message) {
        let header = message.header();
        match header.member().map(|member| member.as_str()) {
            Some("Activated") => on_activated(state, generation, message),
            Some("Deactivated") => on_deactivated(state, generation, message),
            Some("ShortcutsChanged") => on_shortcuts_changed(state, generation, message),
            _ => {}
        }
    }

    fn shortcut_signal(state: &State, generation: u64, message: &Message) -> Option<String> {
        if !sent_by_portal(state, generation, message) {
            return None;
        }
        let (session, id, _timestamp, _options): (OwnedObjectPath, String, u64, Results) =
            message.body().deserialize().ok()?;
        current_session(state, session.as_str())
            .is_some_and(|current| current.shortcuts.contains(&id))
            .then_some(id)
    }

    fn on_activated(state: &RefCell<State>, generation: u64, message: &Message) {
        let mut state = state.borrow_mut();
        let Some(id) = shortcut_signal(&state, generation, message) else {
            return;
        };
        state.active.activate(&id);
        state.emit(PortalEvent::Activated { id });
    }

    fn on_deactivated(state: &RefCell<State>, generation: u64, message: &Message) {
        let mut state = state.borrow_mut();
        let Some(id) = shortcut_signal(&state, generation, message) else {
            return;
        };
        state.active.deactivate(&id);
        state.emit(PortalEvent::Deactivated { id });
    }

    fn on_shortcuts_changed(state: &RefCell<State>, generation: u64, message: &Message) {
        let mut state = state.borrow_mut();
        if !sent_by_portal(&state, generation, message) {
            return;
        }
        let Ok((session, shortcuts)) = message
            .body()
            .deserialize::<(OwnedObjectPath, Vec<(String, Results)>)>()
        else {
            return;
        };
        let Some(current) = state
            .session
            .as_mut()
            .filter(|current| current.handle.as_str() == session.as_str())
        else {
            return;
        };
        let changed = current.shortcuts.merge(
            shortcuts
                .into_iter()
                .map(|(id, properties)| binding_from_properties(id, &properties))
                .collect(),
        );
        let merged = current.shortcuts.bindings();
        for id in changed {
            if state.active.deactivate(&id) {
                state.emit(PortalEvent::Deactivated { id });
            }
        }
        state.emit(PortalEvent::ShortcutsChanged { shortcuts: merged });
    }

    fn on_session_closed(state: &RefCell<State>, generation: u64, message: &Message) {
        let mut state = state.borrow_mut();
        if !sent_by_portal(&state, generation, message) {
            return;
        }
        let header = message.header();
        let Some(path) = header.path() else {
            return;
        };
        if current_session(&state, path.as_str()).is_some() {
            state.lose_session(SessionLostReason::Closed);
        }
    }

    fn owner_change(message: &Message) -> Option<(String, String, String)> {
        if !sent_by_bus(message) {
            return None;
        }
        message.body().deserialize().ok()
    }

    fn on_portal_owner_changed(state: &RefCell<State>, generation: u64, message: &Message) {
        let Some((_name, old_owner, new_owner)) = owner_change(message) else {
            return;
        };
        let mut state = state.borrow_mut();
        let Some(link) = state.link_for(generation) else {
            return;
        };
        match link.info.as_ref() {
            Some(info) => {
                if portal_owner_lost(info.owner.as_str(), &old_owner, &new_owner) {
                    let link = state.lose_link(generation, SessionLostReason::PortalRestarted);
                    drop(state);
                    drop(link);
                }
            }
            None => {
                if portal_appeared(&old_owner, &new_owner) {
                    state.announce_portal(generation);
                }
            }
        }
    }

    fn on_backend_owner_changed(state: &RefCell<State>, generation: u64, message: &Message) {
        let Some((_name, old_owner, new_owner)) = owner_change(message) else {
            return;
        };
        let mut state = state.borrow_mut();
        if portal_appeared(&old_owner, &new_owner) {
            state.announce_portal(generation);
            return;
        }
        if !backend_owner_lost(&old_owner) {
            return;
        }
        if state
            .link_for(generation)
            .is_none_or(|link| link.info.is_none())
        {
            return;
        }
        let link = state.lose_link(generation, SessionLostReason::PortalRestarted);
        drop(state);
        drop(link);
    }

    async fn begin_request(link: &LinkHandle, prefix: &str) -> Result<PendingRequest, PortalError> {
        let unique_name = link.conn.unique_name().ok_or(PortalError::Bus)?.to_string();
        let token = mint_token(prefix);
        let predicted = OwnedObjectPath::try_from(request_path(&unique_name, &token))
            .map_err(|_| PortalError::InvalidReply)?;
        let namespace = request_path(&unique_name, "");
        let namespace = namespace.trim_end_matches('/').to_string();
        let rule = MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(PORTAL_DESTINATION)
            .and_then(|rule| rule.interface(REQUEST_INTERFACE))
            .and_then(|rule| rule.member("Response"))
            .and_then(|rule| rule.path_namespace(namespace))
            .map_err(|_| PortalError::Bus)?
            .build();
        let stream = MessageStream::for_match_rule(rule, &link.conn, None)
            .await
            .map_err(|_| PortalError::Bus)?;
        Ok(PendingRequest {
            token,
            predicted,
            stream,
        })
    }

    fn returned_request_path(reply: &Message) -> Option<OwnedObjectPath> {
        reply.body().deserialize::<OwnedObjectPath>().ok()
    }

    async fn wait_response(
        state: &Rc<RefCell<State>>,
        link: &LinkHandle,
        pending: PendingRequest,
        reply: &Message,
        session_closed: Option<Receiver<()>>,
        timeout: Option<Duration>,
    ) -> Result<PortalResponse, PortalError> {
        let PendingRequest {
            predicted,
            mut stream,
            ..
        } = pending;
        let request = returned_request_path(reply).unwrap_or(predicted);
        state.borrow_mut().pending_request = Some(request.clone());
        let owner = link.info.owner.clone();
        let response = async {
            while let Some(item) = stream.next().await {
                let Ok(message) = item else {
                    break;
                };
                let header = message.header();
                if header.sender() != Some(owner.inner())
                    || header.path().map(|path| path.as_str()) != Some(request.as_str())
                {
                    continue;
                }
                if let Ok((code, results)) = message.body().deserialize::<(u32, Results)>() {
                    return Ok(PortalResponse { code, results });
                }
            }
            Err(PortalError::Bus)
        };
        let lost = async {
            let _ = link.lost.recv().await;
            Err(PortalError::SessionLost)
        };
        let closed = async {
            match session_closed {
                Some(closed) => {
                    let _ = closed.recv().await;
                    Err(PortalError::SessionLost)
                }
                None => future::pending().await,
            }
        };
        let expired = async {
            match timeout {
                Some(limit) => {
                    Timer::after(limit).await;
                    Err(PortalError::Timeout)
                }
                None => future::pending().await,
            }
        };
        let result = response.or(lost).or(closed).or(expired).await;
        let request = state.borrow_mut().pending_request.take();
        if let (Err(PortalError::Timeout), Some(request)) = (&result, request) {
            let _ = close_object(&link.conn, &request, REQUEST_INTERFACE, TEARDOWN_TIMEOUT).await;
        }
        result
    }

    async fn create_session(
        state: &Rc<RefCell<State>>,
        link: &LinkHandle,
    ) -> Result<(), PortalError> {
        let pending = begin_request(link, "fluxer_gs_create").await?;
        let session_token = state.borrow().options.session_token.clone();
        let mut options: HashMap<&str, Value<'_>> = HashMap::new();
        options.insert("handle_token", Value::new(pending.token.as_str()));
        options.insert("session_handle_token", Value::new(session_token.as_str()));
        let reply = call(
            &link.conn,
            PORTAL_DESTINATION,
            PORTAL_PATH,
            GLOBAL_SHORTCUTS_INTERFACE,
            "CreateSession",
            &(options,),
            Some(SETUP_TIMEOUT),
        )
        .await
        .map_err(|err| err.classify(classify_create_session_error))?;
        let response =
            wait_response(state, link, pending, &reply, None, Some(SETUP_TIMEOUT)).await?;
        if response.code != 0 {
            return Err(PortalError::Response(response.code));
        }
        let handle = response
            .results
            .get("session_handle")
            .and_then(|value| string_or_object_path(crate::kwin::value_of_owned(value)))
            .and_then(|handle| OwnedObjectPath::try_from(handle).ok())
            .ok_or(PortalError::InvalidReply)?;
        if state.borrow().link_for(link.generation).is_none() {
            let _ = close_object(&link.conn, &handle, SESSION_INTERFACE, TEARDOWN_TIMEOUT).await;
            return Err(PortalError::SessionLost);
        }
        let (closed, closed_rx) = async_channel::bounded(1);
        state.borrow_mut().session = Some(SessionState {
            handle,
            bind_attempted: false,
            shortcuts: ShortcutTable::default(),
            closed,
            closed_rx,
        });
        Ok(())
    }

    fn session_handles(state: &State) -> Result<(OwnedObjectPath, Receiver<()>), PortalError> {
        state
            .session
            .as_ref()
            .map(|session| (session.handle.clone(), session.closed_rx.clone()))
            .ok_or(PortalError::NoSession)
    }

    async fn list_shortcuts(
        state: &Rc<RefCell<State>>,
        link: &LinkHandle,
    ) -> Result<Vec<ShortcutBinding>, PortalError> {
        let (session, closed) = session_handles(&state.borrow())?;
        let pending = begin_request(link, "fluxer_gs_list").await?;
        let mut options: HashMap<&str, Value<'_>> = HashMap::new();
        options.insert("handle_token", Value::new(pending.token.as_str()));
        let reply = call(
            &link.conn,
            PORTAL_DESTINATION,
            PORTAL_PATH,
            GLOBAL_SHORTCUTS_INTERFACE,
            "ListShortcuts",
            &(&session, options),
            Some(SETUP_TIMEOUT),
        )
        .await
        .map_err(|err| err.classify(classify_interface_error))?;
        let response = wait_response(
            state,
            link,
            pending,
            &reply,
            Some(closed),
            Some(SETUP_TIMEOUT),
        )
        .await?;
        if response.code != 0 {
            return Ok(Vec::new());
        }
        Ok(bindings_from_results(&response.results))
    }

    async fn bind(
        state: &Rc<RefCell<State>>,
        shortcuts: &[ShortcutDefinition],
        parent_window: &str,
    ) -> Result<BindOutcome, PortalError> {
        let result = bind_session(state, shortcuts, parent_window).await;
        if result.is_err() {
            let mut state = state.borrow_mut();
            if state.session.is_none() {
                state.lose_session(SessionLostReason::BusError);
            }
        }
        result
    }

    async fn bind_session(
        state: &Rc<RefCell<State>>,
        shortcuts: &[ShortcutDefinition],
        parent_window: &str,
    ) -> Result<BindOutcome, PortalError> {
        let link = state
            .borrow()
            .current_link()
            .ok_or(PortalError::NoSession)?;
        let attempted = state
            .borrow()
            .session
            .as_ref()
            .ok_or(PortalError::NoSession)?
            .bind_attempted;
        if attempted {
            let previous = state.borrow_mut().drop_session();
            if let Some(previous) = previous {
                let _ = close_object(&link.conn, &previous, SESSION_INTERFACE, SETUP_TIMEOUT).await;
            }
            create_session(state, &link).await?;
        }
        let (session, closed) = {
            let mut state = state.borrow_mut();
            let session = state.session.as_mut().ok_or(PortalError::NoSession)?;
            session.bind_attempted = true;
            session.shortcuts = ShortcutTable::from_definitions(shortcuts);
            (session.handle.clone(), session.closed_rx.clone())
        };
        let pending = begin_request(&link, "fluxer_gs_bind").await?;
        let mut options: HashMap<&str, Value<'_>> = HashMap::new();
        options.insert("handle_token", Value::new(pending.token.as_str()));
        let reply = call(
            &link.conn,
            PORTAL_DESTINATION,
            PORTAL_PATH,
            GLOBAL_SHORTCUTS_INTERFACE,
            "BindShortcuts",
            &(
                &session,
                serialize_shortcuts(shortcuts),
                parent_window,
                options,
            ),
            Some(SETUP_TIMEOUT),
        )
        .await
        .map_err(|err| err.classify(classify_interface_error))?;
        let response = wait_response(state, &link, pending, &reply, Some(closed), None).await?;
        let returned = known_bindings(state, bindings_from_results(&response.results))?;
        let outcome = match bind_step(response.code, returned) {
            BindStep::Done(outcome) => outcome,
            BindStep::List => {
                let listed = list_shortcuts(state, &link).await?;
                listed_outcome(known_bindings(state, listed)?)
            }
        };
        let BindOutcome::Bound(bound) = outcome else {
            return Ok(outcome);
        };
        let mut state = state.borrow_mut();
        let session = state.session.as_mut().ok_or(PortalError::SessionLost)?;
        session.shortcuts.merge(bound);
        Ok(BindOutcome::Bound(session.shortcuts.bindings()))
    }

    fn known_bindings(
        state: &Rc<RefCell<State>>,
        bindings: Vec<ShortcutBinding>,
    ) -> Result<Vec<ShortcutBinding>, PortalError> {
        let state = state.borrow();
        let session = state.session.as_ref().ok_or(PortalError::SessionLost)?;
        Ok(session.shortcuts.known(bindings))
    }

    async fn configure(state: &Rc<RefCell<State>>, parent_window: &str) -> Result<(), PortalError> {
        let link = state
            .borrow()
            .current_link()
            .ok_or(PortalError::NoSession)?;
        let (session, closed) = session_handles(&state.borrow())?;
        if link.info.version < 2 {
            return Err(PortalError::Version);
        }
        let options: HashMap<&str, Value<'_>> = HashMap::new();
        let request = async {
            call(
                &link.conn,
                PORTAL_DESTINATION,
                PORTAL_PATH,
                GLOBAL_SHORTCUTS_INTERFACE,
                "ConfigureShortcuts",
                &(&session, parent_window, options),
                Some(SETUP_TIMEOUT),
            )
            .await
            .map(|_| ())
            .map_err(|err| err.classify(classify_configure_error))
        };
        let interrupted = async {
            async {
                let _ = link.lost.recv().await;
            }
            .or(async {
                let _ = closed.recv().await;
            })
            .await;
            Err(PortalError::SessionLost)
        };
        request.or(interrupted).await
    }

    fn serialize_shortcuts(
        shortcuts: &[ShortcutDefinition],
    ) -> Vec<(&str, HashMap<&str, Value<'_>>)> {
        shortcuts
            .iter()
            .map(|shortcut| {
                let mut options: HashMap<&str, Value<'_>> = HashMap::new();
                options.insert("description", Value::new(shortcut.description.as_str()));
                if let Some(trigger) = shortcut.preferred_trigger.as_deref() {
                    options.insert("preferred_trigger", Value::new(trigger));
                }
                (shortcut.id.as_str(), options)
            })
            .collect()
    }

    fn bindings_from_results(results: &Results) -> Vec<ShortcutBinding> {
        let Some(value) = results.get("shortcuts") else {
            return Vec::new();
        };
        let Value::Array(array) = unbox_value(crate::kwin::value_of_owned(value)) else {
            return Vec::new();
        };
        array
            .inner()
            .iter()
            .filter_map(binding_from_value)
            .collect()
    }

    fn binding_from_value(value: &Value<'_>) -> Option<ShortcutBinding> {
        let Value::Structure(structure) = unbox_value(value) else {
            return None;
        };
        let [id, properties] = structure.fields() else {
            return None;
        };
        let id = string_or_object_path(id)?;
        let Value::Dict(dict) = unbox_value(properties) else {
            return Some(ShortcutBinding::new(id, None, None));
        };
        Some(ShortcutBinding::new(
            id,
            dict_string(dict, "description"),
            dict_string(dict, "trigger_description"),
        ))
    }

    fn binding_from_properties(id: String, properties: &Results) -> ShortcutBinding {
        let read = |key: &str| {
            properties
                .get(key)
                .and_then(|value| string_or_object_path(crate::kwin::value_of_owned(value)))
        };
        ShortcutBinding::new(id, read("description"), read("trigger_description"))
    }

    fn dict_string(dict: &zbus::zvariant::Dict<'_, '_>, key: &str) -> Option<String> {
        dict.iter().find_map(|(k, v)| {
            if string_or_object_path(k).as_deref() == Some(key) {
                string_or_object_path(v)
            } else {
                None
            }
        })
    }

    fn unbox_value<'a>(value: &'a Value<'a>) -> &'a Value<'a> {
        match value {
            Value::Value(inner) => unbox_value(inner),
            other => other,
        }
    }

    fn string_or_object_path(value: &Value<'_>) -> Option<String> {
        match unbox_value(value) {
            Value::Str(v) => Some(v.as_str().to_string()),
            Value::ObjectPath(v) => Some(v.as_str().to_string()),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn binding(id: &str, trigger: Option<&str>) -> ShortcutBinding {
        ShortcutBinding::new(id.into(), None, trigger.map(Into::into))
    }

    fn definition(id: &str, description: &str) -> ShortcutDefinition {
        ShortcutDefinition {
            id: id.into(),
            description: description.into(),
            preferred_trigger: None,
        }
    }

    #[test]
    fn bind_step_prefers_returned_shortcuts_over_code() {
        let bound = vec![binding("voice_push_to_talk", Some("Ctrl+Shift+P"))];
        for code in [0, 1, 2, 9] {
            assert_eq!(
                bind_step(code, bound.clone()),
                BindStep::Done(BindOutcome::Bound(bound.clone()))
            );
        }
    }

    #[test]
    fn bind_step_maps_empty_results_by_code() {
        assert_eq!(bind_step(0, Vec::new()), BindStep::List);
        assert_eq!(
            bind_step(1, Vec::new()),
            BindStep::Done(BindOutcome::Cancelled)
        );
        assert_eq!(
            bind_step(2, Vec::new()),
            BindStep::Done(BindOutcome::Denied)
        );
        assert_eq!(
            bind_step(3, Vec::new()),
            BindStep::Done(BindOutcome::Failed(3))
        );
    }

    #[test]
    fn listed_outcome_binds_only_when_the_portal_lists_shortcuts() {
        assert_eq!(listed_outcome(Vec::new()), BindOutcome::Denied);
        let listed = vec![binding("voice_push_to_talk", None)];
        assert_eq!(listed_outcome(listed.clone()), BindOutcome::Bound(listed));
    }

    #[test]
    fn binding_drops_empty_strings() {
        let parsed = ShortcutBinding::new("a".into(), Some(String::new()), Some(String::new()));
        assert_eq!(parsed.description, None);
        assert_eq!(parsed.trigger_description, None);
        let kept = ShortcutBinding::new("a".into(), Some("A".into()), Some("F13".into()));
        assert_eq!(kept.description.as_deref(), Some("A"));
        assert_eq!(kept.trigger_description.as_deref(), Some("F13"));
    }

    #[test]
    fn definitions_must_be_non_empty_unique_and_described() {
        assert_eq!(validate_definitions(&[]), Err(DefinitionError::Empty));
        assert_eq!(
            validate_definitions(&[definition("", "Mute")]),
            Err(DefinitionError::EmptyId)
        );
        assert_eq!(
            validate_definitions(&[definition("voice_toggle_mute", " ")]),
            Err(DefinitionError::EmptyDescription)
        );
        assert_eq!(
            validate_definitions(&[
                definition("voice_toggle_mute", "Mute"),
                definition("voice_toggle_mute", "Mute again"),
            ]),
            Err(DefinitionError::DuplicateId)
        );
        assert_eq!(
            validate_definitions(&[
                definition("voice_toggle_mute", "Mute"),
                definition("voice_push_to_talk", "Push to talk"),
            ]),
            Ok(())
        );
    }

    #[test]
    fn tokens_only_allow_portal_safe_characters() {
        assert!(is_valid_token("fluxer_global_shortcuts"));
        assert!(is_valid_token("fluxer_canary_global_shortcuts"));
        assert!(!is_valid_token(""));
        assert!(!is_valid_token("fluxer-global"));
        assert!(!is_valid_token("fluxer.global"));
    }

    #[test]
    fn desktop_selects_only_its_own_backend() {
        assert_eq!(
            Desktop::parse("kde").backend_name(),
            Some("org.freedesktop.impl.portal.desktop.kde")
        );
        assert_eq!(
            Desktop::parse("gnome").backend_name(),
            Some("org.freedesktop.impl.portal.desktop.gnome")
        );
        assert_eq!(
            Desktop::parse("hyprland").backend_name(),
            Some("org.freedesktop.impl.portal.desktop.hyprland")
        );
        assert_eq!(Desktop::parse("other").backend_name(), None);
        assert_eq!(Desktop::parse("sway").backend_name(), None);
    }

    #[test]
    fn snap_scopes_are_detected_from_cgroups() {
        assert!(cgroup_is_snap(
            "0::/user.slice/user-1000.slice/user@1000.service/apps.slice/snap.fluxer.fluxer-1234.scope\n"
        ));
        assert!(!cgroup_is_snap(
            "0::/user.slice/user-1000.slice/user@1000.service/app.slice/app-gnome-fluxer-1234.scope\n"
        ));
        assert!(cgroup_is_snap(
            "12:pids:/user.slice\n1:name=systemd:/user.slice/snap.fluxer.fluxer-1.scope\n"
        ));
        assert!(!cgroup_is_snap("garbage\n"));
    }

    #[test]
    fn shortcut_table_drops_unknown_ids() {
        let table = ShortcutTable::from_definitions(&[definition("voice_push_to_talk", "PTT")]);
        assert!(table.contains("voice_push_to_talk"));
        assert!(!table.contains("stray"));
        assert_eq!(
            table.known(vec![
                binding("stray", Some("F1")),
                binding("voice_push_to_talk", Some("F13")),
            ]),
            vec![binding("voice_push_to_talk", Some("F13"))]
        );
    }

    #[test]
    fn shortcut_table_merges_deltas_and_reports_trigger_changes() {
        let mut table = ShortcutTable::from_definitions(&[
            definition("voice_push_to_talk", "PTT"),
            definition("voice_toggle_mute", "Mute"),
        ]);
        let changed = table.merge(vec![
            binding("voice_push_to_talk", Some("F13")),
            binding("voice_toggle_mute", None),
            binding("stray", Some("F2")),
        ]);
        assert_eq!(changed, vec!["voice_push_to_talk".to_string()]);
        assert_eq!(
            table.merge(vec![binding("voice_toggle_mute", Some("F9"))]),
            vec!["voice_toggle_mute".to_string()]
        );
        let bindings = table.bindings();
        assert_eq!(bindings.len(), 2);
        assert_eq!(
            bindings[0],
            ShortcutBinding::new(
                "voice_push_to_talk".into(),
                Some("PTT".into()),
                Some("F13".into())
            )
        );
        assert_eq!(bindings[1].trigger_description.as_deref(), Some("F9"));
        assert_eq!(
            table.merge(vec![binding("voice_push_to_talk", None)]),
            vec!["voice_push_to_talk".to_string()]
        );
        assert!(
            table
                .merge(vec![binding("voice_push_to_talk", None)])
                .is_empty()
        );
    }

    #[test]
    fn listed_table_accepts_the_listed_ids() {
        let table = ShortcutTable::from_listed(&[binding("voice_push_to_talk", Some("F13"))]);
        assert!(table.contains("voice_push_to_talk"));
        assert!(!table.contains("voice_toggle_mute"));
        assert!(ShortcutTable::default().bindings().is_empty());
    }

    #[test]
    fn register_errors_are_classified() {
        for name in [
            "org.freedesktop.DBus.Error.UnknownMethod",
            "org.freedesktop.DBus.Error.UnknownInterface",
            "org.freedesktop.portal.Error.NotAllowed",
            "org.freedesktop.DBus.Error.AccessDenied",
        ] {
            assert_eq!(
                classify_register_error(name, ""),
                RegisterFailure::Unregistered
            );
        }
        assert_eq!(
            classify_register_error(
                "org.freedesktop.portal.Error.Failed",
                "Can't create registered app"
            ),
            RegisterFailure::Retry
        );
        assert_eq!(
            classify_register_error(
                "org.freedesktop.portal.Error.Failed",
                "Connection already associated with an application ID"
            ),
            RegisterFailure::AlreadyAssociated
        );
        for missing in [
            "org.freedesktop.DBus.Error.ServiceUnknown",
            "org.freedesktop.DBus.Error.NameHasNoOwner",
            "org.freedesktop.DBus.Error.NoReply",
            "org.freedesktop.DBus.Error.TimedOut",
        ] {
            assert_eq!(
                classify_register_error(missing, ""),
                RegisterFailure::Fatal(PortalError::NoPortal)
            );
        }
    }

    #[test]
    fn interface_errors_are_classified() {
        for name in [
            "org.freedesktop.DBus.Error.InvalidArgs",
            "org.freedesktop.DBus.Error.UnknownInterface",
            "org.freedesktop.DBus.Error.UnknownMethod",
            "org.freedesktop.DBus.Error.UnknownProperty",
        ] {
            assert_eq!(classify_interface_error(name), PortalError::Interface);
        }
        for name in [
            "org.freedesktop.DBus.Error.ServiceUnknown",
            "org.freedesktop.DBus.Error.NameHasNoOwner",
        ] {
            assert_eq!(classify_interface_error(name), PortalError::NoPortal);
        }
        assert_eq!(
            classify_interface_error("org.freedesktop.DBus.Error.AccessDenied"),
            PortalError::Bus
        );
    }

    #[test]
    fn no_reply_and_timed_out_are_transient_after_register() {
        for name in [
            "org.freedesktop.DBus.Error.NoReply",
            "org.freedesktop.DBus.Error.TimedOut",
        ] {
            assert_eq!(classify_interface_error(name), PortalError::Timeout);
            assert_eq!(classify_create_session_error(name), PortalError::Timeout);
            assert_eq!(classify_configure_error(name), PortalError::Timeout);
            assert_eq!(classify_owner_error(name), PortalError::Timeout);
        }
    }

    #[test]
    fn owner_lookup_without_owner_means_no_portal() {
        assert_eq!(
            classify_owner_error("org.freedesktop.DBus.Error.NameHasNoOwner"),
            PortalError::NoPortal
        );
    }

    #[test]
    fn create_session_not_allowed_means_missing_app_id() {
        assert_eq!(
            classify_create_session_error("org.freedesktop.portal.Error.NotAllowed"),
            PortalError::AppIdRequired
        );
        assert_eq!(
            classify_create_session_error("org.freedesktop.DBus.Error.UnknownMethod"),
            PortalError::Interface
        );
    }

    #[test]
    fn configure_unknown_method_or_failure_means_unsupported() {
        assert_eq!(
            classify_configure_error("org.freedesktop.DBus.Error.UnknownMethod"),
            PortalError::Version
        );
        assert_eq!(
            classify_configure_error("org.freedesktop.portal.Error.Failed"),
            PortalError::Version
        );
    }

    #[test]
    fn error_messages_carry_category_prefixes() {
        assert_eq!(PortalError::NoPortal.to_string(), "unsupported:no-portal");
        assert_eq!(PortalError::Interface.to_string(), "unsupported:interface");
        assert_eq!(PortalError::Version.to_string(), "unsupported:version");
        assert_eq!(PortalError::Response(2).to_string(), "unsupported:2");
        assert_eq!(
            PortalError::AppIdRequired.to_string(),
            "identity:app-id-required"
        );
        assert_eq!(PortalError::Timeout.to_string(), "error:timeout");
    }

    #[test]
    fn portal_owner_changes_ignore_the_stored_owner_appearing() {
        assert!(!portal_owner_lost(":1.5", "", ":1.5"));
    }

    #[test]
    fn portal_owner_changes_detect_loss_and_replacement() {
        assert!(portal_owner_lost(":1.5", ":1.5", ""));
        assert!(portal_owner_lost(":1.5", ":1.5", ":1.9"));
        assert!(portal_owner_lost(":1.5", "", ":1.9"));
        assert!(!portal_owner_lost(":1.5", ":1.7", ""));
    }

    #[test]
    fn portal_appears_only_when_it_gains_an_owner() {
        assert!(portal_appeared("", ":1.9"));
        assert!(!portal_appeared(":1.5", ""));
        assert!(!portal_appeared(":1.5", ":1.9"));
        assert!(!portal_appeared("", ""));
    }

    #[test]
    fn backend_owner_loss_requires_a_previous_owner() {
        assert!(!backend_owner_lost(""));
        assert!(backend_owner_lost(":1.12"));
    }

    #[test]
    fn active_shortcuts_track_held_ids() {
        let mut active = ActiveShortcuts::default();
        active.activate("voice_push_to_talk");
        active.activate("voice_push_to_talk");
        assert!(!active.deactivate("voice_toggle_mute"));
        assert!(active.deactivate("voice_push_to_talk"));
        assert!(!active.deactivate("voice_push_to_talk"));
    }

    #[test]
    fn active_shortcuts_release_all_empties_the_set() {
        let mut active = ActiveShortcuts::default();
        active.activate("voice_push_to_talk");
        active.activate("voice_push_to_mute");
        assert_eq!(
            active.release_all(),
            vec![
                "voice_push_to_mute".to_string(),
                "voice_push_to_talk".to_string()
            ]
        );
        assert!(active.release_all().is_empty());
    }

    #[test]
    fn responder_rejects_with_closed_when_dropped() {
        let (tx, rx) = std::sync::mpsc::channel();
        drop(Responder::<u32>::new(move |result| {
            let _ = tx.send(result);
        }));
        assert_eq!(rx.recv().unwrap(), Err(PortalError::Closed));
    }

    #[test]
    fn responder_delivers_once() {
        let (tx, rx) = std::sync::mpsc::channel();
        Responder::<u32>::new(move |result| {
            let _ = tx.send(result);
        })
        .respond(Ok(7));
        assert_eq!(rx.recv().unwrap(), Ok(7));
        assert!(rx.try_recv().is_err());
    }
}
