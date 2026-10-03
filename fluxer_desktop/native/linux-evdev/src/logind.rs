// SPDX-License-Identifier: AGPL-3.0-or-later

use std::{
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
    thread::{self, JoinHandle},
    time::Duration,
};

use event_listener::Event;
use futures_lite::{StreamExt, future};
use polling::Poller;
use zbus::{Connection, Proxy, zvariant::OwnedObjectPath};

const LOGIND_DESTINATION: &str = "org.freedesktop.login1";
const LOGIND_PATH: &str = "/org/freedesktop/login1";
const MANAGER_INTERFACE: &str = "org.freedesktop.login1.Manager";
const SESSION_INTERFACE: &str = "org.freedesktop.login1.Session";
const USER_INTERFACE: &str = "org.freedesktop.login1.User";
const RETRY_DELAYS: [Duration; 5] = [
    Duration::from_secs(1),
    Duration::from_secs(2),
    Duration::from_secs(5),
    Duration::from_secs(10),
    Duration::from_secs(30),
];

pub struct SessionGate {
    active: AtomicBool,
    poller: Arc<Poller>,
}

impl SessionGate {
    pub fn new(poller: Arc<Poller>) -> Self {
        Self {
            active: AtomicBool::new(false),
            poller,
        }
    }

    pub fn is_active(&self) -> bool {
        self.active.load(Ordering::Acquire)
    }

    pub fn set_active(&self, active: bool) {
        if self.active.swap(active, Ordering::AcqRel) != active {
            let _ = self.poller.notify();
        }
    }
}

struct StopSignal {
    stopped: AtomicBool,
    event: Event,
}

impl StopSignal {
    async fn wait(&self) {
        loop {
            if self.stopped.load(Ordering::Acquire) {
                return;
            }
            let listener = self.event.listen();
            if self.stopped.load(Ordering::Acquire) {
                return;
            }
            listener.await;
        }
    }
}

pub struct SessionWatch {
    stop: Arc<StopSignal>,
    thread: Option<JoinHandle<()>>,
}

impl SessionWatch {
    pub fn spawn(gate: Arc<SessionGate>) -> std::io::Result<Self> {
        let stop = Arc::new(StopSignal {
            stopped: AtomicBool::new(false),
            event: Event::new(),
        });
        let thread_stop = stop.clone();
        let thread = thread::Builder::new()
            .name("fluxer-linux-evdev-logind".to_string())
            .spawn(move || {
                async_io::block_on(future::or(supervise(&gate), thread_stop.wait()));
            })?;
        Ok(Self {
            stop,
            thread: Some(thread),
        })
    }

    fn shutdown(&mut self) {
        self.stop.stopped.store(true, Ordering::Release);
        self.stop.event.notify(usize::MAX);
        if let Some(thread) = self.thread.take() {
            let _ = thread.join();
        }
    }
}

impl Drop for SessionWatch {
    fn drop(&mut self) {
        self.shutdown();
    }
}

#[derive(Debug)]
enum WatchEnd {
    Unreachable(zbus::Error),
    NoSession(zbus::Error),
    Lost(Option<zbus::Error>),
}

fn retry_delay(failures: usize) -> Duration {
    RETRY_DELAYS[failures.min(RETRY_DELAYS.len() - 1)]
}

async fn supervise(gate: &SessionGate) {
    let mut ever_subscribed = false;
    let mut failures = 0usize;
    loop {
        let mut subscribed = false;
        let end = watch_session(gate, &mut subscribed).await;
        if subscribed {
            ever_subscribed = true;
            failures = 0;
        }
        match &end {
            WatchEnd::Unreachable(err) if !ever_subscribed => {
                eprintln!("[linux-evdev] logind unreachable, session gating disabled: {err}");
                gate.set_active(true);
                return;
            }
            WatchEnd::NoSession(err) if !ever_subscribed => {
                eprintln!("[linux-evdev] no logind session found, session gating disabled: {err}");
                gate.set_active(true);
                return;
            }
            WatchEnd::Unreachable(err) | WatchEnd::NoSession(err) | WatchEnd::Lost(Some(err))
                if failures == 0 =>
            {
                eprintln!("[linux-evdev] logind session watch lost, retrying: {err}");
            }
            _ => {}
        }
        async_io::Timer::after(retry_delay(failures)).await;
        failures = failures.saturating_add(1);
    }
}

fn session_id() -> Option<String> {
    std::env::var("XDG_SESSION_ID")
        .ok()
        .filter(|value| !value.is_empty())
}

fn is_unreachable_error(err: &zbus::Error) -> bool {
    match err {
        zbus::Error::MethodError(name, _, _) => matches!(
            name.as_str(),
            "org.freedesktop.DBus.Error.ServiceUnknown"
                | "org.freedesktop.DBus.Error.NameHasNoOwner"
                | "org.freedesktop.DBus.Error.AccessDenied"
                | "org.freedesktop.DBus.Error.Disconnected"
        ),
        zbus::Error::InputOutput(_) | zbus::Error::Address(_) | zbus::Error::Handshake(_) => true,
        _ => false,
    }
}

fn classify(err: zbus::Error) -> WatchEnd {
    if is_unreachable_error(&err) {
        WatchEnd::Unreachable(err)
    } else {
        WatchEnd::NoSession(err)
    }
}

fn is_session_path(path: &OwnedObjectPath) -> bool {
    path.as_str() != "/"
}

async fn display_session(
    connection: &Connection,
    manager: &Proxy<'_>,
) -> zbus::Result<OwnedObjectPath> {
    let user_path: OwnedObjectPath = manager.call("GetUserByPID", &(std::process::id(),)).await?;
    let user = Proxy::new(connection, LOGIND_DESTINATION, user_path, USER_INTERFACE).await?;
    let (id, path): (String, OwnedObjectPath) = user.get_property("Display").await?;
    if id.is_empty() || !is_session_path(&path) {
        return Err(zbus::Error::Failure(
            "user has no display session".to_string(),
        ));
    }
    Ok(path)
}

async fn resolve_session(
    connection: &Connection,
    manager: &Proxy<'_>,
) -> Result<OwnedObjectPath, WatchEnd> {
    if let Some(id) = session_id() {
        match manager
            .call::<_, _, OwnedObjectPath>("GetSession", &(id,))
            .await
        {
            Ok(path) => return Ok(path),
            Err(err) if is_unreachable_error(&err) => return Err(WatchEnd::Unreachable(err)),
            Err(_) => {}
        }
    }
    match manager
        .call::<_, _, OwnedObjectPath>("GetSessionByPID", &(std::process::id(),))
        .await
    {
        Ok(path) => return Ok(path),
        Err(err) if is_unreachable_error(&err) => return Err(WatchEnd::Unreachable(err)),
        Err(_) => {}
    }
    display_session(connection, manager).await.map_err(classify)
}

async fn watch_session(gate: &SessionGate, subscribed: &mut bool) -> WatchEnd {
    let connection = match Connection::system().await {
        Ok(connection) => connection,
        Err(err) => return WatchEnd::Unreachable(err),
    };
    let manager = match Proxy::new(
        &connection,
        LOGIND_DESTINATION,
        LOGIND_PATH,
        MANAGER_INTERFACE,
    )
    .await
    {
        Ok(manager) => manager,
        Err(err) => return classify(err),
    };
    let session_path = match resolve_session(&connection, &manager).await {
        Ok(path) => path,
        Err(end) => return end,
    };
    let session = match Proxy::new(
        &connection,
        LOGIND_DESTINATION,
        session_path,
        SESSION_INTERFACE,
    )
    .await
    {
        Ok(session) => session,
        Err(err) => return classify(err),
    };
    let mut owner_changes = match session.receive_owner_changed().await {
        Ok(stream) => stream,
        Err(err) => return classify(err),
    };
    let mut changes = session.receive_property_changed::<bool>("Active").await;
    match session.get_property::<bool>("Active").await {
        Ok(active) => gate.set_active(active),
        Err(err) => return classify(err),
    }
    *subscribed = true;
    loop {
        let change = future::or(async { changes.next().await }, async {
            owner_changes.next().await;
            None
        })
        .await;
        let Some(change) = change else {
            return WatchEnd::Lost(None);
        };
        match change.get().await {
            Ok(active) => gate.set_active(active),
            Err(err) => return WatchEnd::Lost(Some(err)),
        }
    }
}
