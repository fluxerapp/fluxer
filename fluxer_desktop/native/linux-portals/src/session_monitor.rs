// SPDX-License-Identifier: AGPL-3.0-or-later

use std::time::Duration;

pub const PORTAL_DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PORTAL_PATH: &str = "/org/freedesktop/portal/desktop";
pub const INHIBIT_INTERFACE: &str = "org.freedesktop.portal.Inhibit";
pub const SESSION_INTERFACE: &str = "org.freedesktop.portal.Session";
pub const BUS_NAME: &str = "org.freedesktop.DBus";
pub const BUS_PATH: &str = "/org/freedesktop/DBus";
pub const SETUP_TIMEOUT: Duration = Duration::from_secs(30);
pub const SHORT_TIMEOUT: Duration = Duration::from_secs(2);
pub const QUERY_END: u32 = 2;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MonitorEvent {
    ScreenLocked,
    ScreenUnlocked,
}

#[derive(Debug, Default)]
pub struct LockTracker {
    locked: bool,
}

impl LockTracker {
    pub fn update(&mut self, screensaver_active: bool) -> Option<MonitorEvent> {
        if screensaver_active == self.locked {
            return None;
        }
        self.locked = screensaver_active;
        Some(if screensaver_active {
            MonitorEvent::ScreenLocked
        } else {
            MonitorEvent::ScreenUnlocked
        })
    }
}

#[cfg(target_os = "linux")]
pub use worker::SessionMonitor;

#[cfg(target_os = "linux")]
mod worker {
    use std::{cell::RefCell, collections::HashMap, sync::Arc, thread};

    use async_channel::{Receiver, Sender};
    use async_io::Timer;
    use futures_lite::{FutureExt, StreamExt};
    use zbus::{
        Connection, MatchRule, Message, MessageStream,
        message::Type as MessageType,
        names::OwnedUniqueName,
        zvariant::{DynamicType, OwnedObjectPath, OwnedValue, Value},
    };

    use super::*;
    use crate::portal::{REQUEST_INTERFACE, mint_token, request_path};

    type EventSink = Arc<dyn Fn(MonitorEvent) + Send + Sync + 'static>;
    type Results = HashMap<String, OwnedValue>;

    pub struct SessionMonitor {
        shutdown: Sender<()>,
    }

    impl SessionMonitor {
        pub fn spawn(sink: EventSink) -> std::io::Result<Self> {
            let (shutdown, shutdown_rx) = async_channel::bounded(1);
            thread::Builder::new()
                .name("fluxer-linux-portals-session-monitor".to_string())
                .spawn(move || async_io::block_on(run(sink, shutdown_rx)))?;
            Ok(Self { shutdown })
        }

        pub fn close(&self) {
            self.shutdown.close();
        }
    }

    impl Drop for SessionMonitor {
        fn drop(&mut self) {
            self.close();
        }
    }

    struct Monitor {
        owner: OwnedUniqueName,
        handle: OwnedObjectPath,
    }

    enum Next {
        Owner(Message),
        State(Message),
        End,
    }

    async fn run(sink: EventSink, shutdown: Receiver<()>) {
        let Ok(conn) = Connection::session().await else {
            return;
        };
        let current = RefCell::new(None::<Monitor>);
        watch(&conn, &sink, &current)
            .or(async {
                let _ = shutdown.recv().await;
            })
            .await;
        if let Some(monitor) = current.take() {
            let _ = call(
                &conn,
                PORTAL_DESTINATION,
                monitor.handle.as_str(),
                SESSION_INTERFACE,
                "Close",
                &(),
                SHORT_TIMEOUT,
            )
            .await;
        }
    }

    async fn watch(conn: &Connection, sink: &EventSink, current: &RefCell<Option<Monitor>>) {
        let Ok(mut owners) = subscribe(conn, owner_rule()).await else {
            return;
        };
        let Ok(mut states) = subscribe(conn, state_rule()).await else {
            return;
        };
        let mut tracker = LockTracker::default();
        let mut attempt = true;
        loop {
            if attempt {
                attempt = false;
                let created = create_monitor(conn).await;
                current.replace(created);
            }
            let next = async {
                match owners.next().await {
                    Some(Ok(message)) => Next::Owner(message),
                    _ => Next::End,
                }
            }
            .or(async {
                match states.next().await {
                    Some(Ok(message)) => Next::State(message),
                    _ => Next::End,
                }
            })
            .await;
            match next {
                Next::End => return,
                Next::Owner(message) => {
                    let Some(new_owner) = new_portal_owner(&message) else {
                        continue;
                    };
                    let unchanged = current
                        .borrow()
                        .as_ref()
                        .is_some_and(|monitor| monitor.owner.as_str() == new_owner);
                    if unchanged {
                        continue;
                    }
                    current.replace(None);
                    attempt = !new_owner.is_empty();
                }
                Next::State(message) => {
                    let Some((handle, active, session_state)) =
                        state_change(&message, current.borrow().as_ref())
                    else {
                        continue;
                    };
                    if session_state == QUERY_END {
                        let _ = call(
                            conn,
                            PORTAL_DESTINATION,
                            PORTAL_PATH,
                            INHIBIT_INTERFACE,
                            "QueryEndResponse",
                            &(&handle,),
                            SHORT_TIMEOUT,
                        )
                        .await;
                    }
                    if let Some(event) = tracker.update(active) {
                        sink(event);
                    }
                }
            }
        }
    }

    async fn subscribe(
        conn: &Connection,
        rule: zbus::Result<MatchRule<'static>>,
    ) -> zbus::Result<MessageStream> {
        MessageStream::for_match_rule(rule?, conn, None).await
    }

    fn owner_rule() -> zbus::Result<MatchRule<'static>> {
        Ok(MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(BUS_NAME)?
            .interface(BUS_NAME)?
            .member("NameOwnerChanged")?
            .path(BUS_PATH)?
            .arg(0, PORTAL_DESTINATION)?
            .build())
    }

    fn state_rule() -> zbus::Result<MatchRule<'static>> {
        Ok(MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(PORTAL_DESTINATION)?
            .interface(INHIBIT_INTERFACE)?
            .member("StateChanged")?
            .path(PORTAL_PATH)?
            .build())
    }

    fn new_portal_owner(message: &Message) -> Option<String> {
        let header = message.header();
        if header
            .sender()
            .is_none_or(|sender| sender.as_str() != BUS_NAME)
        {
            return None;
        }
        let (_name, _old, new): (String, String, String) = message.body().deserialize().ok()?;
        Some(new)
    }

    fn state_change(
        message: &Message,
        monitor: Option<&Monitor>,
    ) -> Option<(OwnedObjectPath, bool, u32)> {
        let monitor = monitor?;
        if message.header().sender() != Some(monitor.owner.inner()) {
            return None;
        }
        let (handle, state): (OwnedObjectPath, Results) = message.body().deserialize().ok()?;
        if handle != monitor.handle {
            return None;
        }
        let active = matches!(
            state.get("screensaver-active").map(unbox),
            Some(Value::Bool(true))
        );
        let session_state = state
            .get("session-state")
            .and_then(|value| crate::kwin::integer_from_variant(unbox(value)))
            .unwrap_or(0);
        Some((handle, active, session_state))
    }

    fn unbox(value: &OwnedValue) -> &Value<'_> {
        let mut value = crate::kwin::value_of_owned(value);
        while let Value::Value(inner) = value {
            value = inner;
        }
        value
    }

    async fn create_monitor(conn: &Connection) -> Option<Monitor> {
        let owner_reply = call(
            conn,
            BUS_NAME,
            BUS_PATH,
            BUS_NAME,
            "GetNameOwner",
            &(PORTAL_DESTINATION,),
            SHORT_TIMEOUT,
        )
        .await?;
        let owner: OwnedUniqueName = owner_reply.body().deserialize().ok()?;
        let unique_name = conn.unique_name()?.to_string();
        let token = mint_token("fluxer_sm");
        let session_token = mint_token("fluxer_sm_session");
        let namespace = request_path(&unique_name, "");
        let rule = MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(PORTAL_DESTINATION)
            .and_then(|rule| rule.interface(REQUEST_INTERFACE))
            .and_then(|rule| rule.member("Response"))
            .and_then(|rule| rule.path_namespace(namespace.trim_end_matches('/').to_string()))
            .map(|rule| rule.build());
        let mut responses = subscribe(conn, rule).await.ok()?;
        let mut options: HashMap<&str, Value<'_>> = HashMap::new();
        options.insert("handle_token", Value::new(token.as_str()));
        options.insert("session_handle_token", Value::new(session_token.as_str()));
        let reply = call(
            conn,
            PORTAL_DESTINATION,
            PORTAL_PATH,
            INHIBIT_INTERFACE,
            "CreateMonitor",
            &("", options),
            SETUP_TIMEOUT,
        )
        .await?;
        let request = reply
            .body()
            .deserialize::<OwnedObjectPath>()
            .ok()
            .map(|path| path.to_string())
            .unwrap_or_else(|| request_path(&unique_name, &token));
        let response = async {
            while let Some(Ok(message)) = responses.next().await {
                let header = message.header();
                if header.sender() != Some(owner.inner())
                    || header.path().map(|path| path.as_str()) != Some(request.as_str())
                {
                    continue;
                }
                return message.body().deserialize::<(u32, Results)>().ok();
            }
            None
        }
        .or(async {
            Timer::after(SETUP_TIMEOUT).await;
            None
        })
        .await;
        let (code, results) = response?;
        if code != 0 {
            return None;
        }
        let handle = match results.get("session_handle").map(unbox)? {
            Value::Str(path) => OwnedObjectPath::try_from(path.as_str()).ok()?,
            Value::ObjectPath(path) => OwnedObjectPath::from(path.to_owned()),
            _ => return None,
        };
        Some(Monitor { owner, handle })
    }

    async fn call<B>(
        conn: &Connection,
        destination: &str,
        path: &str,
        interface: &str,
        method: &str,
        body: &B,
        timeout: Duration,
    ) -> Option<Message>
    where
        B: zbus::export::serde::Serialize + DynamicType,
    {
        async {
            conn.call_method(Some(destination), path, Some(interface), method, body)
                .await
                .ok()
        }
        .or(async {
            Timer::after(timeout).await;
            None
        })
        .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lock_tracker_reports_only_transitions() {
        let mut tracker = LockTracker::default();
        assert_eq!(tracker.update(false), None);
        assert_eq!(tracker.update(true), Some(MonitorEvent::ScreenLocked));
        assert_eq!(tracker.update(true), None);
        assert_eq!(tracker.update(false), Some(MonitorEvent::ScreenUnlocked));
        assert_eq!(tracker.update(false), None);
    }
}
