// SPDX-License-Identifier: AGPL-3.0-or-later

pub const LAUNCHER_ENTRY_PATH: &str = "/com/canonical/unity/launcherentry";
pub const LAUNCHER_ENTRY_INTERFACE: &str = "com.canonical.Unity.LauncherEntry";
pub const UNITY_SERVICE: &str = "com.canonical.Unity";

#[cfg(target_os = "linux")]
pub use emitter::LauncherEntryEmitter;

#[cfg(target_os = "linux")]
mod emitter {
    use std::{collections::HashMap, thread};

    use zbus::{
        MatchRule,
        blocking::{Connection, MessageIterator},
        message::Type as MessageType,
        zvariant::Value,
    };

    use super::*;
    use crate::session_monitor::{BUS_NAME, BUS_PATH};

    pub struct LauncherEntryEmitter {
        connection: Connection,
    }

    impl LauncherEntryEmitter {
        pub fn connect() -> zbus::Result<Self> {
            Ok(Self {
                connection: Connection::session()?,
            })
        }

        pub fn update(&self, uri: &str, count: i64, count_visible: bool) -> zbus::Result<()> {
            let properties = HashMap::from([
                ("count", Value::from(count)),
                ("count-visible", Value::from(count_visible)),
            ]);
            self.connection.emit_signal(
                None::<&str>,
                LAUNCHER_ENTRY_PATH,
                LAUNCHER_ENTRY_INTERFACE,
                "Update",
                &(uri, properties),
            )
        }

        pub fn watch_shell(
            &self,
            on_shell_started: impl Fn() + Send + 'static,
        ) -> zbus::Result<()> {
            let messages =
                MessageIterator::for_match_rule(unity_owner_rule()?, &self.connection, None)?;
            thread::Builder::new()
                .name("fluxer-linux-portals-launcher-entry".to_string())
                .spawn(move || {
                    for message in messages.flatten() {
                        let Ok((_name, _old, new)) =
                            message.body().deserialize::<(String, String, String)>()
                        else {
                            continue;
                        };
                        if !new.is_empty() {
                            on_shell_started();
                        }
                    }
                })
                .map_err(|err| zbus::Error::Failure(err.to_string()))?;
            Ok(())
        }
    }

    fn unity_owner_rule() -> zbus::Result<MatchRule<'static>> {
        Ok(MatchRule::builder()
            .msg_type(MessageType::Signal)
            .sender(BUS_NAME)?
            .interface(BUS_NAME)?
            .member("NameOwnerChanged")?
            .path(BUS_PATH)?
            .arg(0, UNITY_SERVICE)?
            .build())
    }
}
