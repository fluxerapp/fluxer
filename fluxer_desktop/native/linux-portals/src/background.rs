// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(target_os = "linux")]
use std::{collections::HashMap, time::Duration};

#[cfg(target_os = "linux")]
use futures_lite::{FutureExt, StreamExt, future};
#[cfg(target_os = "linux")]
use zbus::{
    MatchRule, MessageStream, Proxy,
    message::Type as MessageType,
    zvariant::{OwnedObjectPath, Value},
};

#[cfg(target_os = "linux")]
use crate::portal::{REQUEST_INTERFACE, mint_token, request_path};

pub const PORTAL_DESTINATION: &str = "org.freedesktop.portal.Desktop";
pub const PORTAL_PATH: &str = "/org/freedesktop/portal/desktop";
pub const BACKGROUND_INTERFACE: &str = "org.freedesktop.portal.Background";

#[cfg(target_os = "linux")]
pub const REQUEST_TIMEOUT: Duration = Duration::from_secs(5 * 60);

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct RequestOptions {
    pub reason: Option<String>,
    pub autostart: bool,
    pub commandline: Vec<String>,
    pub dbus_activatable: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RequestResult {
    pub response: u32,
    pub background: bool,
    pub autostart: bool,
}

impl RequestResult {
    pub fn cancelled(&self) -> bool {
        self.response != 0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BackgroundError {
    DbusError,
    PortalTimeout,
    InvalidReply,
    SendFailed,
}

impl std::fmt::Display for BackgroundError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::DbusError => "DbusError",
            Self::PortalTimeout => "PortalTimeout",
            Self::InvalidReply => "InvalidReply",
            Self::SendFailed => "SendFailed",
        };
        f.write_str(s)
    }
}

#[cfg(target_os = "linux")]
pub fn request_background(options: RequestOptions) -> Result<RequestResult, BackgroundError> {
    future::block_on(request_background_async(options))
}

#[cfg(target_os = "linux")]
async fn request_background_async(
    options: RequestOptions,
) -> Result<RequestResult, BackgroundError> {
    let conn = zbus::Connection::session()
        .await
        .map_err(|_| BackgroundError::DbusError)?;
    let unique_owned = conn
        .unique_name()
        .ok_or(BackgroundError::DbusError)?
        .to_owned();
    let unique_name = unique_owned.as_str().to_string();
    let handle_token = mint_token("fluxer_bg");
    let expected_path = request_path(&unique_name, &handle_token);

    let rule = MatchRule::builder()
        .msg_type(MessageType::Signal)
        .interface(REQUEST_INTERFACE)
        .map_err(|_| BackgroundError::DbusError)?
        .member("Response")
        .map_err(|_| BackgroundError::DbusError)?
        .path(expected_path.clone())
        .map_err(|_| BackgroundError::DbusError)?
        .build();
    let mut stream = MessageStream::for_match_rule(rule, &conn, Some(8))
        .await
        .map_err(|_| BackgroundError::DbusError)?;

    send_call(&conn, &handle_token, &options, &expected_path).await?;

    loop {
        let timeout = async {
            async_io::Timer::after(REQUEST_TIMEOUT).await;
            None::<zbus::Result<zbus::Message>>
        };
        match stream.next().or(timeout).await {
            Some(Ok(message)) => {
                if let Some(parsed) = parse_request_response(&message) {
                    return Ok(parsed);
                }
            }
            Some(Err(_)) => return Err(BackgroundError::DbusError),
            None => return Err(BackgroundError::PortalTimeout),
        }
    }
}

#[cfg(target_os = "linux")]
fn parse_request_response(message: &zbus::Message) -> Option<RequestResult> {
    let body = message.body();
    let (response, results): (u32, HashMap<String, zbus::zvariant::OwnedValue>) =
        body.deserialize().ok()?;
    Some(RequestResult {
        response,
        background: bool_result(&results, "background").unwrap_or(false),
        autostart: bool_result(&results, "autostart").unwrap_or(false),
    })
}

#[cfg(target_os = "linux")]
fn bool_result(results: &HashMap<String, zbus::zvariant::OwnedValue>, key: &str) -> Option<bool> {
    results
        .get(key)
        .and_then(|value| bool_from_value(crate::kwin::value_of_owned(value)))
}

#[cfg(target_os = "linux")]
fn bool_from_value(value: &Value<'_>) -> Option<bool> {
    match value {
        Value::Bool(v) => Some(*v),
        Value::Value(inner) => bool_from_value(inner),
        _ => None,
    }
}

#[cfg(target_os = "linux")]
async fn send_call(
    conn: &zbus::Connection,
    handle_token: &str,
    options: &RequestOptions,
    expected_path: &str,
) -> Result<(), BackgroundError> {
    let proxy = Proxy::new(conn, PORTAL_DESTINATION, PORTAL_PATH, BACKGROUND_INTERFACE)
        .await
        .map_err(|_| BackgroundError::DbusError)?;

    let mut vardict: HashMap<&str, Value<'_>> = HashMap::new();
    vardict.insert("handle_token", Value::new(handle_token));
    vardict.insert("autostart", Value::new(options.autostart));
    if let Some(reason) = options.reason.as_deref() {
        vardict.insert("reason", Value::new(reason));
    }
    if !options.commandline.is_empty() {
        let commandline: Vec<&str> = options.commandline.iter().map(String::as_str).collect();
        vardict.insert("commandline", Value::new(commandline));
    }
    if options.dbus_activatable {
        vardict.insert("dbus-activatable", Value::new(true));
    }

    let reply_path: OwnedObjectPath = proxy
        .call("RequestBackground", &("", vardict))
        .await
        .map_err(|_| BackgroundError::SendFailed)?;
    if !reply_path.as_str().is_empty() && reply_path.as_str() != expected_path {
        return Err(BackgroundError::InvalidReply);
    }
    Ok(())
}

#[cfg(not(target_os = "linux"))]
pub fn request_background(_options: RequestOptions) -> Result<RequestResult, BackgroundError> {
    Err(BackgroundError::DbusError)
}
