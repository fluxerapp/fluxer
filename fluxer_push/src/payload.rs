// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::job::{ClearJob, MessageJob};
use crate::metrics::PayloadShrink;
use crate::unix_seconds;
use serde_json::{Map, Value, json};

const WEB_PUSH_MARKER: u64 = 8030;
const CLEAR_TYPE: &str = "notification_clear";
const CLEAR_ACTION: &str = "clear_channel";
const FALLBACK_TAG: &str = "fluxer-message";
const FALLBACK_TITLE: &str = "Fluxer";
const APNS_CATEGORY: &str = "FLUXER_MESSAGE";
const APNS_SOUND: &str = "default";
const APNS_ALERT_EXPIRATION_SECONDS: i64 = 86_400;
const APNS_BACKGROUND_EXPIRATION_SECONDS: i64 = 3_600;
const APNS_COLLAPSE_ID_MAX_BYTES: usize = 64;
const SHRUNK_BODY_MAX_BYTES: usize = 40;
const MINIMAL_TITLE_MAX_BYTES: usize = 120;
const MEDIA_KEYS: [&str; 2] = ["image_url", "image"];
const ICON_KEYS: [&str; 3] = ["icon", "badge", "author_avatar_url"];
const MINIMAL_DATA_KEYS: [&str; 7] = [
    "channel_id",
    "message_id",
    "guild_id",
    "target_user_id",
    "notification_tag",
    "url",
    "badge_count",
];

pub fn web_push_message(job: &MessageJob, target_user_id: &str, badge_count: u32) -> Value {
    let fields = &job.notification;
    let image_url = fields.image_url.as_deref();
    let guild_id = if job.is_direct_message() {
        Value::Null
    } else {
        Value::String(job.guild_id.clone())
    };
    let mut data = json!({
        "channel_id": job.channel_id,
        "author_avatar_url": fields.icon,
        "message_id": job.message_id,
        "notification_tag": fields.notification_tag,
        "guild_id": guild_id,
        "url": fields.url,
        "badge_count": badge_count,
        "target_user_id": target_user_id,
        "has_media": image_url.is_some(),
    });
    merge_image_fields(&mut data, image_url);
    let mut notification = json!({
        "title": fields.title,
        "body": fields.body,
        "icon": fields.icon,
        "badge": fields.badge,
        "tag": fields.tag,
        "navigate": fields.url,
        "app_badge": badge_count.to_string(),
        "data": data,
    });
    merge_image_fields(&mut notification, image_url);
    let mut envelope = json!({
        "web_push": WEB_PUSH_MARKER,
        "notification": notification,
        "title": fields.title,
        "body": fields.body,
        "icon": fields.icon,
        "badge": fields.badge,
        "tag": fields.tag,
        "data": data,
    });
    merge_image_fields(&mut envelope, image_url);
    envelope
}

pub fn web_push_clear(job: &ClearJob, badge_count: u32) -> Value {
    let tag = format!("channel:{}", job.channel_id);
    let data = json!({
        "type": CLEAR_TYPE,
        "action": CLEAR_ACTION,
        "channel_id": job.channel_id,
        "message_id": job.message_id,
        "target_user_id": job.user_id,
        "notification_tag": tag,
        "tag": tag,
        "badge_count": badge_count,
    });
    json!({
        "type": CLEAR_TYPE,
        "action": CLEAR_ACTION,
        "silent": true,
        "tag": tag,
        "notification_tag": tag,
        "data": data,
        "badge_count": badge_count,
        "web_push": WEB_PUSH_MARKER,
        "notification": {
            "tag": tag,
            "data": data,
            "silent": true,
            "close": true,
        },
    })
}

pub fn fcm_message(device_token: &str, envelope: &Value) -> Value {
    if is_clear(envelope) {
        return fcm_clear_message(device_token, envelope);
    }
    fcm_notification_message(device_token, envelope)
}

fn fcm_clear_message(device_token: &str, envelope: &Value) -> Value {
    let tag = match envelope.get("notification_tag") {
        Some(value) => value.as_str().unwrap_or(FALLBACK_TAG),
        None => envelope
            .get("tag")
            .and_then(Value::as_str)
            .unwrap_or(FALLBACK_TAG),
    };
    let mut data = stringified_data(envelope.get("data"));
    data.insert("type".to_owned(), CLEAR_TYPE.into());
    data.insert("action".to_owned(), CLEAR_ACTION.into());
    data.insert("notification_tag".to_owned(), tag.into());
    json!({
        "message": {
            "token": device_token,
            "data": data,
            "android": {
                "priority": "NORMAL",
                "ttl": "3600s",
                "collapse_key": format!("clear:{tag}"),
            },
            "fcm_options": {"analytics_label": CLEAR_TYPE},
        },
    })
}

fn fcm_notification_message(device_token: &str, envelope: &Value) -> Value {
    let notification = envelope.get("notification");
    let title = sanitized_text(fcm_text(notification, envelope, "title", FALLBACK_TITLE));
    let body = sanitized_text(fcm_text(notification, envelope, "body", ""));
    let tag = envelope
        .get("tag")
        .and_then(Value::as_str)
        .unwrap_or(FALLBACK_TAG);
    let image_url = first_non_empty([
        field(envelope, "image_url"),
        notification.and_then(|value| field(value, "image")),
        notification.and_then(|value| field(value, "image_url")),
    ]);
    let mut notification_body = json!({"title": title, "body": body});
    put_image(&mut notification_body, image_url);
    let mut data = stringified_data(envelope.get("data"));
    data.insert("title".to_owned(), title.as_str().into());
    data.insert("body".to_owned(), body.as_str().into());
    data.insert("tag".to_owned(), tag.into());
    if let Some(url) = image_url {
        data.insert("image_url".to_owned(), url.into());
    }
    let mut android_notification = json!({
        "channel_id": "fluxer_default_push",
        "tag": tag,
        "click_action": APNS_CATEGORY,
    });
    put_image(&mut android_notification, image_url);
    json!({
        "message": {
            "token": device_token,
            "notification": notification_body,
            "data": data,
            "android": {
                "priority": "HIGH",
                "ttl": "86400s",
                "notification": android_notification,
            },
            "fcm_options": {"analytics_label": "message_create"},
        },
    })
}

pub fn apns_payload(envelope: &Value) -> Value {
    let data = envelope.get("data");
    let mut payload = data.and_then(Value::as_object).cloned().unwrap_or_default();
    if is_clear(envelope) {
        payload.insert("type".to_owned(), CLEAR_TYPE.into());
        payload.insert("action".to_owned(), CLEAR_ACTION.into());
        payload.insert("aps".to_owned(), json!({"content-available": 1}));
        return Value::Object(payload);
    }
    let notification = envelope.get("notification");
    let title = notification
        .and_then(|value| field(value, "title"))
        .or_else(|| field(envelope, "title"))
        .unwrap_or(FALLBACK_TITLE);
    let body = notification
        .and_then(|value| field(value, "body"))
        .or_else(|| field(envelope, "body"))
        .unwrap_or("");
    let url = data
        .and_then(|data| field(data, "url"))
        .or_else(|| notification.and_then(|value| field(value, "navigate")));
    let image_url = first_non_empty([
        field(envelope, "image_url"),
        notification.and_then(|value| field(value, "image")),
    ]);
    let thread_id = data
        .and_then(|data| field(data, "notification_tag"))
        .map(str::to_owned)
        .or_else(|| {
            data.and_then(|data| field(data, "channel_id"))
                .map(|channel_id| format!("channel:{channel_id}"))
        })
        .unwrap_or_else(|| FALLBACK_TAG.to_owned());
    let mut aps = Map::new();
    aps.insert("alert".to_owned(), json!({"title": title, "body": body}));
    aps.insert("sound".to_owned(), APNS_SOUND.into());
    aps.insert("thread-id".to_owned(), thread_id.into());
    aps.insert("category".to_owned(), APNS_CATEGORY.into());
    aps.insert("interruption-level".to_owned(), "active".into());
    aps.insert("relevance-score".to_owned(), Value::from(0.5));
    if let Some(badge) = badge_number(data.and_then(|data| data.get("badge_count"))) {
        aps.insert("badge".to_owned(), badge.into());
    }
    if image_url.is_some() {
        aps.insert("mutable-content".to_owned(), 1.into());
    }
    payload.insert("title".to_owned(), title.into());
    payload.insert("body".to_owned(), body.into());
    payload.remove("url");
    if let Some(url) = url {
        payload.insert("url".to_owned(), url.into());
    }
    payload.remove("image_url");
    if let Some(image_url) = image_url {
        payload.insert("image_url".to_owned(), image_url.into());
    }
    payload.insert("aps".to_owned(), Value::Object(aps));
    Value::Object(payload)
}

pub fn apns_delivery_headers(envelope: &Value) -> Vec<(String, String)> {
    let clear = is_clear(envelope);
    let expiration = unix_seconds()
        + if clear {
            APNS_BACKGROUND_EXPIRATION_SECONDS
        } else {
            APNS_ALERT_EXPIRATION_SECONDS
        };
    let mut headers = vec![
        (
            "apns-push-type".to_owned(),
            if clear { "background" } else { "alert" }.to_owned(),
        ),
        (
            "apns-priority".to_owned(),
            if clear { "5" } else { "10" }.to_owned(),
        ),
        ("apns-expiration".to_owned(), expiration.to_string()),
        ("content-type".to_owned(), "application/json".to_owned()),
    ];
    let collapse_id = field(envelope, "tag").or_else(|| {
        envelope
            .get("data")
            .and_then(|data| field(data, "message_id"))
    });
    if let Some(collapse_id) = collapse_id.filter(|id| id.len() <= APNS_COLLAPSE_ID_MAX_BYTES) {
        headers.push(("apns-collapse-id".to_owned(), collapse_id.to_owned()));
    }
    headers
}

fn put_image(target: &mut Value, image_url: Option<&str>) {
    let (Some(image_url), Some(object)) = (image_url, target.as_object_mut()) else {
        return;
    };
    object.insert("image".to_owned(), image_url.into());
}

fn fcm_text<'a>(
    notification: Option<&'a Value>,
    envelope: &'a Value,
    key: &str,
    fallback: &'a str,
) -> &'a str {
    notification
        .and_then(|value| value.get(key))
        .or_else(|| envelope.get(key))
        .and_then(Value::as_str)
        .unwrap_or(fallback)
}

fn stringified_data(data: Option<&Value>) -> Map<String, Value> {
    data.and_then(Value::as_object)
        .map(|data| {
            data.iter()
                .map(|(key, value)| (key.clone(), stringified(value)))
                .collect()
        })
        .unwrap_or_default()
}

fn stringified(value: &Value) -> Value {
    match value {
        Value::String(text) => Value::String(text.clone()),
        Value::Null => Value::String("null".to_owned()),
        Value::Bool(flag) => Value::String(flag.to_string()),
        Value::Number(number) => Value::String(number.to_string()),
        other => Value::String(other.to_string()),
    }
}

fn sanitized_text(value: &str) -> String {
    value
        .chars()
        .filter(|character| displayable(*character))
        .collect()
}

fn displayable(character: char) -> bool {
    match character {
        '\n' | '\t' => true,
        '\u{0}'..='\u{1f}'
        | '\u{7f}'..='\u{9f}'
        | '\u{200e}'..='\u{200f}'
        | '\u{202a}'..='\u{202e}'
        | '\u{2066}'..='\u{2069}' => false,
        _ => true,
    }
}

fn field<'a>(value: &'a Value, key: &str) -> Option<&'a str> {
    value.get(key).and_then(non_empty)
}

fn first_non_empty<const N: usize>(candidates: [Option<&str>; N]) -> Option<&str> {
    candidates.into_iter().flatten().next()
}

fn badge_number(value: Option<&Value>) -> Option<u64> {
    match value? {
        Value::Number(number) => {
            let badge = number.as_f64()?;
            badge.is_finite().then(|| badge.max(0.0).trunc() as u64)
        }
        Value::String(text) => text
            .parse::<i64>()
            .ok()
            .map(|badge| badge.max(0).unsigned_abs()),
        _ => None,
    }
}

pub fn is_clear(envelope: &Value) -> bool {
    envelope.get("type").and_then(Value::as_str) == Some(CLEAR_TYPE)
        || envelope.get("action").and_then(Value::as_str) == Some(CLEAR_ACTION)
}

pub fn fit(envelope: &Value, budget: usize) -> (Vec<u8>, Option<PayloadShrink>) {
    let serialized = serialize(envelope);
    if serialized.len() <= budget {
        return (serialized, None);
    }
    let mut working = envelope.clone();
    for step in PayloadShrink::ALL {
        working = shrink(&working, step, budget);
        let serialized = serialize(&working);
        if serialized.len() <= budget {
            return (serialized, Some(step));
        }
    }
    (serialize(&working), Some(PayloadShrink::Minimal))
}

fn shrink(envelope: &Value, step: PayloadShrink, budget: usize) -> Value {
    let mut working = envelope.clone();
    match step {
        PayloadShrink::Media => drop_keys(&mut working, &MEDIA_KEYS),
        PayloadShrink::Icons => drop_keys(&mut working, &ICON_KEYS),
        PayloadShrink::Body => truncate_body(&mut working),
        PayloadShrink::Minimal => working = minimal(envelope, budget),
    }
    working
}

fn minimal(envelope: &Value, budget: usize) -> Value {
    let title = truncate_bytes(
        first_text(envelope, "title").unwrap_or(FALLBACK_TITLE),
        MINIMAL_TITLE_MAX_BYTES,
    );
    let tag = first_text(envelope, "tag")
        .unwrap_or(FALLBACK_TAG)
        .to_owned();
    let url = first_text(envelope, "navigate")
        .or_else(|| {
            envelope
                .get("data")
                .and_then(|data| data.get("url"))
                .and_then(non_empty)
        })
        .unwrap_or_default()
        .to_owned();
    let data = minimal_data(envelope.get("data"));

    let candidates = [
        minimal_envelope(&title, &tag, &url, data.clone()),
        minimal_envelope(&title, &tag, "", data),
        minimal_envelope(&title, &tag, "", Map::new()),
        minimal_envelope(&title, "", "", Map::new()),
    ];
    for candidate in candidates {
        if serialize(&candidate).len() <= budget {
            return candidate;
        }
    }
    title_only(&title, budget)
}

fn minimal_envelope(title: &str, tag: &str, url: &str, data: Map<String, Value>) -> Value {
    json!({
        "web_push": WEB_PUSH_MARKER,
        "title": title,
        "tag": tag,
        "data": data,
        "notification": {
            "title": title,
            "tag": tag,
            "navigate": url,
            "data": data,
        },
    })
}

fn title_only(title: &str, budget: usize) -> Value {
    let mut allowance = title.len();
    loop {
        let candidate = json!({
            "web_push": WEB_PUSH_MARKER,
            "title": truncate_bytes(title, allowance),
        });
        if allowance == 0 || serialize(&candidate).len() <= budget {
            return candidate;
        }
        allowance -= 1;
    }
}

fn minimal_data(data: Option<&Value>) -> Map<String, Value> {
    let Some(data) = data.and_then(Value::as_object) else {
        return Map::new();
    };
    MINIMAL_DATA_KEYS
        .into_iter()
        .filter_map(|key| data.get(key).map(|value| (key.to_owned(), value.clone())))
        .collect()
}

fn drop_keys(envelope: &mut Value, keys: &[&str]) {
    for_each_block(envelope, &mut |block| {
        for key in keys {
            block.remove(*key);
        }
    });
}

fn truncate_body(envelope: &mut Value) {
    for_each_block(envelope, &mut |block| {
        let Some(body) = block.get("body").and_then(Value::as_str) else {
            return;
        };
        let shortened = truncate_bytes(body, SHRUNK_BODY_MAX_BYTES);
        block.insert("body".to_owned(), shortened.into());
    });
}

fn for_each_block(envelope: &mut Value, apply: &mut impl FnMut(&mut Map<String, Value>)) {
    let Some(root) = envelope.as_object_mut() else {
        return;
    };
    for key in ["notification", "data"] {
        if let Some(nested) = root.get_mut(key) {
            for_each_block(nested, apply);
        }
    }
    apply(root);
}

fn truncate_bytes(text: &str, max_bytes: usize) -> String {
    if text.len() <= max_bytes {
        return text.to_owned();
    }
    let mut end = max_bytes;
    while end > 0 && !text.is_char_boundary(end) {
        end -= 1;
    }
    text[..end].to_owned()
}

fn first_text<'a>(envelope: &'a Value, key: &str) -> Option<&'a str> {
    envelope
        .get(key)
        .and_then(non_empty)
        .or_else(|| envelope.get("notification")?.get(key).and_then(non_empty))
}

fn merge_image_fields(target: &mut Value, image_url: Option<&str>) {
    let (Some(image_url), Some(object)) = (image_url, target.as_object_mut()) else {
        return;
    };
    object.insert("image_url".to_owned(), image_url.into());
    object.insert("image".to_owned(), image_url.into());
}

fn non_empty(value: &Value) -> Option<&str> {
    value.as_str().filter(|text| !text.is_empty())
}

fn serialize(value: &Value) -> Vec<u8> {
    serde_json::to_vec(value).expect("a json value serialises")
}
