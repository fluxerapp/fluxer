use std::cmp::Ordering;

use serde_json::Value;

#[derive(Debug, PartialEq)]
pub struct Attachment {
    pub id: String,
    pub url: String,
    pub filename: String,
    pub nsfw: Option<bool>,
    pub content_type: Option<String>,
    pub width: Option<u32>,
    pub height: Option<u32>,
    pub size: Option<u64>,
}

#[derive(Debug, PartialEq)]
pub struct MissingAttachment {
    pub id: String,
    pub filename: String,
    pub content_type: Option<String>,
    pub size: Option<u64>,
}

#[derive(Debug, PartialEq)]
pub struct Message {
    pub id: String,
    pub content: String,
    pub timestamp: String,
    pub author_id: String,
    pub author_username: String,
    pub author_global_name: Option<String>,
    pub author_discriminator: String,
    pub author_avatar: Option<String>,
    pub webhook_id: Option<String>,
    pub author_bot: Option<bool>,
    pub channel_id: String,
    pub channel_nsfw: Option<bool>,
    pub channel_content_warning_level: Option<i32>,
    pub channel_content_warning_text: Option<String>,
    pub guild_nsfw: Option<bool>,
    pub attachments: Vec<Attachment>,
    pub missing_attachments: Vec<MissingAttachment>,
}

pub fn ordered_messages(values: &[Value]) -> Vec<Message> {
    let mut messages: Vec<Message> = values.iter().map(message_from_value).collect();
    messages.sort_by(compare_message_ids);
    messages
}

fn message_from_value(value: &Value) -> Message {
    let attachments = value["attachments"]
        .as_array()
        .into_iter()
        .flatten()
        .map(attachment_from_value)
        .collect();
    let missing_attachments = value["missing_attachments"]
        .as_array()
        .into_iter()
        .flatten()
        .map(missing_attachment_from_value)
        .collect();
    Message {
        id: value_id(&value["id"]).unwrap_or_default(),
        content: value["content"].as_str().unwrap_or("").to_owned(),
        timestamp: value["timestamp"].as_str().unwrap_or("").to_owned(),
        author_id: value_id(&value["author_id"]).unwrap_or_default(),
        author_username: value["author_username"]
            .as_str()
            .unwrap_or("Unknown")
            .to_owned(),
        author_global_name: value["author_global_name"].as_str().map(ToOwned::to_owned),
        author_discriminator: value_id(&value["author_discriminator"])
            .unwrap_or_else(|| "0000".to_owned()),
        author_avatar: value["author_avatar"].as_str().map(ToOwned::to_owned),
        webhook_id: value_id(&value["webhook_id"]),
        author_bot: value["author_bot"].as_bool(),
        channel_id: value_id(&value["channel_id"]).unwrap_or_default(),
        channel_nsfw: value["channel_nsfw"].as_bool(),
        channel_content_warning_level: value["channel_content_warning_level"]
            .as_i64()
            .map(|n| n as i32),
        channel_content_warning_text: value["channel_content_warning_text"]
            .as_str()
            .map(ToOwned::to_owned),
        guild_nsfw: value["guild_nsfw"].as_bool(),
        attachments,
        missing_attachments,
    }
}

fn missing_attachment_from_value(value: &Value) -> MissingAttachment {
    MissingAttachment {
        id: value_id(&value["id"]).unwrap_or_default(),
        filename: value["filename"].as_str().unwrap_or("").to_owned(),
        content_type: value["content_type"].as_str().map(ToOwned::to_owned),
        size: value["size"].as_u64(),
    }
}

fn attachment_from_value(value: &Value) -> Attachment {
    Attachment {
        id: value_id(&value["id"]).unwrap_or_default(),
        url: value["url"].as_str().unwrap_or("").to_owned(),
        filename: value["filename"].as_str().unwrap_or("").to_owned(),
        nsfw: value["nsfw"].as_bool(),
        content_type: value["content_type"].as_str().map(ToOwned::to_owned),
        width: value["width"].as_u64().map(|n| n as u32),
        height: value["height"].as_u64().map(|n| n as u32),
        size: value["size"].as_u64(),
    }
}

pub(crate) fn value_id(value: &Value) -> Option<String> {
    match value {
        Value::String(s) => Some(s.clone()),
        Value::Number(n) => Some(n.to_string()),
        _ => None,
    }
}

fn compare_message_ids(left: &Message, right: &Message) -> Ordering {
    match (left.id.parse::<u128>(), right.id.parse::<u128>()) {
        (Ok(l), Ok(r)) => l.cmp(&r),
        _ => left.id.cmp(&right.id),
    }
}
