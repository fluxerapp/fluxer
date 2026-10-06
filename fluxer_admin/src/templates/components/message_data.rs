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
    pub ncmec_status: String,
    pub ncmec_report_id: Option<String>,
    pub ncmec_failure_reason: Option<String>,
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
    pub channel_id: String,
    pub channel_nsfw: Option<bool>,
    pub channel_content_warning_level: Option<i32>,
    pub channel_content_warning_text: Option<String>,
    pub guild_nsfw: Option<bool>,
    pub attachments: Vec<Attachment>,
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
        ncmec_status: value["ncmec_status"]
            .as_str()
            .unwrap_or("not_submitted")
            .to_owned(),
        ncmec_report_id: value["ncmec_report_id"].as_str().map(ToOwned::to_owned),
        ncmec_failure_reason: value["ncmec_failure_reason"]
            .as_str()
            .map(ToOwned::to_owned),
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

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn maps_message_and_attachment_fields() {
        let value = json!({
            "id": "9007199254740993",
            "content": "Message content",
            "timestamp": "2026-09-11T12:00:00Z",
            "author_id": 42,
            "author_username": "alice",
            "author_global_name": "Alice",
            "author_discriminator": 1234,
            "author_avatar": "avatar-hash",
            "channel_id": "100",
            "channel_nsfw": false,
            "channel_content_warning_level": 2,
            "channel_content_warning_text": "Content warning",
            "guild_nsfw": true,
            "attachments": [{
                "id": 9007199254740993_u64,
                "url": "https://cdn.example.com/image.png",
                "filename": "image.png",
                "nsfw": true,
                "content_type": "image/png",
                "width": 640,
                "height": 480,
                "size": 4096,
                "ncmec_status": "submitted",
                "ncmec_report_id": "report-id",
                "ncmec_failure_reason": "previous failure"
            }]
        });

        assert_eq!(
            message_from_value(&value),
            Message {
                id: "9007199254740993".into(),
                content: "Message content".into(),
                timestamp: "2026-09-11T12:00:00Z".into(),
                author_id: "42".into(),
                author_username: "alice".into(),
                author_global_name: Some("Alice".into()),
                author_discriminator: "1234".into(),
                author_avatar: Some("avatar-hash".into()),
                channel_id: "100".into(),
                channel_nsfw: Some(false),
                channel_content_warning_level: Some(2),
                channel_content_warning_text: Some("Content warning".into()),
                guild_nsfw: Some(true),
                attachments: vec![Attachment {
                    id: "9007199254740993".into(),
                    url: "https://cdn.example.com/image.png".into(),
                    filename: "image.png".into(),
                    nsfw: Some(true),
                    content_type: Some("image/png".into()),
                    width: Some(640),
                    height: Some(480),
                    size: Some(4096),
                    ncmec_status: "submitted".into(),
                    ncmec_report_id: Some("report-id".into()),
                    ncmec_failure_reason: Some("previous failure".into()),
                }],
            }
        );
    }

    #[test]
    fn retains_display_defaults_for_incomplete_snapshots() {
        let message = message_from_value(&json!({"attachments": [{}]}));

        assert_eq!(message.author_username, "Unknown");
        assert_eq!(message.author_discriminator, "0000");
        assert_eq!(message.content, "");
        assert_eq!(message.author_global_name, None);
        assert_eq!(message.channel_nsfw, None);
        assert_eq!(
            message.attachments,
            vec![Attachment {
                id: String::new(),
                url: String::new(),
                filename: String::new(),
                nsfw: None,
                content_type: None,
                width: None,
                height: None,
                size: None,
                ncmec_status: "not_submitted".into(),
                ncmec_report_id: None,
                ncmec_failure_reason: None,
            }]
        );
    }

    #[test]
    fn orders_string_and_numeric_snowflakes_without_rounding() {
        let values = vec![
            json!({"id": "9007199254740993"}),
            json!({"id": "10"}),
            json!({"id": 2}),
            json!({"id": 9007199254740992_u64}),
        ];
        let messages = ordered_messages(&values);
        let ids: Vec<&str> = messages.iter().map(|message| message.id.as_str()).collect();

        assert_eq!(ids, ["2", "10", "9007199254740992", "9007199254740993"]);
        assert_eq!(values[0]["id"], "9007199254740993");
        assert!(ordered_messages(&[]).is_empty());
    }

    #[test]
    fn preserves_message_order_when_ids_are_equal() {
        let values = [
            json!({"id": "10", "content": "first"}),
            json!({"id": 10, "content": "second"}),
        ];
        let messages = ordered_messages(&values);

        assert_eq!(messages[0].content, "first");
        assert_eq!(messages[1].content, "second");
    }
}
