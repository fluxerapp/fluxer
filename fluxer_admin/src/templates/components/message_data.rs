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

#[derive(Default, Debug, PartialEq)]
pub struct PollEmoji {
    pub id: Option<String>,
    pub name: Option<String>,
}

#[derive(Default, Debug, PartialEq)]
pub struct PollMedia {
    pub emoji: Option<PollEmoji>,
    pub text: Option<String>,
}

#[derive(Default, Debug, PartialEq)]
pub struct PollAnswer {
    pub answer_id: Option<i32>,
    pub poll_media: Option<PollMedia>,
}

#[derive(Default, Debug, PartialEq)]
pub struct PollAnswerCount {
    pub id: Option<i32>,
    pub count: Option<i32>,
}

#[derive(Default, Debug, PartialEq)]
pub struct PollResults {
    pub answer_counts: Option<Vec<PollAnswerCount>>,
    pub is_finalized: Option<bool>,
}

#[derive(Debug, PartialEq)]
pub struct Poll {
    pub question: Option<PollMedia>,
    pub answers: Option<Vec<PollAnswer>>,
    pub expiry: Option<String>,
    pub allow_multiselect: Option<bool>,
    pub layout_type: Option<i32>,
    pub results: Option<PollResults>,
}

#[derive(Debug, PartialEq)]
pub struct EmbedField {
    pub name: String,
    pub value: String,
    pub inline: bool,
}

#[derive(Debug, PartialEq)]
pub struct Embed {
    pub title: Option<String>,
    pub url: Option<String>,
    pub description: Option<String>,
    pub author_name: Option<String>,
    pub fields: Vec<EmbedField>,
    pub footer_text: Option<String>,
    pub image_url: Option<String>,
    pub thumbnail_url: Option<String>,
    pub nsfw: bool,
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
    pub guild_id: Option<String>,
    pub guild_nsfw: Option<bool>,
    pub poll: Option<Poll>,
    pub attachments: Vec<Attachment>,
    pub missing_attachments: Vec<MissingAttachment>,
    pub embeds: Vec<Embed>,
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
    let embeds = value["embeds"]
        .as_array()
        .into_iter()
        .flatten()
        .map(embed_from_value)
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
        guild_id: value_id(&value["guild_id"]),
        guild_nsfw: value["guild_nsfw"].as_bool(),
        poll: value.get("poll").map(poll_from_value),
        attachments,
        missing_attachments,
        embeds,
    }
}

fn embed_from_value(value: &Value) -> Embed {
    Embed {
        title: non_empty_str(&value["title"]),
        url: non_empty_str(&value["url"]),
        description: non_empty_str(&value["description"]),
        author_name: non_empty_str(&value["author"]["name"]),
        fields: value["fields"]
            .as_array()
            .into_iter()
            .flatten()
            .map(|field| EmbedField {
                name: field["name"].as_str().unwrap_or("").to_owned(),
                value: field["value"].as_str().unwrap_or("").to_owned(),
                inline: field["inline"].as_bool().unwrap_or(false),
            })
            .collect(),
        footer_text: non_empty_str(&value["footer"]["text"]),
        image_url: embed_media_url(&value["image"]),
        thumbnail_url: embed_media_url(&value["thumbnail"]),
        nsfw: value["nsfw"].as_bool().unwrap_or(false),
    }
}

fn embed_media_url(value: &Value) -> Option<String> {
    non_empty_str(&value["proxy_url"]).or_else(|| non_empty_str(&value["url"]))
}

fn non_empty_str(value: &Value) -> Option<String> {
    value
        .as_str()
        .filter(|s| !s.is_empty())
        .map(ToOwned::to_owned)
}

fn missing_attachment_from_value(value: &Value) -> MissingAttachment {
    MissingAttachment {
        id: value_id(&value["id"]).unwrap_or_default(),
        filename: value["filename"].as_str().unwrap_or("").to_owned(),
        content_type: value["content_type"].as_str().map(ToOwned::to_owned),
        size: value["size"].as_u64(),
    }
}

pub fn poll_from_value(value: &Value) -> Poll {
    Poll {
        question: value.get("question").map(poll_media_from_value),
        answers: value
            .get("answers")
            .and_then(Value::as_array)
            .map(|answers| answers.iter().map(poll_answer_from_value).collect()),
        expiry: value
            .get("expiry")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        allow_multiselect: value.get("allow_multiselect").and_then(Value::as_bool),
        layout_type: value
            .get("layout_type")
            .and_then(Value::as_i64)
            .and_then(|id| i32::try_from(id).ok()),
        results: value.get("results").map(poll_results_from_value),
    }
}

fn poll_media_from_value(value: &Value) -> PollMedia {
    PollMedia {
        emoji: value.get("emoji").map(poll_emoji_from_value),
        text: value
            .get("text")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
    }
}

fn poll_emoji_from_value(value: &Value) -> PollEmoji {
    PollEmoji {
        id: value
            .get("id")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        name: value
            .get("name")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
    }
}

fn poll_answer_from_value(value: &Value) -> PollAnswer {
    PollAnswer {
        answer_id: value
            .get("answer_id")
            .and_then(Value::as_i64)
            .and_then(|id| i32::try_from(id).ok()),
        poll_media: value.get("poll_media").map(poll_media_from_value),
    }
}

fn poll_results_from_value(value: &Value) -> PollResults {
    PollResults {
        answer_counts: value
            .get("answer_counts")
            .and_then(Value::as_array)
            .map(|answer_counts| {
                (answer_counts.iter())
                    .map(poll_answer_count_from_value)
                    .collect()
            }),
        is_finalized: value.get("is_finalized").and_then(Value::as_bool),
    }
}

fn poll_answer_count_from_value(value: &Value) -> PollAnswerCount {
    PollAnswerCount {
        id: value
            .get("id")
            .and_then(Value::as_i64)
            .and_then(|id| i32::try_from(id).ok()),
        count: value
            .get("count")
            .and_then(Value::as_i64)
            .and_then(|id| i32::try_from(id).ok()),
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
