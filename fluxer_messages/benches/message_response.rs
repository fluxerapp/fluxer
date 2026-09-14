// SPDX-License-Identifier: AGPL-3.0-or-later

use criterion::{Criterion, criterion_group, criterion_main};
use fluxer_messages::mention_extractor::extract_mentions_from_markdown;
use fluxer_messages::types::{
    ApiEmbedFieldResponse, ApiEmbedMediaResponse, ApiMessageAttachmentResponse,
    ApiMessageEmbedChildResponse, ApiMessageEmbedResponse, ApiMessageReactionResponse,
    ApiMessageReferenceResponse, ApiMessageResponse, ApiReactionEmojiResponse,
    ApiUserPartialResponse,
};
use std::hint::black_box;

fn message_corpus() -> Vec<String> {
    (0..5_000)
        .map(|index| {
            format!(
                "message {index} <@{}> <@!{}> <#{}> <@&{}> `ignored <@{}>` [visible <@{}>](https://example.com/<@{}>)",
                10_000 + index,
                20_000 + index,
                30_000 + index,
                40_000 + index,
                50_000 + index,
                60_000 + index,
                70_000 + index
            )
        })
        .collect()
}

fn sample_user(id: u64) -> ApiUserPartialResponse {
    ApiUserPartialResponse {
        id: id.to_string(),
        username: format!("user{id}"),
        discriminator: "0000".to_owned(),
        global_name: Some(format!("User {id}")),
        avatar: Some(format!("avatar-{id}")),
        avatar_color: Some(0x336699),
        bot: None,
        system: None,
        flags: 0,
        mention_flags: None,
    }
}

fn sample_message(index: u64) -> ApiMessageResponse {
    let user = sample_user(10_000 + index);
    ApiMessageResponse {
        id: (1_000_000 + index).to_string(),
        channel_id: "123456789".to_owned(),
        author: user.clone(),
        webhook_id: None,
        message_type: 0,
        flags: 0,
        content: format!("hello <@{}> <#{}>", user.id, 30_000 + index),
        timestamp: "2026-01-01T00:00:00.000Z".to_owned(),
        edited_timestamp: None,
        pinned: false,
        mention_everyone: false,
        tts: false,
        mentions: vec![user.clone()],
        mention_roles: vec![(40_000 + index).to_string()],
        mention_channels: None,
        users: Some(vec![sample_user(60_000 + index)]),
        embeds: vec![ApiMessageEmbedResponse {
            base: ApiMessageEmbedChildResponse {
                embed_type: "rich".to_owned(),
                title: Some(format!("embed {index}")),
                description: Some(format!("embed mentions <@{}>", 70_000 + index)),
                url: Some("https://example.com".to_owned()),
                timestamp: None,
                color: Some(0x123456),
                author: None,
                provider: None,
                thumbnail: Some(ApiEmbedMediaResponse {
                    url: "https://cdn.example.com/thumb.png".to_owned(),
                    proxy_url: "https://media.example.com/external/sign/v2/path".to_owned(),
                    width: Some(320),
                    height: Some(180),
                    duration: None,
                    description: Some("thumbnail".to_owned()),
                    content_type: Some("image/png".to_owned()),
                    content_hash: None,
                    placeholder: None,
                    flags: Some(0),
                }),
                image: None,
                video: None,
                audio: None,
                footer: None,
                fields: Some(vec![ApiEmbedFieldResponse {
                    name: "field".to_owned(),
                    value: "value".to_owned(),
                    is_inline: false,
                }]),
                nsfw: None,
                html: None,
                html_width: None,
                html_height: None,
            },
            children: None,
        }],
        attachments: vec![ApiMessageAttachmentResponse {
            id: (80_000 + index).to_string(),
            filename: "image.png".to_owned(),
            title: None,
            description: None,
            content_type: Some("image/png".to_owned()),
            content_hash: Some("hash".to_owned()),
            size: 123_456,
            url: Some("https://media.example.com/attachments/1/2/image.png".to_owned()),
            proxy_url: Some("https://media.example.com/attachments/1/2/image.png".to_owned()),
            width: Some(640),
            height: Some(480),
            placeholder: None,
            flags: 0,
            nsfw: None,
            duration: None,
            waveform: None,
            expires_at: None,
            expired: None,
        }],
        stickers: Vec::new(),
        reactions: Some(vec![ApiMessageReactionResponse {
            emoji: ApiReactionEmojiResponse {
                id: None,
                name: "thumbsup".to_owned(),
                animated: None,
            },
            count: 3,
            me: Some(true),
        }]),
        message_reference: Some(ApiMessageReferenceResponse {
            channel_id: "123456789".to_owned(),
            message_id: (900_000 + index).to_string(),
            guild_id: Some("987654321".to_owned()),
            reference_type: 0,
        }),
        message_snapshots: None,
        nonce: None,
        call: None,
        referenced_message: None,
    }
}

fn bench_extract_markdown_mentions(c: &mut Criterion) {
    let corpus = message_corpus();
    c.bench_function(
        "extract markdown mentions for 5000 message page corpus",
        |b| {
            b.iter(|| {
                let mut total = 0usize;
                for content in &corpus {
                    let mentions = extract_mentions_from_markdown(Some(content));
                    total += mentions.users.len() + mentions.roles.len() + mentions.channels.len();
                }
                black_box(total);
            });
        },
    );
}

fn bench_serialize_message_page(c: &mut Criterion) {
    let messages = (0..50).map(sample_message).collect::<Vec<_>>();
    c.bench_function("serialize 50 api message responses", |b| {
        b.iter(|| {
            let bytes = serde_json::to_vec(black_box(&messages)).expect("serializes");
            black_box(bytes);
        });
    });
}

criterion_group!(
    benches,
    bench_extract_markdown_mentions,
    bench_serialize_message_page
);
criterion_main!(benches);
