/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

use anyhow::Result;
use serde::Serialize;
use time::OffsetDateTime;
use time::format_description::well_known::Rfc3339;
use tracing::info;

use crate::db::CrashEventData;

#[derive(Serialize)]
struct DiscordWebhookPayload {
    embeds: Vec<DiscordEmbed>,
}

#[derive(Serialize)]
struct DiscordEmbed {
    title: String,
    description: String,
    color: u32,
    fields: Vec<DiscordField>,
    timestamp: String,
    footer: DiscordFooter,
}

#[derive(Serialize)]
struct DiscordField {
    name: String,
    value: String,
    inline: bool,
}

#[derive(Serialize)]
struct DiscordFooter {
    text: String,
}

pub async fn send_discord_crash_alert(
    webhook_url: &str,
    crash: &CrashEventData,
    admin_endpoint: Option<&str>,
) -> Result<()> {
    let timestamp =
        OffsetDateTime::from_unix_timestamp_nanos(i128::from(crash.timestamp) * 1_000_000)
            .unwrap_or_else(|_| OffsetDateTime::now_utc());

    let stacktrace = if crash.stacktrace.len() > 1000 {
        format!("{}...\n\n(truncated)", &crash.stacktrace[..1000])
    } else {
        crash.stacktrace.clone()
    };

    let guild_link = admin_endpoint.map_or_else(
        || format!("Guild ID: {}", crash.guild_id),
        |ep| format!("{ep}/guilds/{}", crash.guild_id),
    );

    let payload = DiscordWebhookPayload {
        embeds: vec![DiscordEmbed {
            title: "Guild Crash Detected".to_string(),
            description: format!(
                "A guild process has crashed on the gateway.\n\n**Guild:** {guild_link}"
            ),
            color: 0x00ED_4245,
            fields: vec![
                DiscordField {
                    name: "Guild ID".to_string(),
                    value: format!("`{}`", crash.guild_id),
                    inline: true,
                },
                DiscordField {
                    name: "Crash ID".to_string(),
                    value: format!("`{}`", crash.id),
                    inline: true,
                },
                DiscordField {
                    name: "Stacktrace".to_string(),
                    value: format!("```\n{stacktrace}\n```"),
                    inline: false,
                },
            ],
            timestamp: timestamp.format(&Rfc3339).unwrap_or_default(),
            footer: DiscordFooter {
                text: "Fluxer Metrics".to_string(),
            },
        }],
    };

    let client = reqwest::Client::new();
    let response = client.post(webhook_url).json(&payload).send().await?;

    if response.status().is_success() {
        info!("Discord crash alert sent for guild {}", crash.guild_id);
    } else {
        let status = response.status();
        let body = response.text().await.unwrap_or_default();
        anyhow::bail!("Discord webhook failed with status {status}: {body}");
    }

    Ok(())
}
