// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, html};

use super::badge::{BadgeVariant, badge};
use super::media::user_avatar_url;
use crate::{
    api::types::ReportEntry,
    config::AdminConfig,
    utils::{timestamps::format_admin_timestamp, user_tag::user_tag},
};

pub struct ReportedWebhook<'a> {
    pub id: &'a str,
    pub name: String,
    pub configured_name: Option<&'a str>,
    pub configured_avatar_url: Option<String>,
    pub avatar_url: String,
    pub reports_href: String,
    pub kind: Option<String>,
    pub created_at: Option<String>,
    pub channel_id: Option<&'a str>,
    pub guild_id: Option<&'a str>,
    pub application: Option<WebhookApplication<'a>>,
    pub record_deleted: bool,
    pub creator: Option<WebhookCreator<'a>>,
}

pub struct WebhookApplication<'a> {
    pub id: &'a str,
    pub href: String,
}

pub struct WebhookCreator<'a> {
    pub id: &'a str,
    pub label: String,
    pub avatar_url: String,
    pub href: String,
    pub account_deleted: bool,
    pub bot: bool,
}

pub fn webhook_type_label(webhook_type: i32) -> String {
    match webhook_type {
        1 => "Incoming".to_owned(),
        2 => "Channel Follower".to_owned(),
        other => format!("Type {other}"),
    }
}

fn non_blank(value: Option<&str>) -> Option<&str> {
    value.filter(|value| !value.trim().is_empty())
}

pub fn reported_webhook<'a>(
    config: &AdminConfig,
    report: &'a ReportEntry,
) -> Option<ReportedWebhook<'a>> {
    let id = report.reported_webhook_id.as_deref()?;
    let name = non_blank(report.reported_webhook_name.as_deref())
        .map_or_else(|| format!("Webhook {id}"), str::to_owned);
    let configured_name = non_blank(report.reported_webhook_default_name.as_deref())
        .filter(|configured| *configured != name);
    let channel_id = non_blank(report.reported_webhook_channel_id.as_deref());
    let guild_id = non_blank(report.reported_webhook_guild_id.as_deref());
    let record_present = report.reported_webhook_type.is_some()
        || channel_id.is_some()
        || guild_id.is_some()
        || non_blank(report.reported_webhook_default_name.as_deref()).is_some();
    let configured_avatar_hash = non_blank(report.reported_webhook_default_avatar_hash.as_deref());
    let configured_avatar_url = (record_present
        && configured_avatar_hash != non_blank(report.reported_webhook_avatar_hash.as_deref()))
    .then(|| user_avatar_url(config, id, configured_avatar_hash, 80, true));
    let created_at = non_blank(report.reported_webhook_created_at.as_deref());
    Some(ReportedWebhook {
        id,
        name,
        configured_name,
        configured_avatar_url,
        avatar_url: user_avatar_url(
            config,
            id,
            report.reported_webhook_avatar_hash.as_deref(),
            80,
            true,
        ),
        reports_href: format!(
            "{}/reports?reported_webhook_id={}",
            config.base_path,
            urlencoding::encode(id)
        ),
        kind: report.reported_webhook_type.map(webhook_type_label),
        created_at: created_at.map(format_admin_timestamp),
        channel_id,
        guild_id,
        application: non_blank(report.reported_webhook_application_id.as_deref()).map(|id| {
            WebhookApplication {
                id,
                href: format!(
                    "{}/applications/{}",
                    config.base_path,
                    urlencoding::encode(id)
                ),
            }
        }),
        record_deleted: created_at.is_some() && !record_present,
        creator: webhook_creator(config, report),
    })
}

fn webhook_creator<'a>(
    config: &AdminConfig,
    report: &'a ReportEntry,
) -> Option<WebhookCreator<'a>> {
    let id = report.reported_webhook_creator_id.as_deref()?;
    let username = non_blank(report.reported_webhook_creator_username.as_deref());
    let bot = report.reported_webhook_application_id.as_deref() == Some(id);
    let tag = match username {
        Some(username) => Some(user_tag(
            username,
            report
                .reported_webhook_creator_discriminator
                .as_deref()
                .unwrap_or("0000"),
            bot,
        )),
        None => non_blank(report.reported_webhook_creator_tag.as_deref()).map(str::to_owned),
    };
    let label = match (
        non_blank(report.reported_webhook_creator_global_name.as_deref()),
        tag,
    ) {
        (Some(display), Some(tag)) => format!("{display} ({tag})"),
        (_, Some(tag)) => tag,
        (Some(display), None) => display.to_owned(),
        (None, None) => format!("User {id}"),
    };
    Some(WebhookCreator {
        id,
        label,
        avatar_url: user_avatar_url(
            config,
            id,
            report.reported_webhook_creator_avatar_hash.as_deref(),
            80,
            true,
        ),
        href: format!("{}/users/{}", config.base_path, urlencoding::encode(id)),
        account_deleted: username.is_none()
            && non_blank(report.reported_webhook_creator_tag.as_deref()).is_none()
            && non_blank(report.reported_webhook_creator_global_name.as_deref()).is_none(),
        bot,
    })
}

pub fn webhook_identity(webhook: &ReportedWebhook<'_>) -> Markup {
    html! {
        span class="inline-flex min-w-0 items-center gap-2" data-report-webhook=(webhook.id) {
            img src=(webhook.avatar_url) alt=(format!("{}'s avatar", webhook.name))
                class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
            span class="flex min-w-0 flex-wrap items-center gap-x-1.5 gap-y-0.5" {
                span class="font-medium text-neutral-900 text-sm break-words" { (webhook.name) }
                (badge("Webhook", BadgeVariant::Info))
            }
        }
    }
}

pub fn webhook_creator_link(creator: &WebhookCreator<'_>) -> Markup {
    html! {
        a href=(creator.href) title=(creator.id) data-report-webhook-creator=(creator.id)
            class="inline-flex min-w-0 items-center gap-2 text-neutral-900 underline decoration-neutral-300 hover:text-neutral-600 hover:decoration-neutral-500" {
            img src=(creator.avatar_url) alt=(format!("{}'s avatar", creator.label))
                class="h-8 w-8 flex-shrink-0 rounded-full object-cover";
            span class="break-words" { (creator.label) }
        }
    }
}

pub fn webhook_creator_badges(creator: &WebhookCreator<'_>) -> Markup {
    html! {
        @if creator.bot {
            span class="inline-flex" data-report-webhook-creator-bot { (badge("Bot", BadgeVariant::Info)) }
        }
        @if creator.account_deleted {
            span class="inline-flex" data-report-webhook-creator-deleted { (badge("Account Deleted", BadgeVariant::Default)) }
        }
    }
}
