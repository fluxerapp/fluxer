// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

use super::badge::{BadgeVariant, badge};
use super::enforcement_fields::{TakeAction, take_action_href};
use super::icons::paperclip_icon;
use super::media::user_avatar_url;
use super::nsfw_indicators::{attachment_nsfw_badge, channel_nsfw_state_badge};
use super::user_display::format_user_display;
use crate::config::AdminConfig;
use crate::utils::timestamps::format_admin_timestamp;

use super::message_data::{Attachment, Message};

fn is_image(att: &Attachment) -> bool {
    att.content_type
        .as_deref()
        .is_some_and(|ct| ct.starts_with("image/"))
}

fn render_image_attachments(msg: &Message, include_delete: bool) -> Markup {
    let mut images = msg.attachments.iter().filter(|a| is_image(a)).peekable();
    if images.peek().is_none() {
        return html! {};
    }
    let spacer = if !msg.content.is_empty() {
        "mt-2 space-y-3"
    } else {
        "mt-1 space-y-3"
    };
    html! {
        div class=(spacer) {
            @for att in images {
                div class="max-w-xl overflow-hidden rounded-xl border border-neutral-200 bg-neutral-50" {
                    a href=(att.url) target="_blank" rel="noopener noreferrer"
                      class="block overflow-hidden bg-neutral-100" {
                        img src=(att.url) alt=(att.filename) loading="lazy"
                            class="block max-h-96 w-full scale-110 object-contain blur-2xl \
                                   transition-[filter,transform] duration-150 \
                                   hover:scale-100 hover:blur-none";
                    }
                    div class="space-y-2 p-3" {
                        div class="flex flex-wrap items-center gap-2 text-xs" {
                            a href=(att.url) target="_blank" rel="noopener noreferrer"
                              class="font-medium text-blue-600 hover:underline" { (att.filename) }
                            (attachment_nsfw_badge(att.nsfw.unwrap_or(false)))
                        }
                        @if include_delete {
                            div class="flex flex-wrap gap-2" {
                                button type="button"
                                    class="delete-message-btn rounded bg-white px-2.5 py-1 \
                                           text-red-600 text-xs shadow-sm ring-1 ring-neutral-200 \
                                           transition-colors hover:bg-red-50 hover:text-red-700"
                                    data-channel-id=(msg.channel_id)
                                    data-message-id=(msg.id) { "Delete" }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn render_other_attachments(msg: &Message, has_content_or_images: bool) -> Markup {
    let mut others = msg.attachments.iter().filter(|a| !is_image(a)).peekable();
    if others.peek().is_none() {
        return html! {};
    }
    let spacer = if has_content_or_images {
        "mt-1.5 space-y-1"
    } else {
        "space-y-1"
    };
    html! {
        div class=(spacer) {
            @for att in others {
                div class="flex flex-wrap items-center gap-2 text-xs" {
                    (paperclip_icon("text-neutral-400"))
                    a href=(att.url) target="_blank" rel="noopener noreferrer"
                      class="text-blue-600 hover:underline" { (att.filename) }
                    (attachment_nsfw_badge(att.nsfw.unwrap_or(false)))
                }
            }
        }
    }
}

fn render_missing_attachments(msg: &Message) -> Markup {
    if msg.missing_attachments.is_empty() {
        return html! {};
    }
    html! {
        div class="mt-1.5 space-y-1" {
            @for att in &msg.missing_attachments {
                div class="flex flex-wrap items-center gap-2 text-amber-800 text-xs"
                    data-missing-attachment=(att.id) {
                    (paperclip_icon("text-amber-500"))
                    span class="break-words" {
                        @if att.filename.is_empty() {
                            "An attachment was not preserved in the report snapshot"
                        } @else {
                            (att.filename) " was not preserved in the report snapshot"
                        }
                    }
                }
            }
        }
    }
}

fn copy_button(value: &str, label: &str) -> Markup {
    html! {
        button type="button"
            class="cursor-pointer text-blue-600 hover:underline"
            data-copy-value=(value)
            onclick="window.__adminCopyToClipboard && window.__adminCopyToClipboard(this.dataset.copyValue, this, 'Copied')" {
            (label)
        }
    }
}

fn message_meta(msg: &Message, jump_link: Option<&str>, take_action: Option<&str>) -> Markup {
    html! {
        div class="mt-1 flex flex-wrap items-center gap-x-2 gap-y-0.5 text-neutral-400 text-xs" {
            span class="select-all" { (msg.id) }
            @if let Some(href) = take_action {
                a href=(href) class="text-blue-600 hover:underline" { "Take action" }
            }
            @if let Some(link) = jump_link {
                (copy_button(link, "Copy link"))
            }
            (copy_button(&msg.id, "Copy message ID"))
            @if !msg.channel_id.is_empty() {
                (copy_button(&msg.channel_id, "Copy channel ID"))
            }
        }
    }
}

fn message_jump_link(web_app_endpoint: &str, msg: &Message) -> Option<String> {
    if msg.channel_id.is_empty() {
        return None;
    }
    Some(format!(
        "{}/channels/{}/{}/{}",
        web_app_endpoint.trim_end_matches('/'),
        msg.guild_id.as_deref().unwrap_or("@me"),
        msg.channel_id,
        msg.id
    ))
}

fn message_row(
    base_path: &str,
    avatar_url: &str,
    msg: &Message,
    include_delete: bool,
    is_highlighted: bool,
    is_grouped: bool,
    meta: &Markup,
) -> Markup {
    let hover = if is_highlighted {
        " hover:bg-amber-100"
    } else {
        " hover:bg-neutral-800/[.04]"
    };
    let highlight = if is_highlighted {
        " rounded-lg bg-amber-100/90 ring-1 ring-inset ring-amber-300/90 shadow-sm"
    } else {
        ""
    };
    let has_images = msg.attachments.iter().any(is_image);
    let has_content_or_images = !msg.content.is_empty() || has_images;

    if is_grouped {
        let row_class =
            format!("group relative py-0.5 pr-4 pl-4 transition-colors{hover}{highlight}");
        return html! {
            div class=(row_class)
                style="display:grid;grid-template-columns:16px 40px 16px minmax(0,1fr);\
                       min-height:1.375rem;"
                data-message-id=(msg.id) data-message-row="" {
                div style="grid-column:1/span 3;" {}
                div class="min-w-0" style="grid-column:4;" {
                    @if !msg.content.is_empty() {
                        div class="whitespace-pre-wrap break-words text-neutral-800 \
                                   text-sm leading-snug" { (msg.content) }
                    }
                    @if !msg.attachments.is_empty() {
                        (render_image_attachments(msg, include_delete))
                        (render_other_attachments(msg, has_content_or_images))
                    }
                    (render_missing_attachments(msg))
                    (meta)
                    @if include_delete && !has_images {
                        div class="absolute top-0 right-2 hidden group-hover:block" {
                            button type="button"
                                class="delete-message-btn rounded bg-white px-2 py-0.5 \
                                       text-red-600 text-xs shadow-sm ring-1 ring-neutral-200 \
                                       transition-colors hover:bg-red-50 hover:text-red-700"
                                data-channel-id=(msg.channel_id)
                                data-message-id=(msg.id) { "Delete" }
                        }
                    }
                }
            }
        };
    }

    let tag = format_user_display(
        msg.author_global_name.as_deref(),
        Some(&msg.author_username),
        None,
        false,
    );
    let row_class = format!(
        "group relative mt-4 py-0.5 pr-4 pl-4 transition-colors first:mt-0{hover}{highlight}"
    );
    html! {
        div class=(row_class)
            style="display:grid;grid-template-columns:16px 40px 16px minmax(0,1fr);"
            data-message-id=(msg.id) data-message-row="" {
            div style="grid-row:1;grid-column:1;" {}
            @if let Some(webhook_id) = &msg.webhook_id {
                span title=(webhook_id) class="block flex-shrink-0"
                  style="grid-row:1;grid-column:2;align-self:start;" {
                    img src=(avatar_url) alt=(msg.author_username)
                        class="rounded-full" style="width:40px;height:40px;";
                }
            } @else {
                a href={(base_path) "/users/" (msg.author_id)}
                  title=(msg.author_id) class="block flex-shrink-0"
                  style="grid-row:1;grid-column:2;align-self:start;" {
                    img src=(avatar_url) alt=(msg.author_username)
                        class="rounded-full" style="width:40px;height:40px;";
                }
            }
            div style="grid-row:1;grid-column:3;" {}
            div class="min-w-0" style="grid-column:4;" {
                div class="flex items-baseline gap-2" {
                    @if let Some(webhook_id) = &msg.webhook_id {
                        span class="font-medium text-neutral-900 text-sm"
                          title=(webhook_id) data-message-webhook=(webhook_id) { (msg.author_username) }
                        span class="self-center" { (badge("Webhook", BadgeVariant::Info)) }
                    } @else {
                        a href={(base_path) "/users/" (msg.author_id)}
                          class="font-medium text-neutral-900 text-sm hover:underline"
                          title=(msg.author_id) { (tag) }
                        @if msg.author_bot == Some(true) {
                            span class="self-center" data-message-author-bot=(msg.author_id) {
                                (badge("Bot", BadgeVariant::Info))
                            }
                        }
                    }
                    span class="text-neutral-400 text-xs" {
                        " \u{2014} " (format_admin_timestamp(&msg.timestamp))
                    }
                    (channel_nsfw_state_badge(
                        msg.channel_nsfw.unwrap_or(false),
                        None, None, msg.guild_nsfw,
                        msg.channel_content_warning_level,
                        msg.channel_content_warning_text.as_deref(),
                        true,
                    ))
                }
                @if !msg.content.is_empty() {
                    div class="mt-0.5 whitespace-pre-wrap break-words text-neutral-800 \
                               text-sm leading-snug" { (msg.content) }
                }
                @if !msg.attachments.is_empty() {
                    (render_image_attachments(msg, include_delete))
                    (render_other_attachments(msg, has_content_or_images))
                }
                (render_missing_attachments(msg))
                (meta)
            }
            @if include_delete && !has_images {
                div class="absolute top-1 right-2 hidden group-hover:block" {
                    button type="button"
                        class="delete-message-btn rounded bg-white px-2 py-0.5 \
                               text-red-600 text-xs shadow-sm ring-1 ring-neutral-200 \
                               transition-colors hover:bg-red-50 hover:text-red-700"
                        data-channel-id=(msg.channel_id)
                        data-message-id=(msg.id) { "Delete" }
                }
            }
        }
    }
}

pub fn message_list(
    config: &AdminConfig,
    base_path: &str,
    messages: &[Message],
    include_delete: bool,
    highlight_message_id: Option<&str>,
    take_action: Option<&TakeAction<'_>>,
) -> Markup {
    html! {
        div class="divide-y-0" {
            @for (i, msg) in messages.iter().enumerate() {
                @let is_grouped = i > 0
                    && messages[i - 1].author_id == msg.author_id
                    && messages[i - 1].author_username == msg.author_username;
                @let is_highlighted = highlight_message_id == Some(msg.id.as_str());
                @let avatar_url = user_avatar_url(
                    config, &msg.author_id, msg.author_avatar.as_deref(), 160, true,
                );
                @let take_action_href = take_action
                    .filter(|action| {
                        msg.webhook_id.is_none()
                            && action.author_id.is_none_or(|author_id| author_id == msg.author_id)
                    })
                    .map(|action| take_action_href(
                        base_path,
                        &msg.author_id,
                        action.report_id,
                        Some((&msg.channel_id, &msg.id)),
                    ));
                @let jump_link = message_jump_link(&config.web_app_endpoint, msg);
                @let meta = message_meta(msg, jump_link.as_deref(), take_action_href.as_deref());
                (message_row(
                    base_path, &avatar_url, msg,
                    include_delete, is_highlighted, is_grouped,
                    &meta,
                ))
            }
        }
    }
}

pub fn message_delete_prefill_script() -> Markup {
    html! {
        script defer { (PreEscaped(MESSAGE_DELETE_PREFILL_SCRIPT)) }
    }
}

const MESSAGE_DELETE_PREFILL_SCRIPT: &str = r#"(function() {
    if (window.__fluxerMessageDeletePrefill) return;
    window.__fluxerMessageDeletePrefill = true;
    document.addEventListener('click', function(e) {
        var t = e.target;
        if (!(t instanceof HTMLElement)) return;
        var button = t.closest('.delete-message-btn');
        var form = document.getElementById('delete-message-form');
        if (!(button instanceof HTMLButtonElement) || !form) return;
        e.preventDefault();
        form.querySelector('[name="channel_id"]').value = button.dataset.channelId || '';
        form.querySelector('[name="message_id"]').value = button.dataset.messageId || '';
        form.scrollIntoView({behavior: 'smooth', block: 'center'});
        form.querySelector('[name="basis"]').focus({preventScroll: true});
    });
})();"#;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::templates::components::message_data::ordered_messages;
    use serde_json::json;

    fn message(guild_id: serde_json::Value, channel_id: serde_json::Value) -> Message {
        ordered_messages(&[json!({
            "id": "1560373081531219968",
            "channel_id": channel_id,
            "guild_id": guild_id,
            "author_id": "1",
            "author_username": "someone",
        })])
        .remove(0)
    }

    #[test]
    fn jump_link_uses_guild_id_and_web_app_endpoint() {
        let msg = message(json!("42"), json!("7"));
        assert_eq!(
            message_jump_link("https://web.fluxer.example/", &msg).as_deref(),
            Some("https://web.fluxer.example/channels/42/7/1560373081531219968")
        );
    }

    #[test]
    fn jump_link_falls_back_to_me_for_dms() {
        let msg = message(json!(null), json!("7"));
        assert_eq!(
            message_jump_link("https://web.fluxer.example", &msg).as_deref(),
            Some("https://web.fluxer.example/channels/@me/7/1560373081531219968")
        );
    }

    #[test]
    fn meta_without_channel_skips_link_and_channel_copy() {
        let msg = message(json!(null), json!(null));
        assert_eq!(message_jump_link("https://web.fluxer.example", &msg), None);
        let html = message_meta(&msg, None, None).into_string();
        assert!(html.contains(r#"data-copy-value="1560373081531219968""#));
        assert!(html.contains("Copy message ID"));
        assert!(!html.contains("Copy link"));
        assert!(!html.contains("Copy channel ID"));
    }
}
