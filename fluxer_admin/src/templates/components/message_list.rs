// SPDX-License-Identifier: AGPL-3.0-or-later

use maud::{Markup, PreEscaped, html};

use super::icons::paperclip_icon;
use super::media::user_avatar_url;
use super::nsfw_indicators::{attachment_nsfw_badge, channel_nsfw_state_badge};
use super::user_display::format_user_display;
use crate::config::AdminConfig;

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

fn is_image(att: &Attachment) -> bool {
    att.content_type
        .as_deref()
        .is_some_and(|ct| ct.starts_with("image/"))
}

fn ncmec_badge(att: &Attachment) -> Markup {
    match att.ncmec_status.as_str() {
        "submitted" => {
            let label = att
                .ncmec_report_id
                .as_deref()
                .map(|id| format!("NCMEC {id}"))
                .unwrap_or_else(|| "Reported to NCMEC".into());
            html! {
                span class="rounded bg-green-100 px-2 py-0.5 text-[11px] text-green-800"
                     title=(label) { (label) }
            }
        }
        "failed" => {
            let title = att
                .ncmec_failure_reason
                .as_deref()
                .unwrap_or("NCMEC report failed");
            html! {
                span class="rounded bg-red-100 px-2 py-0.5 text-[11px] text-red-800"
                     title=(title) { "NCMEC Failed" }
            }
        }
        _ => html! {},
    }
}

fn render_image_attachments(msg: &Message, include_delete: bool) -> Markup {
    let images: Vec<&Attachment> = msg.attachments.iter().filter(|a| is_image(a)).collect();
    if images.is_empty() {
        return html! {};
    }
    let spacer = if !msg.content.is_empty() {
        "mt-2 space-y-3"
    } else {
        "mt-1 space-y-3"
    };
    html! {
        div class=(spacer) {
            @for att in &images {
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
                            (ncmec_badge(att))
                        }
                        @if include_delete {
                            div class="flex flex-wrap gap-2" {
                                button type="button"
                                    class="delete-message-btn rounded bg-white px-2.5 py-1 \
                                           text-red-600 text-xs shadow-sm ring-1 ring-neutral-200 \
                                           transition-colors hover:bg-red-50 hover:text-red-700"
                                    data-channel-id=(msg.channel_id)
                                    data-message-id=(msg.id) { "Delete" }
                                button type="button"
                                    class="ncmec-report-btn rounded bg-white px-2.5 py-1 text-xs \
                                           shadow-sm ring-1 ring-neutral-200 transition-colors \
                                           hover:bg-neutral-100 disabled:cursor-not-allowed \
                                           disabled:opacity-70"
                                    data-channel-id=(msg.channel_id)
                                    data-message-id=(msg.id)
                                    data-attachment-id=(att.id)
                                    data-filename=(att.filename)
                                    data-content-type=(att.content_type.as_deref().unwrap_or(""))
                                    data-size=(att.size.map(|s| s.to_string()).unwrap_or_default())
                                    data-author-id=(msg.author_id)
                                    data-ncmec-status=(att.ncmec_status)
                                    data-ncmec-report-id=(att.ncmec_report_id.as_deref().unwrap_or(""))
                                    disabled[att.ncmec_status == "submitted"]
                                    title=(if att.ncmec_status == "submitted" {
                                        "Already reported to NCMEC"
                                    } else {
                                        "Report this image to NCMEC"
                                    }) {
                                    @if att.ncmec_status == "submitted" {
                                        "Reported to NCMEC"
                                    } @else {
                                        "Report to NCMEC"
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn render_other_attachments(msg: &Message, has_content_or_images: bool) -> Markup {
    let others: Vec<&Attachment> = msg.attachments.iter().filter(|a| !is_image(a)).collect();
    if others.is_empty() {
        return html! {};
    }
    let spacer = if has_content_or_images {
        "mt-1.5 space-y-1"
    } else {
        "space-y-1"
    };
    html! {
        div class=(spacer) {
            @for att in &others {
                div class="flex flex-wrap items-center gap-2 text-xs" {
                    (paperclip_icon("text-neutral-400"))
                    a href=(att.url) target="_blank" rel="noopener noreferrer"
                      class="text-blue-600 hover:underline" { (att.filename) }
                    (attachment_nsfw_badge(att.nsfw.unwrap_or(false)))
                    (ncmec_badge(att))
                }
            }
        }
    }
}

fn message_row(
    base_path: &str,
    avatar_url: &str,
    msg: &Message,
    include_delete: bool,
    is_highlighted: bool,
    is_grouped: bool,
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
    );
    let row_class = format!(
        "group relative mt-4 py-0.5 pr-4 pl-4 transition-colors first:mt-0{hover}{highlight}"
    );
    html! {
        div class=(row_class)
            style="display:grid;grid-template-columns:16px 40px 16px minmax(0,1fr);"
            data-message-id=(msg.id) data-message-row="" {
            div style="grid-row:1;grid-column:1;" {}
            a href={(base_path) "/users/" (msg.author_id)}
              title=(msg.author_id) class="block flex-shrink-0"
              style="grid-row:1;grid-column:2;align-self:start;" {
                img src=(avatar_url) alt=(msg.author_username)
                    class="rounded-full" style="width:40px;height:40px;";
            }
            div style="grid-row:1;grid-column:3;" {}
            div class="min-w-0" style="grid-column:4;" {
                div class="flex items-baseline gap-2" {
                    a href={(base_path) "/users/" (msg.author_id)}
                      class="font-medium text-neutral-900 text-sm hover:underline"
                      title=(msg.author_id) { (tag) }
                    span class="text-neutral-400 text-xs" {
                        " \u{2014} " (msg.timestamp)
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
                div class="mt-1 text-neutral-400 text-xs" {
                    span class="" { (msg.id) }
                }
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
) -> Markup {
    html! {
        div class="divide-y-0" {
            @for (i, msg) in messages.iter().enumerate() {
                @let is_grouped = i > 0 && messages[i - 1].author_id == msg.author_id;
                @let is_highlighted = highlight_message_id == Some(msg.id.as_str());
                @let avatar_url = user_avatar_url(
                    config, &msg.author_id, msg.author_avatar.as_deref(), 160, true,
                );
                (message_row(
                    base_path, &avatar_url, msg,
                    include_delete, is_highlighted, is_grouped,
                ))
            }
        }
    }
}

pub fn message_deletion_script(csrf_token: &str) -> Markup {
    let csrf = serde_json::to_string(csrf_token).unwrap_or_else(|_| "\"\"".into());
    let script = r#"(function() {
    var csrf = __CSRF__;
    function bp() {
        return document.documentElement.dataset.basePath || '';
    }
    function toast(level, message) {
        document.body.dispatchEvent(new CustomEvent('showFlash', {detail: {level: level, message: message}}));
    }
    function post(action, fields) {
        fields.append('_csrf', csrf);
        return fetch(bp() + '/messages?action=' + action, {
            method: 'POST',
            body: fields,
            credentials: 'same-origin',
            headers: {'x-csrf-token': csrf}
        });
    }
    function deleteMessage(b) {
        var fields = new URLSearchParams();
        fields.append('channel_id', b.dataset.channelId || '');
        fields.append('message_id', b.dataset.messageId || '');
        b.disabled = true;
        b.textContent = 'Deleting...';
        toast('info', 'Deleting message...');
        post('delete', fields).then(function(r) {
            if (!r.ok) throw new Error('Failed');
            var row = b.closest('[data-message-id]');
            if (row) {
                row.style.opacity = '0.5';
                row.style.pointerEvents = 'none';
            }
            b.textContent = 'Deleted';
            toast('success', 'Message deleted.');
        }).catch(function() {
            b.disabled = false;
            b.textContent = 'Delete';
            toast('error', 'Failed to delete message.');
        });
    }
    function reportNcmec(b) {
        var name = prompt('Type your full name to confirm you personally viewed this image and want to submit it to NCMEC.');
        if (!name || !name.trim()) return;
        var fields = new URLSearchParams();
        fields.append('channel_id', b.dataset.channelId || '');
        fields.append('message_id', b.dataset.messageId || '');
        fields.append('attachment_id', b.dataset.attachmentId || '');
        fields.append('filename', b.dataset.filename || '');
        fields.append('reporter_full_name', name.trim());
        fields.append('confirmed_viewed', 'true');
        b.disabled = true;
        b.textContent = 'Reporting...';
        toast('info', 'Submitting NCMEC report...');
        post('report-to-ncmec', fields).then(function(r) {
            return r.json().catch(function() {
                return null;
            }).then(function(data) {
                if (!r.ok || !data || data.success !== true) throw new Error(data && (data.error || data.message) || 'Failed to report attachment to NCMEC');
                return data;
            });
        }).then(function(data) {
            b.textContent = 'Reported to NCMEC';
            b.dataset.ncmecStatus = 'submitted';
            if (data.ncmec_report_id) b.dataset.ncmecReportId = data.ncmec_report_id;
            toast('success', 'NCMEC report submitted.');
        }).catch(function(err) {
            b.disabled = false;
            b.textContent = 'Report to NCMEC';
            toast('error', err && err.message ? err.message : 'Failed to report attachment to NCMEC.');
        });
    }
    document.addEventListener('click', function(e) {
        var t = e.target;
        if (!(t instanceof HTMLElement)) return;
        var d = t.closest('.delete-message-btn');
        if (d instanceof HTMLButtonElement) {
            e.preventDefault();
            deleteMessage(d);
            return;
        }
        var n = t.closest('.ncmec-report-btn');
        if (n instanceof HTMLButtonElement && !n.disabled) {
            e.preventDefault();
            reportNcmec(n);
        }
    });
})();"#
    .replace("__CSRF__", &csrf);
    html! {
        script defer { (PreEscaped(script)) }
    }
}
