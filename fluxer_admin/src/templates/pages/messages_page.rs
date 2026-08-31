// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            error_display::error_alert,
            form::{
                FORM_INPUT_CLASS, csrf_input, danger_button, form_actions, form_field_group,
                submit_button,
            },
            message_list::{
                Attachment, Message, Poll, PollAnswer, PollAnswerCount, PollEmoji, PollMedia,
                PollResults, message_deletion_script, message_list,
            },
            page_container::{card, page_header},
        },
        layout::{LayoutOptions, admin_layout_ext},
    },
};
use maud::{Markup, html};
use serde_json::Value;
use std::cmp::Ordering;

const MESSAGE_BROWSE_SCRIPT: &str = r#"
(function () {
  var TOP_LOAD_THRESHOLD_PX = 160;
  var BOTTOM_LOAD_THRESHOLD_PX = 160;
  var RETRY_BACKOFF_MS = 2500;
  function getBasePath(root) {
    return document.documentElement.dataset.basePath || root.dataset.basePath || '';
  }
  function getStatusBarHeight(scrollContainer) {
    var bar = scrollContainer.querySelector('[data-message-scroll-status-bar]');
    return bar instanceof HTMLElement ? bar.offsetHeight : 0;
  }
  function setStatus(statusEl, text, isError) {
    if (!(statusEl instanceof HTMLElement)) return;
    statusEl.textContent = text;
    statusEl.classList.toggle('text-red-600', Boolean(isError));
    statusEl.classList.toggle('text-neutral-500', !isError);
  }
  function getRows(scrollContainer) {
    return scrollContainer.querySelectorAll('[data-message-row]');
  }
  function centerMessage(scrollContainer, messageId) {
    if (!messageId) return false;
    var target = null;
    var rows = getRows(scrollContainer);
    for (var i = 0; i < rows.length; i++) {
      if (rows[i] instanceof HTMLElement && rows[i].dataset.messageId === messageId) {
        target = rows[i];
        break;
      }
    }
    if (!target) return false;
    var scrollRect = scrollContainer.getBoundingClientRect();
    var targetRect = target.getBoundingClientRect();
    var barHeight = getStatusBarHeight(scrollContainer);
    var visibleHeight = Math.max(scrollContainer.clientHeight - barHeight, 1);
    var targetTop = scrollContainer.scrollTop + (targetRect.top - scrollRect.top) - barHeight;
    scrollContainer.scrollTop = Math.max(0, targetTop - Math.max((visibleHeight - targetRect.height) / 2, 0));
    return true;
  }
  function initScrollRoot(root) {
    if (!(root instanceof HTMLElement) || root.dataset.messageScrollInit === 'true') return;
    root.dataset.messageScrollInit = 'true';
    var scrollContainer = root.querySelector('[data-message-scroll]');
    if (!(scrollContainer instanceof HTMLElement)) return;
    var mode = root.dataset.mode || 'lookup';
    var isBrowse = mode === 'browse';
    var channelId = root.dataset.channelId || '';
    var focusMessageId = root.dataset.focusMessageId || '';
    var oldestMessageId = root.dataset.oldestMessageId || '';
    var newestMessageId = root.dataset.newestMessageId || '';
    var hasMore = root.dataset.hasMore === 'true';
    var hasNewer = root.dataset.hasNewer === 'true';
    var loading = false;
    var loadingNewer = false;
    var cooldownUntil = 0;
    var newerCooldownUntil = 0;
    var listContainer = root.querySelector('[data-browse-list]');
    var statusEl = root.querySelector('[data-browse-status]');
    var anchorEl = root.querySelector('[data-message-anchor-status]');
    if (anchorEl instanceof HTMLElement && focusMessageId) {
      anchorEl.textContent = 'Jump to anchor ' + focusMessageId;
      anchorEl.classList.remove('text-neutral-400');
      anchorEl.classList.add('cursor-pointer', 'text-blue-600', 'hover:underline');
      anchorEl.setAttribute('role', 'button');
      anchorEl.setAttribute('tabindex', '0');
      anchorEl.addEventListener('click', function () {
        centerMessage(scrollContainer, focusMessageId);
      });
    }
    function needsPrefill() {
      return scrollContainer.scrollHeight <= scrollContainer.clientHeight + 1;
    }
    function isNearBottom() {
      return scrollContainer.scrollHeight - scrollContainer.scrollTop - scrollContainer.clientHeight <= BOTTOM_LOAD_THRESHOLD_PX;
    }
    async function loadFragment(direction) {
      if (!isBrowse || !channelId || !(listContainer instanceof HTMLElement)) return;
      var older = direction === 'older';
      if (older) {
        if (loading || !hasMore || !oldestMessageId || Date.now() < cooldownUntil) return;
        loading = true;
      } else {
        if (loadingNewer || !hasNewer || !newestMessageId || Date.now() < newerCooldownUntil) return;
        loadingNewer = true;
      }
      setStatus(statusEl, older ? 'Loading older messages...' : 'Loading newer messages...', false);
      var prevScrollHeight = scrollContainer.scrollHeight;
      var prevScrollTop = scrollContainer.scrollTop;
      try {
        var url = new URL(getBasePath(root) + '/messages/browse-fragment', window.location.origin);
        url.searchParams.set('channel_id', channelId);
        url.searchParams.set(older ? 'before' : 'after', older ? oldestMessageId : newestMessageId);
        var res = await fetch(url.toString(), {credentials: 'same-origin', headers: {'X-Requested-With': 'fetch'}});
        if (!res.ok) throw new Error('HTTP ' + res.status);
        var doc = new DOMParser().parseFromString(await res.text(), 'text/html');
        var fragment = doc.querySelector('[data-browse-fragment]');
        if (!(fragment instanceof HTMLElement)) throw new Error('Invalid response');
        var count = Number(fragment.dataset.messageCount || '0');
        if (older) {
          var nextOldest = fragment.dataset.oldestMessageId || '';
          hasMore = fragment.dataset.hasMore === 'true';
          if (count > 0 && nextOldest && nextOldest !== oldestMessageId) {
            listContainer.insertBefore(fragment, listContainer.firstChild);
            oldestMessageId = nextOldest;
          } else {
            hasMore = false;
          }
          root.dataset.hasMore = hasMore ? 'true' : 'false';
          root.dataset.oldestMessageId = oldestMessageId;
        } else {
          var nextNewest = fragment.dataset.newestMessageId || '';
          hasNewer = fragment.dataset.hasMore === 'true';
          if (count > 0 && nextNewest && nextNewest !== newestMessageId) {
            listContainer.appendChild(fragment);
            newestMessageId = nextNewest;
          } else {
            hasNewer = false;
          }
          root.dataset.hasNewer = hasNewer ? 'true' : 'false';
          root.dataset.newestMessageId = newestMessageId;
        }
        requestAnimationFrame(function () {
          if (older) scrollContainer.scrollTop = prevScrollTop + (scrollContainer.scrollHeight - prevScrollHeight);
          setStatus(statusEl, '', false);
          loading = false;
          loadingNewer = false;
          if (older && hasMore && needsPrefill()) loadFragment('older');
          if (!older && hasNewer && needsPrefill()) loadFragment('newer');
        });
      } catch (err) {
        console.error('[messages] browse fragment failed:', err);
        setStatus(statusEl, older ? 'Could not load older messages. Retrying shortly...' : 'Could not load newer messages. Retrying shortly...', true);
        if (older) {
          cooldownUntil = Date.now() + RETRY_BACKOFF_MS;
          loading = false;
        } else {
          newerCooldownUntil = Date.now() + RETRY_BACKOFF_MS;
          loadingNewer = false;
        }
      }
    }
    scrollContainer.addEventListener('scroll', function () {
      if (isBrowse && hasMore && !loading && scrollContainer.scrollTop <= TOP_LOAD_THRESHOLD_PX) loadFragment('older');
      if (isBrowse && hasNewer && !loadingNewer && isNearBottom()) loadFragment('newer');
    }, {passive: true});
    requestAnimationFrame(function () {
      if (focusMessageId) {
        if (!centerMessage(scrollContainer, focusMessageId)) scrollContainer.scrollTop = 0;
      } else if (isBrowse) {
        scrollContainer.scrollTop = scrollContainer.scrollHeight;
      }
      if (isBrowse && hasMore && needsPrefill()) loadFragment('older');
      if (isBrowse && hasNewer && focusMessageId && needsPrefill()) loadFragment('newer');
    });
  }
  function initAll() {
    document.querySelectorAll('[data-message-scroll-root]').forEach(initScrollRoot);
  }
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', initAll);
  } else {
    initAll();
  }
})();
"#;

pub struct MessagesPageParams<'a> {
    pub csrf_token: &'a str,
    pub prefill_channel_id: Option<&'a str>,
    pub can_lookup: bool,
    pub can_delete: bool,
    pub lookup_result: Option<&'a Value>,
    pub browse_result: Option<&'a Value>,
    pub search_result: Option<&'a Value>,
    pub browse_channel_id: Option<&'a str>,
    pub search_query_text: Option<&'a str>,
    pub context_limit: u32,
    pub error: Option<&'a str>,
}

pub fn messages_page(
    config: &AdminConfig,
    auth: &AuthContext,
    params: &MessagesPageParams<'_>,
) -> Markup {
    let content = html! {
        (page_header("Message Tools", None))

        div class="space-y-6" {
            @if let Some(error) = params.error {
                (error_alert(error))
            }
            @if let (Some(result), Some(channel_id), Some(query)) = (
                params.search_result, params.browse_channel_id, params.search_query_text,
            ) {
                (search_result_card(config, result, channel_id, query, params.can_delete))
            }
            @if params.search_result.is_none()
                && let (Some(result), Some(channel_id)) = (
                    params.browse_result, params.browse_channel_id,
                ) {
                (browse_result_card(
                    config, result, channel_id, params.can_delete,
                    params.csrf_token, params.context_limit,
                ))
            }
            @if let Some(result) = params.lookup_result {
                (lookup_result_card(config, result, params.can_delete, params.context_limit))
            }
            @if params.can_lookup {
                (browse_channel_form(config, params.csrf_token, params.prefill_channel_id))
                (lookup_message_form(config, params.csrf_token, params.prefill_channel_id))
                (lookup_by_attachment_form(config, params.csrf_token))
            }
            @if params.can_delete {
                (delete_message_form(config, params.csrf_token))
            }
            (message_deletion_script(params.csrf_token))
        }
    };
    admin_layout_ext(
        config,
        auth,
        "Message Tools",
        "message-tools",
        None,
        content,
        LayoutOptions {
            extra_scripts: Some(MESSAGE_BROWSE_SCRIPT),
            csrf_token: params.csrf_token,
            ..LayoutOptions::default()
        },
    )
}

pub fn browse_messages_fragment(
    config: &AdminConfig,
    result: &Value,
    show_delete: bool,
    highlight_message_id: Option<&str>,
) -> Markup {
    let messages = ordered_messages(result);
    let has_more = result
        .get("has_more")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    let oldest_message_id = messages.first().map(|m| m.id.as_str()).unwrap_or("");
    let newest_message_id = messages.last().map(|m| m.id.as_str()).unwrap_or("");
    html! {
        div data-browse-fragment=""
            data-has-more=(if has_more { "true" } else { "false" })
            data-message-count=(messages.len())
            data-oldest-message-id=(oldest_message_id)
            data-newest-message-id=(newest_message_id) {
            (message_list(
                config,
                &config.base_path,
                &messages,
                show_delete,
                highlight_message_id,
            ))
        }
    }
}

fn browse_result_card(
    config: &AdminConfig,
    result: &Value,
    channel_id: &str,
    show_delete: bool,
    csrf_token: &str,
    context_limit: u32,
) -> Markup {
    let messages = ordered_messages(result);
    let has_more = result
        .get("has_more")
        .and_then(Value::as_bool)
        .unwrap_or(false);
    card(html! {
        (result_heading(config, result, channel_id, "Browse Channel: "))
        form method="post" action={(config.base_path) "/messages?action=search"} class="mb-4" {
            (csrf_input(csrf_token))
            input type="hidden" name="channel_id" value=(channel_id);
            div class="flex flex-col gap-2 sm:flex-row sm:items-end" {
                div class="flex-1" {
                    (form_field_group(
                        "Search messages", "browse-search-query", false, None, None,
                        html! {
                            input type="text" id="browse-search-query" name="search"
                                placeholder="Search message content..."
                                class=(FORM_INPUT_CLASS);
                        },
                    ))
                }
                button type="submit" class="inline-flex h-8 items-center justify-center rounded-md border border-neutral-300 bg-white px-3 font-medium text-neutral-700 text-sm hover:bg-neutral-50" {
                    "Search"
                }
            }
        }
        @if messages.is_empty() {
            (empty_state("No messages found in this channel."))
        } @else {
            (message_scroll_pane(MessageScrollPane {
                config,
                channel_id,
                messages: &messages,
                show_delete,
                has_more,
                has_newer: false,
                focus_message_id: None,
                context_limit,
            }))
        }
    })
}

fn search_result_card(
    config: &AdminConfig,
    result: &Value,
    channel_id: &str,
    query_text: &str,
    show_delete: bool,
) -> Markup {
    let messages = ordered_messages(result);
    let total = result
        .get("total")
        .and_then(Value::as_u64)
        .unwrap_or(messages.len() as u64);
    let title = format!("Search results for \"{query_text}\" in ");
    card(html! {
        (result_heading(config, result, channel_id, &title))
        p class="mb-4 text-neutral-500 text-sm" {
            (total) " result" @if total != 1 { "s" }
        }
        div class="mb-4" {
            a href={(config.base_path) "/messages?channel_id=" (channel_id)}
                class="inline-flex h-8 items-center rounded-md border border-neutral-300 bg-white px-3 font-medium text-neutral-700 text-sm hover:bg-neutral-50" {
                "Back to Browse"
            }
        }
        @if messages.is_empty() {
            (empty_state("No messages found."))
        } @else {
            (message_list(
                config,
                &config.base_path,
                &messages,
                show_delete,
                None,
            ))
        }
    })
}

fn lookup_result_card(
    config: &AdminConfig,
    result: &Value,
    show_delete: bool,
    context_limit: u32,
) -> Markup {
    let messages = ordered_messages(result);
    let channel_id = messages
        .first()
        .map(|m| m.channel_id.as_str())
        .unwrap_or("");
    let message_id = result.get("message_id").and_then(Value::as_str);
    card(html! {
        @if channel_id.is_empty() {
            h3 class="mb-4 text-base font-semibold text-neutral-900" { "Lookup Result" }
        } @else {
            (result_heading(config, result, channel_id, "Lookup Result: "))
        }
        @if messages.is_empty() {
            (empty_state("No messages found for this lookup."))
        } @else {
            (message_scroll_pane(MessageScrollPane {
                config,
                channel_id,
                messages: &messages,
                show_delete,
                has_more: true,
                has_newer: true,
                focus_message_id: message_id,
                context_limit,
            }))
        }
    })
}

struct MessageScrollPane<'a> {
    config: &'a AdminConfig,
    channel_id: &'a str,
    messages: &'a [Message],
    show_delete: bool,
    has_more: bool,
    has_newer: bool,
    focus_message_id: Option<&'a str>,
    context_limit: u32,
}

fn message_scroll_pane(pane: MessageScrollPane<'_>) -> Markup {
    let oldest_message_id = pane.messages.first().map(|m| m.id.as_str()).unwrap_or("");
    let newest_message_id = pane.messages.last().map(|m| m.id.as_str()).unwrap_or("");
    html! {
        div data-message-scroll-root=""
            data-mode="browse"
            data-channel-id=(pane.channel_id)
            data-focus-message-id=(pane.focus_message_id.unwrap_or(""))
            data-has-more=(if pane.has_more { "true" } else { "false" })
            data-has-newer=(if pane.has_newer { "true" } else { "false" })
            data-oldest-message-id=(oldest_message_id)
            data-newest-message-id=(newest_message_id)
            data-context-limit=(pane.context_limit) {
            div data-message-scroll=""
                class="max-h-[70vh] min-h-[26rem] overflow-y-auto overscroll-contain rounded-xl border border-neutral-200 bg-white shadow-inner focus:outline-none focus:ring-2 focus:ring-neutral-300" {
                div data-message-scroll-status-bar=""
                    class="sticky top-0 z-10 border-neutral-200 border-b bg-white/95 px-4 py-2 backdrop-blur" {
                    div class="flex flex-wrap items-center justify-between gap-2 text-xs" {
                        span data-browse-status="" class="text-neutral-500" {}
                        span data-message-anchor-status="" class="text-[11px] text-neutral-400" {}
                    }
                }
                div data-browse-list="" class="px-0 py-3" {
                    div data-browse-fragment=""
                        data-has-more=(if pane.has_more { "true" } else { "false" })
                        data-message-count=(pane.messages.len())
                        data-oldest-message-id=(oldest_message_id)
                        data-newest-message-id=(newest_message_id) {
                        (message_list(
                            pane.config,
                            &pane.config.base_path,
                            pane.messages,
                            pane.show_delete,
                            pane.focus_message_id,
                        ))
                    }
                }
            }
        }
    }
}

fn result_heading(config: &AdminConfig, result: &Value, channel_id: &str, prefix: &str) -> Markup {
    let first = result
        .get("messages")
        .and_then(Value::as_array)
        .and_then(|messages| messages.first());
    let channel_name = first
        .and_then(|m| m.get("channel_name"))
        .and_then(Value::as_str);
    let guild_id = first.and_then(|m| m.get("guild_id")).and_then(value_id);
    let guild_name = first
        .and_then(|m| m.get("guild_name"))
        .and_then(Value::as_str);
    html! {
        h3 class="mb-4 text-base font-semibold text-neutral-900" {
            (prefix)
            @if let Some(name) = channel_name {
                a href={(config.base_path) "/messages?channel_id=" (channel_id)}
                    class="text-blue-600 hover:underline" { "#" (name) }
            } @else {
                a href={(config.base_path) "/messages?channel_id=" (channel_id)}
                    class="text-blue-600 hover:underline" { (channel_id) }
            }
            @if let (Some(gid), Some(gname)) = (guild_id, guild_name) {
                " "
                span class="text-neutral-400" { "in" }
                " "
                a href={(config.base_path) "/guilds/" (gid)}
                    class="text-blue-600 hover:underline" { (gname) }
            }
            @if channel_name.is_some() {
                span class="ml-2 text-neutral-400 text-xs" { (channel_id) }
            }
        }
    }
}

fn empty_state(text: &str) -> Markup {
    html! {
        div class="rounded-lg border border-dashed border-neutral-300 bg-neutral-50 px-4 py-8 text-center text-neutral-500 text-sm" {
            (text)
        }
    }
}

fn ordered_messages(result: &Value) -> Vec<Message> {
    let mut messages: Vec<Message> = result
        .get("messages")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .map(message_from_value)
        .collect();
    messages.sort_by(compare_message_ids);
    messages
}

fn message_from_value(value: &Value) -> Message {
    let attachments = value
        .get("attachments")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .map(attachment_from_value)
        .collect();
    Message {
        id: value.get("id").and_then(value_id).unwrap_or_default(),
        content: value
            .get("content")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        timestamp: value
            .get("timestamp")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        author_id: value
            .get("author_id")
            .and_then(value_id)
            .unwrap_or_default(),
        author_username: value
            .get("author_username")
            .and_then(Value::as_str)
            .unwrap_or("Unknown")
            .to_owned(),
        author_global_name: value
            .get("author_global_name")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        author_discriminator: value
            .get("author_discriminator")
            .and_then(value_id)
            .unwrap_or_else(|| "0000".to_owned()),
        author_avatar: value
            .get("author_avatar")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        channel_id: value
            .get("channel_id")
            .and_then(value_id)
            .unwrap_or_default(),
        channel_nsfw: value.get("channel_nsfw").and_then(Value::as_bool),
        channel_content_warning_level: value
            .get("channel_content_warning_level")
            .and_then(Value::as_i64)
            .map(|n| n as i32),
        channel_content_warning_text: value
            .get("channel_content_warning_text")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        guild_nsfw: value.get("guild_nsfw").and_then(Value::as_bool),
        poll: value.get("poll").map(poll_from_value),
        attachments,
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
        id: value.get("id").and_then(value_id).unwrap_or_default(),
        url: value
            .get("url")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        filename: value
            .get("filename")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_owned(),
        nsfw: value.get("nsfw").and_then(Value::as_bool),
        content_type: value
            .get("content_type")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        width: value.get("width").and_then(Value::as_u64).map(|n| n as u32),
        height: value
            .get("height")
            .and_then(Value::as_u64)
            .map(|n| n as u32),
        size: value.get("size").and_then(Value::as_u64),
        ncmec_status: value
            .get("ncmec_status")
            .and_then(Value::as_str)
            .unwrap_or("not_submitted")
            .to_owned(),
        ncmec_report_id: value
            .get("ncmec_report_id")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
        ncmec_failure_reason: value
            .get("ncmec_failure_reason")
            .and_then(Value::as_str)
            .map(ToOwned::to_owned),
    }
}

fn value_id(value: &Value) -> Option<String> {
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

fn browse_channel_form(config: &AdminConfig, csrf_token: &str, prefill: Option<&str>) -> Markup {
    let base = &config.base_path;
    card(html! {
        h3 class="mb-4 text-base font-semibold text-neutral-900" {
            "Browse Channel"
        }
        form method="post" action={(base) "/messages?action=browse"} {
            (csrf_input(csrf_token))
            div class="flex flex-col gap-4" {
                (form_field_group(
                    "Channel ID", "browse-channel-id", true, None, None,
                    html! {
                        input type="text" id="browse-channel-id" name="channel_id"
                            placeholder="123456789" required
                            value=[prefill]
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_actions(html! {
                    (submit_button("Browse Channel"))
                }))
            }
        }
    })
}

fn lookup_message_form(config: &AdminConfig, csrf_token: &str, prefill: Option<&str>) -> Markup {
    let base = &config.base_path;
    card(html! {
        h3 class="mb-4 text-base font-semibold text-neutral-900" {
            "Lookup Message"
        }
        form method="post" action={(base) "/messages?action=lookup"} {
            (csrf_input(csrf_token))
            div class="flex flex-col gap-4" {
                (form_field_group(
                    "Channel ID", "lookup-message-channel-id", true, None, None,
                    html! {
                        input type="text" id="lookup-message-channel-id" name="channel_id"
                            placeholder="123456789" required
                            value=[prefill]
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Message ID", "lookup-message-message-id", true, None, None,
                    html! {
                        input type="text" id="lookup-message-message-id" name="message_id"
                            placeholder="123456789" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Context Limit (messages before and after)",
                    "lookup-message-context-limit", true, None, None,
                    html! {
                        input type="number" id="lookup-message-context-limit"
                            name="context_limit" value="50" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_actions(html! {
                    (submit_button("Lookup Message"))
                }))
            }
        }
    })
}

fn lookup_by_attachment_form(config: &AdminConfig, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    card(html! {
        h3 class="mb-4 text-base font-semibold text-neutral-900" {
            "Lookup Message by Attachment"
        }
        form method="post" action={(base) "/messages?action=lookup-by-attachment"} {
            (csrf_input(csrf_token))
            div class="flex flex-col gap-4" {
                (form_field_group(
                    "Channel ID", "lookup-by-attachment-channel-id", true, None, None,
                    html! {
                        input type="text" id="lookup-by-attachment-channel-id"
                            name="channel_id" placeholder="123456789" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Attachment ID", "lookup-by-attachment-attachment-id",
                    true, None, None,
                    html! {
                        input type="text" id="lookup-by-attachment-attachment-id"
                            name="attachment_id" placeholder="123456789" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Filename", "lookup-by-attachment-filename", true, None, None,
                    html! {
                        input type="text" id="lookup-by-attachment-filename"
                            name="filename" placeholder="image.png" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Context Limit (messages before and after)",
                    "lookup-by-attachment-context-limit", true, None, None,
                    html! {
                        input type="number" id="lookup-by-attachment-context-limit"
                            name="context_limit" value="50" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_actions(html! {
                    (submit_button("Lookup by Attachment"))
                }))
            }
        }
    })
}

fn delete_message_form(config: &AdminConfig, csrf_token: &str) -> Markup {
    let base = &config.base_path;
    card(html! {
        h3 class="mb-4 text-base font-semibold text-neutral-900" {
            "Delete Message"
        }
        form method="post" action={(base) "/messages?action=delete"} {
            (csrf_input(csrf_token))
            div class="flex flex-col gap-4" {
                (form_field_group(
                    "Channel ID", "delete-message-channel-id", true, None, None,
                    html! {
                        input type="text" id="delete-message-channel-id"
                            name="channel_id" placeholder="123456789" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Message ID", "delete-message-message-id", true, None, None,
                    html! {
                        input type="text" id="delete-message-message-id"
                            name="message_id" placeholder="123456789" required
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_field_group(
                    "Audit Log Reason (optional)",
                    "delete-message-audit-log-reason", false, None, None,
                    html! {
                        input type="text" id="delete-message-audit-log-reason"
                            name="audit_log_reason" placeholder="Reason for deletion"
                            class=(FORM_INPUT_CLASS);
                    },
                ))
                (form_actions(html! {
                    (danger_button("Delete Message"))
                }))
            }
        }
    })
}
