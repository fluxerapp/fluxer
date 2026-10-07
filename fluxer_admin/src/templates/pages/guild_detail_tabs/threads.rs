// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{GuildInfo, GuildThreadItem},
    config::AdminConfig,
    templates::components::{
        badge::{BadgeVariant, badge},
        form::{csrf_input, danger_button, form_actions, submit_button},
        page_container::card_with_header,
        table::{data_table, table_cell, table_row},
    },
};
use maud::{Markup, html};

pub fn threads_tab(
    config: &AdminConfig,
    guild: &GuildInfo,
    threads: &[GuildThreadItem],
    can_delete: bool,
    can_reindex: bool,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    html! {
        @if can_reindex {
            (card_with_header("Thread search index", html! {
                form method="post"
                    action={(base) "/guilds/" (guild.id) "?tab=threads&action=refresh_search_index"}
                    class="w-full" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="index_type" value="threads";
                    (form_actions(html! {
                        (submit_button("Refresh threads"))
                    }))
                }
            }))
        }
        (card_with_header(
            &format!("Threads ({})", threads.len()),
            html! {
                @if threads.is_empty() {
                    p class="text-sm text-neutral-500" { "No threads found for this guild." }
                } @else {
                    (data_table(
                        &["Thread", "Parent", "State", "Members", "Messages", ""],
                        html! {
                            @for thread in threads {
                                (thread_row(base, &guild.id, thread, can_delete, csrf_token))
                            }
                        },
                    ))
                }
            },
        ))
    }
}

fn thread_kind(channel_type: i32) -> &'static str {
    match channel_type {
        10 => "Announcement",
        12 => "Private",
        _ => "Public",
    }
}

fn thread_state(thread: &GuildThreadItem) -> Markup {
    let metadata = thread.thread_metadata.as_ref();
    let archived = metadata.is_some_and(|metadata| metadata.archived);
    let locked = metadata.is_some_and(|metadata| metadata.locked);
    html! {
        div class="flex flex-wrap gap-1" {
            (badge(thread_kind(thread.channel_type), BadgeVariant::Default))
            @if archived { (badge("Archived", BadgeVariant::Default)) }
            @if locked { (badge("Locked", BadgeVariant::Default)) }
        }
    }
}

fn thread_row(
    base: &str,
    guild_id: &str,
    thread: &GuildThreadItem,
    can_delete: bool,
    csrf_token: &str,
) -> Markup {
    table_row(html! {
        (table_cell(false, html! {
            div class="font-medium" { (thread.name.as_deref().unwrap_or("")) }
            div class="text-xs text-neutral-500" { "ID: " (thread.id) }
        }))
        (table_cell(true, html! { (thread.parent_id.as_deref().unwrap_or("")) }))
        (table_cell(false, thread_state(thread)))
        (table_cell(true, html! { (thread.member_count.unwrap_or(0)) }))
        (table_cell(true, html! { (thread.message_count.unwrap_or(0)) }))
        (table_cell(false, html! {
            @if can_delete {
                form method="post"
                    action={(base) "/guilds/" (guild_id) "?tab=threads&action=delete_thread"} {
                    (csrf_input(csrf_token))
                    input type="hidden" name="thread_id" value=(thread.id);
                    (danger_button("Delete thread"))
                }
            }
        }))
    })
}
