// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::utils::timestamps::format_admin_timestamp;
use crate::{
    api::types::{ListUserRelationshipsResponse, RelationshipEntry},
    config::AdminConfig,
    templates::components::{
        form::{csrf_input, submit_button},
        media::{initials, user_avatar_url},
        page_container::card_with_header,
    },
};
use maud::{Markup, html};

struct CategoryConfig {
    id: &'static str,
    title: &'static str,
    description: &'static str,
}

const CATEGORIES: &[CategoryConfig] = &[
    CategoryConfig {
        id: "friend",
        title: "Friends",
        description: "Mutual friend relationships. Removing one drops it for both users.",
    },
    CategoryConfig {
        id: "incoming_request",
        title: "Incoming Friend Requests",
        description: "Pending requests other users have sent to this user.",
    },
    CategoryConfig {
        id: "outgoing_request",
        title: "Outgoing Friend Requests",
        description: "Pending requests this user has sent.",
    },
    CategoryConfig {
        id: "blocked",
        title: "Blocked Users",
        description: "Users this user has blocked. Removing one unblocks that user.",
    },
];

pub fn relationships_tab(
    config: &AdminConfig,
    user_id: &str,
    data: &ListUserRelationshipsResponse,
    can_mutate: bool,
    csrf_token: &str,
) -> Markup {
    let total = data.friends.len()
        + data.incoming_requests.len()
        + data.outgoing_requests.len()
        + data.blocked.len();

    html! {
        div class="space-y-6" {
            (card_with_header("Relationships", html! {
                div class="space-y-2" {
                    p class="text-sm text-neutral-500" {
                        (total) " total \u{2014} "
                        (data.friends.len()) " friend(s), "
                        (data.incoming_requests.len()) " incoming, "
                        (data.outgoing_requests.len()) " outgoing, "
                        (data.blocked.len()) " blocked."
                    }
                    @if !can_mutate {
                        p class="text-sm italic text-neutral-500" {
                            "Read-only \u{2014} USER_REMOVE_RELATIONSHIP permission required."
                        }
                    }
                }
            }))

            @for cat in CATEGORIES {
                @let entries = match cat.id {
                    "friend" => &data.friends,
                    "incoming_request" => &data.incoming_requests,
                    "outgoing_request" => &data.outgoing_requests,
                    "blocked" => &data.blocked,
                    _ => &data.friends,
                };
                (category_section(config, user_id, cat, entries, can_mutate, csrf_token))
            }
        }
    }
}

fn category_section(
    config: &AdminConfig,
    _user_id: &str,
    cat: &CategoryConfig,
    entries: &[RelationshipEntry],
    can_mutate: bool,
    csrf_token: &str,
) -> Markup {
    let base = &config.base_path;
    html! {
        (card_with_header(
            &format!("{} ({})", cat.title, entries.len()),
            html! {
                div class="space-y-4" {
                    div class="flex flex-wrap items-start justify-between gap-3" {
                        p class="text-sm text-neutral-500" { (cat.description) }
                        @if can_mutate && !entries.is_empty() {
                            form method="post"
                                action="?action=remove_relationships_by_category&tab=relationships" {
                                (csrf_input(csrf_token))
                                input type="hidden" name="category" value=(cat.id);
                                button type="submit"
                                    class="inline-flex items-center rounded-md bg-red-600 px-3 \
                                           py-1.5 text-sm font-medium text-white hover:bg-red-700" {
                                    "Remove all " (entries.len())
                                }
                            }
                        }
                    }
                    @if entries.is_empty() {
                        p class="text-sm text-neutral-500" { "None" }
                    } @else {
                        div class="space-y-2" {
                            @for entry in entries {
                                (relationship_row(config, base, cat.id, entry, can_mutate, csrf_token))
                            }
                        }
                    }
                }
            },
        ))
    }
}

fn relationship_row(
    config: &AdminConfig,
    base: &str,
    category: &str,
    entry: &RelationshipEntry,
    can_mutate: bool,
    csrf_token: &str,
) -> Markup {
    let display = entry
        .target
        .as_ref()
        .map(super::resolved_user_display)
        .unwrap_or_else(|| entry.target_user_id.clone());

    html! {
        div class="flex flex-wrap items-center justify-between gap-3 rounded-lg border \
                    border-neutral-200 bg-neutral-50 px-3 py-2" {
            div class="flex min-w-0 flex-1 items-center gap-3" {
                @if let Some(target) = entry.target.as_ref() {
                    @let avatar_url = user_avatar_url(
                        config, &target.id, target.avatar.as_deref(), 160, false,
                    );
                    img src=(avatar_url) alt=(target.username)
                        class="h-10 w-10 flex-shrink-0 rounded-full bg-neutral-200";
                } @else {
                    div class="flex h-10 w-10 flex-shrink-0 items-center justify-center rounded-full bg-neutral-200 text-neutral-600 text-sm" {
                        (initials(&display))
                    }
                }
                div class="min-w-0 flex-1" {
                    p class="truncate text-sm text-neutral-900" {
                        a href={(base) "/users/" (entry.target_user_id)}
                            class="font-medium transition-colors hover:text-blue-600 \
                                   hover:underline" {
                            (display)
                        }
                    }
                    p class="truncate text-xs text-neutral-500" {
                        (entry.target_user_id)
                    }
                    @if let Some(ref nick) = entry.nickname {
                        p class="truncate text-xs text-neutral-500" {
                            "Nickname: " (nick)
                        }
                    }
                    @if let Some(ref since) = entry.since {
                        p class="text-xs text-neutral-500" {
                            "Since: " (format_admin_timestamp(since))
                        }
                    }
                }
            }
            @if can_mutate {
                form method="post"
                    action="?action=remove_relationship&tab=relationships" {
                    (csrf_input(csrf_token))
                    input type="hidden" name="target_user_id" value=(entry.target_user_id);
                    input type="hidden" name="category" value=(category);
                    (submit_button("Remove"))
                }
            }
        }
    }
}
