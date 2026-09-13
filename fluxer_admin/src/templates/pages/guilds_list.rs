// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::GuildInfo,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::search_input_ext,
            media::{guild_icon_url, initials},
            nsfw_indicators::{adult_content_badge, content_warning_badge},
            page_container::{page_header, page_header_with_actions},
            table::empty_state,
        },
        layout::admin_layout,
    },
    utils::forms::parse_comma_separated,
};
use maud::{Markup, html};

pub struct GuildsListParams {
    pub q: String,
    pub ids: String,
    pub requested_ids: Vec<String>,
    pub limit: u32,
    pub page: u32,
}

impl GuildsListParams {
    pub fn from_query(
        q: Option<String>,
        ids: Option<String>,
        limit: Option<u32>,
        page: Option<u32>,
    ) -> Self {
        let ids = ids.unwrap_or_default();
        Self {
            q: q.unwrap_or_default().trim().to_owned(),
            requested_ids: parse_comma_separated(&ids),
            ids: ids.trim().to_owned(),
            limit: match limit.unwrap_or(50) {
                25 => 25,
                100 => 100,
                200 => 200,
                _ => 50,
            },
            page: page.unwrap_or(0),
        }
    }

    pub fn has_id_lookup(&self) -> bool {
        !self.requested_ids.is_empty()
    }

    pub fn has_search(&self) -> bool {
        self.has_id_lookup() || !self.q.is_empty()
    }

    pub fn search_query(&self) -> &str {
        &self.q
    }
}

pub struct GuildsListResults<'a> {
    pub guilds: Option<&'a [GuildInfo]>,
    pub total: Option<u64>,
}

pub fn guilds_list_page(
    config: &AdminConfig,
    auth: &AuthContext,
    params: &GuildsListParams,
    results: Option<&[GuildInfo]>,
    total: Option<u64>,
    is_htmx: bool,
) -> Markup {
    let base = &config.base_path;
    let result_state = GuildsListResults {
        guilds: results,
        total,
    };
    let has_results = results.is_some_and(|guilds| !guilds.is_empty());
    let header = html! {
        @if has_results {
            @if params.has_id_lookup() {
                (page_header_with_actions("Guilds", None, html! {
                    p class="text-sm font-normal text-neutral-500" {
                        @let found = result_state.guilds.map(|guilds| guilds.len()).unwrap_or_default();
                        @let requested = params.requested_ids.len();
                        @let missing = requested.saturating_sub(found);
                        (found) " of " (requested) " requested guild" (if requested == 1 { "" } else { "s" }) " found"
                        @if missing > 0 {
                            " (" (missing) " missing)"
                        }
                    }
                }))
            } @else if let Some(count) = result_state.total {
                (page_header_with_actions("Guilds", None, html! {
                    p class="text-sm font-normal text-neutral-500" {
                        "Found " (count) " results (showing "
                        (result_state.guilds.map(|guilds| guilds.len()).unwrap_or_default())
                        ")"
                    }
                }))
            } @else {
                (page_header("Guilds", None))
            }
        } @else {
            (page_header("Guilds", None))
        }
    };
    let content = html! {
        div id="guilds-results" class="space-y-6" {
            (header)
            div class="rounded-lg bg-white transition-all border border-neutral-200 p-4" {
                (search_input_ext(
                    "q",
                    &params.q,
                    "Search by ID, guild name, or vanity URL...",
                    &format!("{base}/guilds"),
                    "guilds-results",
                    Some("outerHTML"),
                ))
            }
            (render_results(config, params, &result_state))
        }
    };

    if is_htmx {
        return content;
    }
    admin_layout(config, auth, "Guilds", "guilds", None, content)
}

fn render_results(
    config: &AdminConfig,
    params: &GuildsListParams,
    results: &GuildsListResults<'_>,
) -> Markup {
    html! {
        @if let Some(guilds) = results.guilds {
            @if guilds.is_empty() {
                (empty_state("No guilds found matching your search."))
            } @else {
                (render_guilds_cards(config, guilds))
                @if !params.has_id_lookup() {
                    (pagination(config, params, results.total.unwrap_or(guilds.len() as u64)))
                }
            }
        } @else if params.has_search() {
            (empty_state("No results found."))
        } @else {
            (empty_state(
                "Enter a search query to find guilds. Search by Guild ID, \
                 Guild Name, Vanity URL, or other attributes."
            ))
        }
    }
}

fn pagination(config: &AdminConfig, params: &GuildsListParams, total: u64) -> Markup {
    let total_pages = total.div_ceil(u64::from(params.limit)).max(1);
    let has_previous = params.page > 0;
    let next_page = params
        .page
        .checked_add(1)
        .filter(|page| u64::from(*page) < total_pages);
    html! {
        div class="mt-6 flex items-center justify-center gap-3" {
            @if has_previous {
                a href=(pagination_url(&config.base_path, params, params.page - 1))
                    class="rounded-lg border border-neutral-300 bg-white px-6 py-2 \
                           font-medium text-neutral-900 text-sm no-underline \
                           transition-colors hover:bg-neutral-50" {
                    "\u{2190} Previous"
                }
            } @else {
                div class="cursor-not-allowed rounded-lg border border-neutral-200 \
                           bg-neutral-100 px-6 py-2 font-medium text-neutral-400 text-sm" {
                    "\u{2190} Previous"
                }
            }
            span class="text-neutral-600 text-sm" {
                "Page " (u64::from(params.page) + 1) " of " (total_pages)
            }
            @if let Some(next_page) = next_page {
                a href=(pagination_url(&config.base_path, params, next_page))
                    class="rounded-lg bg-neutral-900 px-6 py-2 font-medium text-sm \
                           text-white no-underline transition-colors hover:bg-neutral-800" {
                    "Next \u{2192}"
                }
            } @else {
                div class="cursor-not-allowed rounded-lg bg-neutral-100 px-6 py-2 \
                           font-medium text-neutral-400 text-sm" {
                    "Next \u{2192}"
                }
            }
        }
    }
}

fn pagination_url(base: &str, params: &GuildsListParams, page: u32) -> String {
    let mut parts = vec![format!("page={page}"), format!("limit={}", params.limit)];
    if !params.q.is_empty() {
        parts.push(format!("q={}", urlencoding::encode(&params.q)));
    }
    format!("{base}/guilds?{}", parts.join("&"))
}

fn render_guilds_cards(config: &AdminConfig, guilds: &[GuildInfo]) -> Markup {
    let base = &config.base_path;
    html! {
        div class="grid grid-cols-1 md:grid-cols-1 gap-4" {
            @for guild in guilds {
                div class="overflow-hidden rounded-lg border border-neutral-200 bg-white \
                           transition-colors hover:border-neutral-300" {
                    div class="p-5" {
                        div class="flex flex-col gap-4 sm:flex-row sm:items-center" {
                            (guild_icon(config, guild))
                            div class="min-w-0 flex-1" {
                                div class="mb-2 flex flex-wrap items-center gap-2" {
                                    h2 class="text-gray-900 tracking-tight text-base" {
                                        (guild.name)
                                    }
                                    (adult_content_badge(guild.nsfw.unwrap_or(false), None))
                                    (content_warning_badge(
                                        guild.content_warning_level,
                                        guild.content_warning_text.as_deref(),
                                        false,
                                    ))
                                }
                                div class="space-y-0.5" {
                                    p class="text-sm font-normal text-neutral-500 break-all" {
                                        "ID: " (guild.id)
                                    }
                                    p class="text-sm font-normal text-neutral-500" {
                                        "Members: " (guild.member_count)
                                    }
                                    p class="text-sm font-normal text-neutral-500" {
                                        "Owner: "
                                        a href={(base) "/users/" (guild.owner_id)}
                                            class="transition-colors hover:text-blue-600 \
                                                   hover:underline" {
                                            (owner_display(guild))
                                        }
                                    }
                                }
                            }
                            a href={(base) "/guilds/" (guild.id)}
                                role="button"
                                class="inline-flex items-center justify-center gap-2 font-medium \
                                       rounded-lg transition-all duration-150 focus:outline-none \
                                       focus:ring-2 focus:ring-offset-2 bg-neutral-900 text-white \
                                       hover:bg-neutral-800 px-3 py-1.5 text-sm w-fit \
                                       focus:ring-offset-white" {
                                span { "View Details" }
                            }
                        }
                    }
                }
            }
        }
    }
}

fn owner_display(guild: &GuildInfo) -> String {
    let Some(username) = guild.owner_username.as_deref() else {
        return guild.owner_id.clone();
    };
    let Some(discriminator) = guild.owner_discriminator.as_deref() else {
        return guild.owner_id.clone();
    };
    let tag = format!("{username}#{discriminator}");
    if let Some(global_name) = guild
        .owner_global_name
        .as_deref()
        .filter(|name| !name.trim().is_empty())
    {
        format!("{global_name} ({tag})")
    } else {
        tag
    }
}

fn guild_icon(config: &AdminConfig, guild: &GuildInfo) -> Markup {
    match guild_icon_url(config, &guild.id, guild.icon.as_deref(), 160, true) {
        Some(url) => {
            html! {
                div class="flex flex-shrink-0 items-center justify-center sm:block" {
                    img src=(url) alt=(guild.name) class="h-16 w-16 rounded-full";
                }
            }
        }
        None => {
            html! {
                div class="flex flex-shrink-0 items-center justify-center sm:block" {
                    div class="flex h-16 w-16 items-center justify-center rounded-full \
                               bg-neutral-200 font-medium text-base text-neutral-600" {
                        (initials(&guild.name))
                    }
                }
            }
        }
    }
}
