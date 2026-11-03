// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::templates::components::pagination::pager_with_missing;

use crate::{
    api::types::AdminUser,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            drawer::{DrawerSide, DrawerWidth, drawer},
            form::{
                FORM_CONTROL_CLASS, FORM_SEARCH_INPUT_SIZE_CLASS, secondary_button_link,
                select_chevron, submit_button,
            },
            media::user_avatar_url,
            page_container::page_header,
            table::{
                empty_state, table_body, table_cell, table_container, table_head,
                table_header_cell, table_row,
            },
            user_profile_badges::{BadgeSize, user_profile_badges},
        },
        layout::admin_layout,
    },
    utils::{
        bigint::format_discriminator,
        user_tag::{unique_usernames, user_tag},
    },
};
use maud::{Markup, html};

pub struct UserListParams {
    pub q: String,
    pub email: String,
    pub ip: String,
    pub requested_ids: Vec<String>,
    pub limit: u32,
    pub page: u32,
}

impl UserListParams {
    pub fn from_query(
        q: Option<String>,
        email: Option<String>,
        ip: Option<String>,
        ids: Option<String>,
        limit: Option<u32>,
        page: Option<u32>,
    ) -> Self {
        let ids = ids.unwrap_or_default();
        Self {
            q: q.unwrap_or_default().trim().to_owned(),
            email: email.unwrap_or_default().trim().to_owned(),
            ip: ip.unwrap_or_default().trim().to_owned(),
            requested_ids: parse_ids_query(&ids),
            limit: match limit.unwrap_or(25) {
                50 => 50,
                100 => 100,
                200 => 200,
                _ => 25,
            },
            page: page.unwrap_or(0),
        }
    }

    pub fn has_id_lookup(&self) -> bool {
        !self.requested_ids.is_empty()
    }

    pub fn has_search(&self) -> bool {
        self.has_id_lookup() || !self.q.is_empty() || !self.email.is_empty() || !self.ip.is_empty()
    }

    pub fn search_query(&self) -> Option<&str> {
        if self.email.is_empty() && self.ip.is_empty() && !self.q.is_empty() {
            Some(&self.q)
        } else {
            None
        }
    }

    pub fn email_query(&self) -> Option<&str> {
        if self.email.is_empty() {
            None
        } else {
            Some(&self.email)
        }
    }

    pub fn ip_query(&self) -> Option<&str> {
        if self.ip.is_empty() {
            None
        } else {
            Some(&self.ip)
        }
    }
}

#[derive(Clone, Copy)]
pub enum UserEmailAccess {
    Hidden,
    SearchOnly,
    SearchAndView,
}

pub enum UserListOutput {
    Page,
    Results,
}

pub struct UserListResults<'a> {
    pub users: &'a [AdminUser],
    pub has_more: bool,
    pub total: Option<u64>,
}

pub struct UserListView<'a> {
    pub params: &'a UserListParams,
    pub results: Option<UserListResults<'a>>,
    pub email: UserEmailAccess,
    pub output: UserListOutput,
}

pub fn users_list_page(config: &AdminConfig, auth: &AuthContext, view: UserListView<'_>) -> Markup {
    let base = &config.base_path;
    let UserListView {
        params,
        results,
        email,
        output,
    } = view;
    let results_markup = render_results(config, params, results.as_ref(), email);

    if matches!(output, UserListOutput::Results) {
        return results_markup;
    }

    let content = html! {
        div class="space-y-6" {
            (page_header("Users", None))
            div class="rounded-lg bg-white transition-all border border-neutral-200 p-3" {
                p class="mb-1 text-xs text-neutral-500" {
                    "Every account is listed, oldest first. Type to narrow the list."
                }
                (search_form(base, params, email))
            }
            (results_markup)
        }
        (drawer(
            "user-peek", "User", None,
            DrawerSide::Right, DrawerWidth::Lg, None, None,
        ))
    };
    admin_layout(config, auth, "Users", "users", None, content)
}

fn parse_ids_query(ids_query: &str) -> Vec<String> {
    let mut ids = Vec::new();
    for raw in ids_query.split(',') {
        let id = raw.trim();
        if !id.is_empty()
            && id.chars().all(|ch| ch.is_ascii_digit())
            && !ids.iter().any(|v| v == id)
        {
            ids.push(id.to_owned());
        }
    }
    ids
}

fn search_input(name: &str, value: &str, placeholder: &str, action: &str) -> Markup {
    html! {
        div class="flex-1" {
            input id={"search-" (name)} type="text" name=(name) value=(value)
                placeholder=(placeholder)
                class={(FORM_CONTROL_CLASS) " " (FORM_SEARCH_INPUT_SIZE_CLASS)}
                hx-get=(action)
                hx-trigger="input changed delay:300ms, search"
                hx-target="#users-results"
                hx-push-url="true"
                hx-include="closest form"
                hx-swap="outerHTML";
        }
    }
}

fn search_form(base: &str, params: &UserListParams, email: UserEmailAccess) -> Markup {
    let action = format!("{base}/users");
    let placeholder = if unique_usernames() {
        "Search by user ID, username, or Stripe ID..."
    } else {
        "Search by user ID, username, tag#0000, or Stripe ID..."
    };
    html! {
        form method="get" action=(&action)
            class="flex flex-col gap-3 sm:flex-row sm:items-center" {
            div class="flex flex-1 flex-col gap-2 sm:flex-row" {
                (search_input("q", &params.q, placeholder, &action))
                @if !matches!(email, UserEmailAccess::Hidden) {
                    (search_input("email", &params.email, "Exact email address...", &action))
                }
                (search_input("ip", &params.ip, "Last active IP address...", &action))
                div class="flex-1" {
                    div class="relative" {
                        select id="search-limit" name="limit"
                            hx-get=(&action)
                            hx-trigger="change"
                            hx-target="#users-results"
                            hx-push-url="true"
                            hx-include="closest form"
                            hx-swap="outerHTML"
                            class={(FORM_CONTROL_CLASS) " h-10 appearance-none px-3 py-1.5 pr-10"} {
                            @for value in [25_u32, 50, 100, 200] {
                                option value=(value) selected[value == params.limit] { (value) }
                            }
                        }
                        (select_chevron())
                    }
                }
            }
            div class="flex flex-col gap-2 sm:shrink-0 sm:flex-row" {
                (submit_button("Search"))
                (secondary_button_link("Clear", &action))
            }
        }
    }
}

fn render_results(
    config: &AdminConfig,
    params: &UserListParams,
    results: Option<&UserListResults<'_>>,
    email: UserEmailAccess,
) -> Markup {
    let base = &config.base_path;
    html! {
        @if let Some(result) = results {
            @let users = result.users;
            @if users.is_empty() {
                div id="users-results" class="flex flex-col gap-4 items-stretch" {
                    (empty_state(&empty_state_title(params)))
                }
            } @else {
                div id="users-results" class="flex flex-col gap-4 items-stretch" {
                    @if params.has_id_lookup() && users.len() < params.requested_ids.len() {
                        p class="text-sm text-neutral-500" {
                            (users.len()) " of " (params.requested_ids.len()) " requested users found (" (params.requested_ids.len() - users.len()) " missing)."
                        }
                    }
                    div class="flex items-center justify-between gap-2" {
                        p class="text-sm text-neutral-500" {
                            @if let Some(total) = result.total {
                                @let filtered = params.has_search();
                                @let noun = if total == 1 { "user" } else { "users" };
                                @if filtered {
                                    (format!("{total} matching {noun}"))
                                } @else {
                                    (format!("{total} {noun} on this instance"))
                                }
                            }
                        }
                        button type="button" id="copy-ids-btn" onclick="copyAllIds()"
                            class="rounded border border-neutral-300 px-3 py-1.5 font-medium text-neutral-700 text-xs hover:bg-neutral-50" {
                            "Copy IDs"
                        }
                    }
                    (render_users_table(config, users, email))
                    script { (maud::PreEscaped(copy_ids_script())) }
                    @if !params.has_id_lookup() && (params.page > 0 || result.has_more) {
                        (pagination_controls(base, params, result))
                    }
                }
            }
        } @else if params.has_search() {
            div id="users-results" class="flex flex-col gap-4 items-stretch" {
                (empty_state("No results found."))
            }
        } @else {
            div id="users-results" class="flex flex-col gap-4 items-stretch" {
                (empty_state("The user list could not be loaded."))
            }
        }
    }
}

fn empty_state_title(params: &UserListParams) -> String {
    if !params.has_search() {
        "No users yet".to_owned()
    } else if params.has_id_lookup() {
        format!(
            "No users found for the {} requested IDs",
            params.requested_ids.len()
        )
    } else if !params.email.is_empty() {
        format!("No users found with email \"{}\"", params.email)
    } else if !params.ip.is_empty() {
        format!("No users found with last active IP \"{}\"", params.ip)
    } else {
        format!("No users found matching \"{}\"", params.q)
    }
}

fn copy_ids_script() -> &'static str {
    r#"
function copyAllIds() {
	var cells = document.querySelectorAll('[data-user-id]');
	var ids = [];
	for (var i = 0; i < cells.length; i++) {
		ids.push(cells[i].getAttribute('data-user-id'));
	}
	if (ids.length === 0) return;
	navigator.clipboard.writeText(ids.join('\n')).then(function () {
		var btn = document.getElementById('copy-ids-btn');
		if (!btn) return;
		var original = btn.textContent;
		btn.textContent = 'Copied!';
		setTimeout(function () { btn.textContent = original; }, 1500);
	});
}

"#
}

fn user_status_badge(user: &AdminUser) -> Markup {
    if user.bot {
        badge("Bot", BadgeVariant::Info)
    } else if user.system {
        badge("System", BadgeVariant::Warning)
    } else {
        badge("User", BadgeVariant::Default)
    }
}

fn render_users_table(config: &AdminConfig, users: &[AdminUser], email: UserEmailAccess) -> Markup {
    let base = &config.base_path;
    table_container(html! {
        table class="min-w-full divide-y divide-neutral-200" {
            (table_head(html! {
                tr {
                    (table_header_cell("User"))
                    (table_header_cell("ID"))
                    @if matches!(email, UserEmailAccess::SearchAndView) {
                        (table_header_cell("Email"))
                    }
                    (table_header_cell("Status"))
                    (table_header_cell(""))
                }
            }))
            (table_body(html! {
                @for user in users {
                    @let display_name = user.global_name.as_deref()
                        .filter(|n| !n.trim().is_empty())
                        .unwrap_or(&user.username);
                    @let avatar_url = user_avatar_url(config, &user.id, user.avatar.as_deref(), 160, true);
                    (table_row(html! {
                        (table_cell(false, html! {
                            a href={(base) "/users/" (user.id)}
                                class="flex items-center gap-3 hover:opacity-80" {
                                img src=(avatar_url) alt=""
                                    class="h-8 w-8 rounded-full";
                                div class="flex flex-col gap-0.5" {
                                    div class="flex items-center gap-2" {
                                        p class="text-sm font-normal text-gray-900 font-medium" {
                                            @if user.global_name.as_deref().map(|n| !n.trim().is_empty()).unwrap_or(false) {
                                                (display_name)
                                            } @else {
                                                (user_tag(&user.username, &format_discriminator(&user.discriminator), user.bot))
                                            }
                                        }
                                        (user_profile_badges(
                                            &config.static_cdn_endpoint,
                                            &user.profile_badges,
                                            BadgeSize::Dense,
                                        ))
                                    }
                                    @if user.global_name.as_deref().map(|n| !n.trim().is_empty()).unwrap_or(false) {
                                        p class="text-xs font-normal text-neutral-500" {
                                            (user_tag(&user.username, &format_discriminator(&user.discriminator), user.bot))
                                        }
                                    }
                                }
                            }
                        }))
                        (table_cell(true, html! {
                            span class="text-gray-900 text-xs"
                                data-user-id=(user.id) {
                                (user.id)
                            }
                        }))
                        @if matches!(email, UserEmailAccess::SearchAndView) {
                            (table_cell(true, html! {
                                (user.email.as_deref().unwrap_or("-"))
                            }))
                        }
                        (table_cell(false, html! {
                            (user_status_badge(user))
                        }))
                        (table_cell(false, html! {
                            button type="button"
                                data-drawer-open="user-peek"
                                data-drawer-href={(base) "/users/" (user.id) "/fragment"}
                                data-drawer-title=(display_name)
                                popovertarget="user-peek"
                                hx-get={(base) "/users/" (user.id) "/fragment"}
                                hx-target="#user-peek-body"
                                hx-swap="innerHTML"
                                aria-label={"Peek user " (display_name)}
                                class="inline-flex min-h-[36px] items-center justify-center \
                                       rounded-md border border-neutral-300 px-3 py-1.5 \
                                       font-medium text-neutral-700 text-sm transition-colors \
                                       hover:border-neutral-400 hover:bg-neutral-50 \
                                       focus:outline-none focus-visible:ring-2 \
                                       focus-visible:ring-brand-primary \
                                       focus-visible:ring-offset-2" {
                                "Peek"
                            }
                        }))
                    }))
                }
            }))
        }
    })
}

fn pagination_controls(
    base: &str,
    params: &UserListParams,
    result: &UserListResults<'_>,
) -> Markup {
    let next_page = params.page.checked_add(1).filter(|_| result.has_more);
    pager_with_missing(
        (params.page > 0).then(|| users_url(base, params, params.page - 1)),
        None,
        next_page.map(|page| users_url(base, params, page)),
        true,
    )
}

fn users_url(base: &str, params: &UserListParams, page: u32) -> String {
    let mut pairs = Vec::new();
    if !params.q.is_empty() {
        pairs.push(format!("q={}", urlencoding::encode(&params.q)));
    }
    if !params.email.is_empty() {
        pairs.push(format!("email={}", urlencoding::encode(&params.email)));
    }
    if !params.ip.is_empty() {
        pairs.push(format!("ip={}", urlencoding::encode(&params.ip)));
    }
    pairs.push(format!("limit={}", params.limit));
    pairs.push(format!("page={page}"));
    format!("{base}/users?{}", pairs.join("&"))
}
