// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::{BlocklistEntry, BlocklistEntryPage},
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            badge::{BadgeVariant, badge},
            form::{checkbox, csrf_input},
            page_container::page_header,
            table::{data_table, empty_state, table_cell, table_row},
        },
        layout::admin_layout,
        pages::blocklist_helpers::{
            BlocklistActionVariant, blocklist_action_card, blocklist_text_field,
        },
    },
};
use maud::{Markup, html};

const PAGE_DESCRIPTION: &str = "A domain entry blocks that host and, when it matches subdomains, every host under it. \
     A pattern such as *shop*.example.com matches the one label left of a registrable domain, so it blocks \
     shop.example.com and my-shop-2.example.com but never example.com itself. Patterns are matched against the \
     ASCII form of a host.";

pub fn url_domain_bans_page(
    config: &AdminConfig,
    auth: &AuthContext,
    flash: Option<&crate::api::types::FlashMessage>,
    csrf_token: &str,
    entries: Option<&BlocklistEntryPage>,
) -> Markup {
    let base = &config.base_path;
    let content = html! {
        (page_header("URL Domain Blocklist", Some(PAGE_DESCRIPTION)))
        div class="grid gap-6 lg:grid-cols-2" {
            (ban_card(base, csrf_token))
            (check_card(base, csrf_token))
        }
        div class="mt-6" {
            (unban_card(base, csrf_token))
        }
        div class="mt-6" {
            (entries_card(base, csrf_token, entries))
        }
    };
    admin_layout(
        config,
        auth,
        "URL Domain Blocklist",
        "url-domain-bans",
        flash,
        content,
    )
}

fn ban_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/url-domain-bans?action=ban&_csrf={csrf_token}");
    blocklist_action_card(
        "Ban URL Domain or Pattern",
        &action_url,
        csrf_token,
        html! {
            (blocklist_text_field("domain", "Domain or pattern", "example.com or *shop*.example.com", true))
            (checkbox("match_subdomains", "true", "Match subdomains (e.g. sub.example.com)", true, true))
            (blocklist_text_field("audit_log_reason", "Private reason (audit log, optional)", "Why is this ban being applied?", false))
        },
        "Ban",
        BlocklistActionVariant::Primary,
    )
}

fn check_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/url-domain-bans?action=check&_csrf={csrf_token}");
    blocklist_action_card(
        "Test a Host or URL",
        &action_url,
        csrf_token,
        html! {
            (blocklist_text_field("domain", "Host or URL", "shop-2.example.com or https://shop.example.com/x", true))
        },
        "Test",
        BlocklistActionVariant::Primary,
    )
}

fn unban_card(base: &str, csrf_token: &str) -> Markup {
    let action_url = format!("{base}/url-domain-bans?action=unban&_csrf={csrf_token}");
    blocklist_action_card(
        "Remove Domain or Pattern",
        &action_url,
        csrf_token,
        html! {
            (blocklist_text_field("domain", "Domain or pattern", "example.com or *shop*.example.com", true))
            (blocklist_text_field("audit_log_reason", "Private reason (audit log, optional)", "Why is this ban being removed?", false))
        },
        "Unban",
        BlocklistActionVariant::Danger,
    )
}

fn entries_card(base: &str, csrf_token: &str, entries: Option<&BlocklistEntryPage>) -> Markup {
    html! {
        div class="rounded-lg border border-neutral-200 bg-white p-4 shadow-sm sm:p-6" {
            div class="mb-4 flex items-center justify-between gap-4" {
                h3 class="text-base font-medium text-neutral-900" { "Blocked Domains and Patterns" }
                a href={(base) "/url-domain-bans"} class="text-sm text-brand-primary hover:underline" { "Refresh" }
            }
            @match entries {
                None => {
                    p class="text-sm text-red-700" { "Failed to load the blocklist entries" }
                }
                Some(page) => {
                    (entries_table(base, csrf_token, page))
                }
            }
        }
    }
}

fn entries_table(base: &str, csrf_token: &str, page: &BlocklistEntryPage) -> Markup {
    if page.items.is_empty() {
        return empty_state("No domains or patterns are blocked");
    }
    let next_after = page.next_after.as_deref().filter(|_| page.has_more);
    html! {
        (data_table(
            &["Value", "Kind", "Subdomains", "Category", "Added", ""],
            html! {
                @for entry in &page.items {
                    (entry_row(base, csrf_token, entry))
                }
            },
        ))
        @if let Some(next) = next_after {
            div class="mt-4" {
                a href={(base) "/url-domain-bans?after=" (urlencoding::encode(next))}
                    class="text-sm text-brand-primary hover:underline" {
                    "Next page"
                }
            }
        }
    }
}

fn entry_row(base: &str, csrf_token: &str, entry: &BlocklistEntry) -> Markup {
    let action_url = format!("{base}/url-domain-bans?action=unban&_csrf={csrf_token}");
    let is_pattern = entry.value.contains('*');
    table_row(html! {
        (table_cell(false, html! { code class="break-all" { (entry.value) } }))
        (table_cell(false, html! {
            @if is_pattern {
                (badge("Pattern", BadgeVariant::Info))
            } @else {
                (badge("Domain", BadgeVariant::Default))
            }
        }))
        (table_cell(true, html! {
            @if entry.match_subdomains.unwrap_or(true) { "Yes" } @else { "No" }
        }))
        (table_cell(true, html! { (entry.category.as_deref().unwrap_or("")) }))
        (table_cell(true, html! { (entry.created_at.as_deref().unwrap_or("")) }))
        (table_cell(false, html! {
            form method="post" action=(action_url) {
                (csrf_input(csrf_token))
                input type="hidden" name="domain" value=(entry.value);
                button type="submit" class="text-sm font-medium text-red-600 hover:text-red-700" {
                    "Remove"
                }
            }
        }))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn entry(value: &str, match_subdomains: bool) -> BlocklistEntry {
        BlocklistEntry {
            value: value.to_owned(),
            match_subdomains: Some(match_subdomains),
            category: Some("manual".to_owned()),
            created_at: None,
        }
    }

    #[test]
    fn entries_table_lists_patterns_with_remove_forms() {
        let page = BlocklistEntryPage {
            items: vec![
                entry("*shop*.example.com", false),
                entry("store.example.com", true),
            ],
            has_more: true,
            next_after: Some("store.example.com".to_owned()),
        };
        let markup = entries_table("/admin", "token", &page).into_string();
        assert!(markup.contains("*shop*.example.com"));
        assert!(markup.contains(">Pattern</span>"));
        assert!(markup.contains(">Domain</span>"));
        assert!(markup.contains(r#"name="domain" value="*shop*.example.com""#));
        assert!(markup.contains("/admin/url-domain-bans?action=unban&amp;_csrf=token"));
        assert!(markup.contains("/admin/url-domain-bans?after=store.example.com"));
    }

    #[test]
    fn entries_table_reports_an_empty_list() {
        let page = BlocklistEntryPage {
            items: Vec::new(),
            has_more: false,
            next_after: None,
        };
        let markup = entries_table("/admin", "token", &page).into_string();
        assert!(markup.contains("No domains or patterns are blocked"));
    }
}
