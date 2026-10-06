// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::types::AuditLogsListResponse,
    config::AdminConfig,
    middleware::auth::AuthContext,
    templates::{
        components::{
            form::{secondary_button_link, select_input, submit_button, text_input},
            page_container::page_header_with_actions,
            table::{empty_state, table, table_container},
        },
        layout::admin_layout,
    },
};
use maud::{Markup, PreEscaped, html};

use super::audit_logs_table::{audit_log_table_body, audit_log_table_headers};

pub struct AuditLogsParams<'a> {
    pub query: &'a str,
    pub admin_user_id: &'a str,
    pub target_id: &'a str,
    pub target_type: &'a str,
    pub access: &'a str,
    pub sort_by: &'a str,
    pub sort_order: &'a str,
    pub limit: u32,
    pub current_page: u32,
}

fn filters_section(base: &str, params: &AuditLogsParams<'_>) -> Markup {
    let target_type_options: &[(&str, &str)] = &[
        ("", "Any"),
        ("user", "User"),
        ("guild", "Guild"),
        ("bulk_job", "Bulk job"),
        ("email_domain", "Email domain"),
        ("ip", "IP"),
        ("phrase", "Phrase"),
        ("url", "URL"),
        ("url_domain", "URL domain"),
        ("file_sha", "File SHA"),
        ("email", "Email"),
    ];
    let access_options: &[(&str, &str)] = &[
        ("", "All entries"),
        ("write", "Writes only"),
        ("read", "Reads only"),
    ];
    let sort_options: &[(&str, &str)] = &[("createdAt", "Created at"), ("relevance", "Relevance")];
    let order_options: &[(&str, &str)] = &[("desc", "Newest first"), ("asc", "Oldest first")];
    let limit_options: &[(&str, &str)] =
        &[("25", "25"), ("50", "50"), ("100", "100"), ("200", "200")];
    let limit_str = params.limit.to_string();

    html! {
        form method="get" action={(base) "/audit-logs"}
            class="rounded-lg bg-white border border-neutral-200 p-4" {
            div class="grid grid-cols-1 gap-4 sm:grid-cols-2 lg:grid-cols-4" {
                (text_input("q", "Search", params.query,
                    "Search by action, reason, or metadata..."))
                (text_input("target_id", "Target ID", params.target_id,
                    "Filter by target ID..."))
                (text_input("admin_user_id", "Admin User ID", params.admin_user_id,
                    "Filter by admin user ID..."))
                (select_input("target_type", "Target type",
                    target_type_options, params.target_type))
                (select_input("access", "Access", access_options, params.access))
                (select_input("sort_by", "Sort by", sort_options, params.sort_by))
                (select_input("sort_order", "Order", order_options, params.sort_order))
                (select_input("limit", "Page size", limit_options, &limit_str))
            }
            div class="mt-4 flex flex-wrap gap-2" {
                (submit_button("Search"))
                (secondary_button_link("Clear", &format!("{base}/audit-logs")))
            }
        }
    }
}

fn build_pagination_url(base: &str, page: u32, params: &AuditLogsParams<'_>) -> String {
    let mut query = url::form_urlencoded::Serializer::new(String::new());
    query.append_pair("page", &page.to_string());
    query.extend_pairs(
        [
            ("q", params.query),
            ("admin_user_id", params.admin_user_id),
            ("target_id", params.target_id),
            ("target_type", params.target_type),
            ("access", params.access),
            ("sort_by", params.sort_by),
            ("sort_order", params.sort_order),
        ]
        .into_iter()
        .filter(|(_, value)| !value.is_empty()),
    );
    query.append_pair("limit", &params.limit.to_string());
    format!("{base}/audit-logs?{}", query.finish())
}

pub fn audit_logs_page(
    config: &AdminConfig,
    auth: &AuthContext,
    params: &AuditLogsParams<'_>,
    result: Option<&AuditLogsListResponse>,
) -> Markup {
    let base = &config.base_path;
    let content = match result {
        Some(data) => {
            let total = data.total;
            let entries = &data.logs;
            let total_pages = if params.limit > 0 {
                total.div_ceil(u64::from(params.limit)).max(1)
            } else {
                1
            };
            let page_number = u64::from(params.current_page) + 1;
            let next_page = params
                .current_page
                .checked_add(1)
                .filter(|page| u64::from(*page) < total_pages);
            let showing = format!("Showing {} of {} entries", entries.len(), total);
            html! {
                (page_header_with_actions("Audit Logs", None, html! {
                    span class="text-sm text-neutral-500" { (showing) }
                }))
                (filters_section(base, params))
                @if entries.is_empty() {
                    (empty_state("No audit logs found. Try adjusting your filters."))
                } @else {
                    (table_container(table(html! {
                        (audit_log_table_headers())
                        (audit_log_table_body(base, entries))
                    })))
                }
                @if total > params.limit as u64 {
                    div class="mt-4 flex items-center justify-between" {
                        @if params.current_page > 0 {
                            a href=(build_pagination_url(base, params.current_page - 1, params))
                                class="text-sm text-neutral-900 underline" {
                                (PreEscaped("&larr; Previous"))
                            }
                        } @else { span {} }
                        span class="text-sm text-neutral-500" {
                            "Page " (page_number) " of " (total_pages)
                        }
                        @if let Some(page) = next_page {
                            a href=(build_pagination_url(base, page, params))
                                class="text-sm text-neutral-900 underline" {
                                (PreEscaped("Next &rarr;"))
                            }
                        } @else { span {} }
                    }
                }
            }
        }
        None => {
            html! {
                (page_header_with_actions("Audit Logs", None, html! {}))
                (filters_section(base, params))
                (empty_state("Failed to load audit logs."))
            }
        }
    };
    admin_layout(config, auth, "Audit Logs", "audit-logs", None, content)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn target_type_filter_offers_bulk_jobs() {
        let params = AuditLogsParams {
            query: "",
            admin_user_id: "",
            target_id: "",
            target_type: "bulk_job",
            access: "",
            sort_by: "createdAt",
            sort_order: "desc",
            limit: 50,
            current_page: 0,
        };
        let markup = filters_section("/admin", &params).into_string();
        assert!(markup.contains(r#"<option value="bulk_job" selected>Bulk job</option>"#));
        assert!(markup.contains(r#"<option value="" selected>All entries</option>"#));
    }

    #[test]
    fn access_filter_survives_form_and_pagination() {
        let params = AuditLogsParams {
            query: "",
            admin_user_id: "",
            target_id: "1500000000000000001",
            target_type: "",
            access: "read",
            sort_by: "createdAt",
            sort_order: "desc",
            limit: 50,
            current_page: 0,
        };
        let markup = filters_section("/admin", &params).into_string();
        assert!(markup.contains(r#"<select id="access" name="access""#));
        assert!(markup.contains(r#"<option value="read" selected>Reads only</option>"#));
        assert_eq!(
            build_pagination_url("/admin", 1, &params),
            "/admin/audit-logs?page=1&target_id=1500000000000000001&access=read&sort_by=createdAt&sort_order=desc&limit=50"
        );
    }
}
