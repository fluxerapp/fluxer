// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        audit::SearchAuditLogsParams,
        client::{AdminApiClient, ApiResultExt},
    },
    config::AdminConfig,
    templates::{
        components::{
            section_card::section_card_simple,
            table::{table, table_container},
        },
        pages::{
            audit_logs_table::{audit_log_table_body, audit_log_table_headers},
            guild_detail_tabs as tabs,
        },
    },
};

pub struct GuildTabQuery {
    pub members_page: Option<u32>,
    pub reports_page: Option<u32>,
    pub audit_before: Option<String>,
}

pub async fn render(
    client: &AdminApiClient,
    config: &AdminConfig,
    csrf_token: &str,
    guild_id: &str,
    tab: &str,
    query: &GuildTabQuery,
    admin_acls: &[String],
) -> Option<maud::Markup> {
    let guild = client
        .get_guild_by_id(guild_id)
        .await
        .log_error("load guild tab guild")?;
    match tab {
        "members" => {
            let page = query.members_page.unwrap_or(0);
            let limit: u32 = 50;
            let resp = client
                .list_guild_members(guild_id, limit, page * limit)
                .await
                .log_error("load guild members")?;
            Some(tabs::members::members_tab(
                config, &guild, &resp, page, csrf_token,
            ))
        }
        "reports" => {
            if !acl::has_permission(admin_acls, acl::REPORT_VIEW) {
                return None;
            }
            let page = query.reports_page.unwrap_or(0);
            let limit: u32 = 25;
            let resp = client
                .search_reports_by_guild(guild_id, limit, page * limit)
                .await
                .log_error("load guild reports")?;
            Some(tabs::reports::reports_tab(
                config,
                &guild,
                &resp.reports,
                resp.total,
                page,
            ))
        }
        "emojis" => {
            if !acl::has_permission(admin_acls, acl::ASSET_PURGE) {
                return None;
            }
            let emojis = client
                .list_guild_emojis(guild_id)
                .await
                .map(|response| response.emojis)
                .map_err(|error| tracing::warn!(%error, guild_id, "admin API request failed: list guild emojis"))
                .unwrap_or_default();
            Some(tabs::emojis::emojis_tab(
                config, &guild, &emojis, csrf_token,
            ))
        }
        "stickers" => {
            if !acl::has_permission(admin_acls, acl::ASSET_PURGE) {
                return None;
            }
            let stickers = client
                .list_guild_stickers(guild_id)
                .await
                .map(|response| response.stickers)
                .map_err(|error| tracing::warn!(%error, guild_id, "admin API request failed: list guild stickers"))
                .unwrap_or_default();
            Some(tabs::stickers::stickers_tab(
                config, &guild, &stickers, csrf_token,
            ))
        }
        "audit_log" | "audit-log" => {
            if !acl::has_permission(admin_acls, acl::GUILD_AUDIT_LOG_VIEW) {
                return None;
            }
            let resp = client
                .list_guild_audit_logs(guild_id, Some(50), query.audit_before.as_deref())
                .await
                .log_error("load guild audit log")?;
            Some(tabs::audit_log::audit_log_tab(
                config,
                &guild,
                &resp.audit_log_entries,
                &resp.users,
            ))
        }
        "audit_logs" | "audit-logs" => {
            if !acl::has_permission(admin_acls, acl::AUDIT_LOG_VIEW) {
                return None;
            }
            let resp = client
                .search_audit_logs(&SearchAuditLogsParams {
                    query: None,
                    admin_user_id: None,
                    target_id: Some(guild_id.to_owned()),
                    target_type: Some("guild".to_owned()),
                    sort_by: Some("created_at".to_owned()),
                    sort_order: Some("desc".to_owned()),
                    limit: 50,
                    offset: 0,
                })
                .await
                .log_error("load guild admin audit logs");
            Some(section_card_simple(
                "Admin Audit Logs",
                maud::html! {
                    @if let Some(resp) = resp {
                        @if resp.logs.is_empty() {
                            p class="text-sm text-neutral-500" {
                                "No admin audit log entries for this guild."
                            }
                        } @else {
                            (table_container(table(maud::html! {
                                (audit_log_table_headers())
                                (audit_log_table_body(&config.base_path, &resp.logs))
                            })))
                        }
                    } @else {
                        p class="text-sm text-neutral-500" {
                            "Failed to load admin audit log entries."
                        }
                    }
                },
            ))
        }
        "applications" => {
            if !acl::has_any_permission(
                admin_acls,
                &[acl::APPLICATION_LOOKUP, acl::APPLICATION_LIST_BY_OWNER],
            ) {
                return None;
            }
            let apps = client
                .list_guild_applications(guild_id)
                .await
                .map_err(|error| tracing::warn!(%error, guild_id, "admin API request failed: list guild applications"))
                .unwrap_or_default();
            Some(tabs::applications::applications_tab(config, &guild, &apps))
        }
        "archives" => {
            if !acl::has_any_permission(
                admin_acls,
                &[acl::ARCHIVE_VIEW_ALL, acl::ARCHIVE_TRIGGER_GUILD],
            ) {
                return None;
            }
            let a = client
                .list_archives("guild", Some(guild_id), false, None)
                .await;
            Some(tabs::archives::archives_tab(
                config,
                &guild,
                &a.map_err(|error| tracing::warn!(%error, guild_id, "admin API request failed: list guild archives"))
                    .map(|r| r.archives)
                    .unwrap_or_default(),
                csrf_token,
            ))
        }
        _ => None,
    }
}
