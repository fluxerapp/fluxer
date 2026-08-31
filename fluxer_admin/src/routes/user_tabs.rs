// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        audit::SearchAuditLogsParams,
        client::{AdminApiClient, ApiResultExt},
    },
    config::AdminConfig,
    templates::{
        components::audit_logs_for_target::audit_logs_for_target, pages::user_detail_tabs as tabs,
    },
};

pub struct TabQuery {
    pub reports_sent_page: Option<u32>,
    pub reports_received_page: Option<u32>,
    pub reports_limit: Option<u32>,
    pub dm_before: Option<String>,
    pub dm_after: Option<String>,
    pub dm_limit: Option<u32>,
    pub audit_logs_page: Option<u32>,
    pub message_shred_job_id: Option<String>,
    pub delete_all_messages_dry_run: Option<String>,
    pub delete_all_messages_channel_count: Option<u64>,
    pub delete_all_messages_message_count: Option<u64>,
}

pub async fn render(
    client: &AdminApiClient,
    config: &AdminConfig,
    csrf_token: &str,
    user_id: &str,
    tab: &str,
    query: &TabQuery,
    admin_acls: &[String],
) -> Option<maud::Markup> {
    match tab {
        "overview" => {
            let u = client
                .get_user_by_id(user_id)
                .await
                .log_error("load user overview")?;
            let change_log = if acl::has_permission(admin_acls, acl::USER_VIEW_CONTACT_LOG) {
                client
                    .list_user_change_log(user_id, Some(50))
                    .await
                    .log_error("load user change log")
            } else {
                None
            };
            let limit_config = client
                .get_limit_config()
                .await
                .log_error("load limit config for user overview");
            Some(tabs::overview::overview_tab_with_limit_config(
                config,
                &u,
                admin_acls,
                csrf_token,
                change_log.as_ref(),
                limit_config.as_ref(),
            ))
        }
        "account" => {
            let u = client
                .get_user_by_id(user_id)
                .await
                .log_error("load user account")?;
            let s = client
                .list_user_sessions(user_id)
                .await
                .map(|r| r.sessions)
                .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user sessions"))
                .unwrap_or_default();
            let webauthn_credentials = if u.authenticator_types.contains(&2) {
                client
                    .list_webauthn_credentials(user_id)
                    .await
                    .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list webauthn credentials"))
                    .unwrap_or_default()
            } else {
                Vec::new()
            };
            Some(tabs::account::account_tab(
                config,
                &u,
                &s,
                &webauthn_credentials,
                csrf_token,
            ))
        }
        "moderation" => {
            let u = client
                .get_user_by_id(user_id)
                .await
                .log_error("load user moderation")?;
            let message_shred_status = if let Some(job_id) = query
                .message_shred_job_id
                .as_deref()
                .filter(|job_id| !job_id.trim().is_empty())
            {
                Some(client.get_message_shred_status(job_id).await)
            } else {
                None
            };
            let delete_all_messages_dry_run = parse_bool_flag(
                query
                    .delete_all_messages_dry_run
                    .as_deref()
                    .unwrap_or_default(),
            )
            .unwrap_or(false)
            .then_some((
                query.delete_all_messages_channel_count.unwrap_or(0),
                query.delete_all_messages_message_count.unwrap_or(0),
            ));
            Some(tabs::moderation::moderation_tab(
                config,
                &u,
                csrf_token,
                admin_acls,
                query.message_shred_job_id.as_deref(),
                message_shred_status.as_ref(),
                delete_all_messages_dry_run,
            ))
        }
        "applications" => {
            if !acl::has_permission(admin_acls, acl::APPLICATION_LIST_BY_OWNER) {
                return None;
            }
            let apps = client
                .list_user_applications(user_id)
                .await
                .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user applications"))
                .unwrap_or_default();
            Some(tabs::applications::applications_tab(config, &apps))
        }
        "archives" => {
            if !acl::has_any_permission(
                admin_acls,
                &[acl::ARCHIVE_VIEW_ALL, acl::ARCHIVE_TRIGGER_USER],
            ) {
                return None;
            }
            let a = client
                .list_archives("user", Some(user_id), false, None)
                .await;
            Some(tabs::archives::archives_tab(
                config,
                user_id,
                &a.map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user archives"))
                    .map(|r| r.archives)
                    .unwrap_or_default(),
                csrf_token,
            ))
        }
        "guilds" => {
            let g = client
                .get_user_guilds(user_id, Some(200), None, None, Some(true))
                .await
                .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user guilds"))
                .unwrap_or_default();
            Some(tabs::guilds::guilds_tab(config, user_id, &g))
        }
        "reports" => {
            let lim = query.reports_limit.unwrap_or(25);
            let sp = query.reports_sent_page.unwrap_or(0);
            let rp = query.reports_received_page.unwrap_or(0);
            let sent = client
                .search_reports_by_reporter(user_id, lim, sp * lim)
                .await
                .log_error("load reports sent by user");
            let recv = client
                .search_reports_by_reported_user(user_id, lim, rp * lim)
                .await
                .log_error("load reports against user");
            Some(tabs::reports::reports_tab(
                config,
                user_id,
                sent.as_ref(),
                recv.as_ref(),
                sp,
                rp,
                lim,
            ))
        }
        "relationships" => {
            if !acl::has_permission(admin_acls, acl::USER_LIST_RELATIONSHIPS) {
                return None;
            }
            let d = client
                .list_user_relationships(user_id)
                .await
                .log_error("load user relationships")?;
            Some(tabs::relationships::relationships_tab(
                config,
                user_id,
                &d,
                acl::has_permission(admin_acls, acl::USER_REMOVE_RELATIONSHIP),
                csrf_token,
            ))
        }
        "dm-history" | "dm_history" => {
            let limit = query.dm_limit.unwrap_or(50);
            let before = query.dm_before.as_deref();
            let after = query.dm_after.as_deref();
            let ch = client
                .list_user_dm_channels(user_id, before, after, Some(limit))
                .await
                .map(|r| r.channels)
                .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user DM channels"))
                .unwrap_or_default();
            Some(tabs::dm_history::dm_history_tab(
                config,
                user_id,
                &ch,
                before,
                after,
                limit as usize,
            ))
        }
        "group-dm" | "group_dms" => {
            let ch = client
                .list_user_group_dm_channels(user_id)
                .await
                .map(|r| r.channels)
                .map_err(|error| tracing::warn!(%error, user_id, "admin API request failed: list user group DM channels"))
                .unwrap_or_default();
            Some(tabs::group_dm::group_dm_tab(config, user_id, &ch))
        }
        "audit_logs" | "audit-logs" => {
            if !acl::has_permission(admin_acls, acl::AUDIT_LOG_VIEW) {
                return None;
            }
            let page = query.audit_logs_page.unwrap_or(0);
            let limit = 50;
            let resp = client
                .search_audit_logs(&SearchAuditLogsParams {
                    query: None,
                    admin_user_id: None,
                    target_id: Some(user_id.to_owned()),
                    target_type: Some("user".to_owned()),
                    sort_by: Some("created_at".to_owned()),
                    sort_order: Some("desc".to_owned()),
                    limit,
                    offset: page * limit,
                })
                .await
                .log_error("load user admin audit logs")?;
            Some(audit_logs_for_target(
                &config.base_path,
                &resp.logs,
                user_id,
                page,
                resp.total,
                &format!("{}/users/{}?tab=audit_logs", config.base_path, user_id),
            ))
        }
        _ => None,
    }
}

fn parse_bool_flag(value: &str) -> Option<bool> {
    match value.trim().to_ascii_lowercase().as_str() {
        "1" | "true" => Some(true),
        "0" | "false" => Some(false),
        _ => None,
    }
}
