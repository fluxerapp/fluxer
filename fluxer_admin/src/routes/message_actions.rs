// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::client::AdminApiClient,
    middleware::{
        auth::AuthContext,
        flash::{self, FlashData},
    },
    state::AppState,
    utils::forms::MultiValueForm,
};
use axum::{
    Json,
    extract::{Query, Request, State},
    http::StatusCode,
    response::{Html, IntoResponse, Redirect, Response},
};
use serde::Deserialize;

use super::ActionQuery;

#[derive(Deserialize)]
pub(crate) struct BrowseFragmentQuery {
    channel_id: Option<String>,
    before: Option<String>,
    after: Option<String>,
}

#[derive(Deserialize)]
pub(crate) struct ArchiveDownloadQuery {
    subject_type: Option<String>,
    subject_id: Option<String>,
    archive_id: Option<String>,
}

pub(crate) async fn messages_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/messages"),
                FlashData::error("Invalid form data"),
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let channel_id = form.clean("channel_id");
    let message_id = form.clean("message_id");
    match action {
        "lookup" => {
            let context_limit = parse_context_limit(form.first("context_limit"));
            return Redirect::to(&format!(
                "{base}/messages?channel_id={}&message_id={}&context_limit={context_limit}",
                encode_opt(&channel_id),
                encode_opt(&message_id)
            ))
            .into_response();
        }
        "lookup-by-attachment" => {
            let attachment_id = form.clean("attachment_id");
            let filename = form.clean("filename");
            let context_limit = parse_context_limit(form.first("context_limit"));
            return Redirect::to(&format!(
                "{base}/messages?channel_id={}&attachment_id={}&filename={}&context_limit={context_limit}",
                encode_opt(&channel_id),
                encode_opt(&attachment_id),
                encode_opt(&filename)
            ))
            .into_response();
        }
        "browse" => {
            return Redirect::to(&format!(
                "{base}/messages?channel_id={}",
                encode_opt(&channel_id)
            ))
            .into_response();
        }
        "search" => {
            let search = form.clean("search");
            return Redirect::to(&format!(
                "{base}/messages?channel_id={}&search={}",
                encode_opt(&channel_id),
                encode_opt(&search)
            ))
            .into_response();
        }
        "delete" => {
            let (Some(cid), Some(mid)) = (&channel_id, &message_id) else {
                return json_error(StatusCode::BAD_REQUEST, "Missing channel_id or message_id");
            };
            let audit_log_reason = form.clean("audit_log_reason");
            return match client
                .delete_message(cid, mid, audit_log_reason.as_deref())
                .await
            {
                Ok(()) => Json(serde_json::json!({"success": true})).into_response(),
                Err(e) => json_error(StatusCode::BAD_REQUEST, &format!("{e}")),
            };
        }
        "report-to-ncmec" => {
            let attachment_id = form.clean("attachment_id");
            let filename = form.clean("filename");
            let reporter_full_name = form.clean("reporter_full_name");
            let source_report_id = form.clean("source_report_id");
            let confirmed_viewed = form.bool_value("confirmed_viewed");
            let (Some(cid), Some(mid), Some(aid), Some(name), Some(reporter)) = (
                &channel_id,
                &message_id,
                &attachment_id,
                &filename,
                &reporter_full_name,
            ) else {
                return json_error(
                    StatusCode::BAD_REQUEST,
                    "Missing required NCMEC report fields",
                );
            };
            if !confirmed_viewed {
                return json_error(
                    StatusCode::BAD_REQUEST,
                    "Missing required NCMEC report fields",
                );
            }
            return match client
                .report_attachment_to_ncmec(
                    cid,
                    mid,
                    aid,
                    name,
                    reporter,
                    source_report_id.as_deref(),
                )
                .await
            {
                Ok(resp) => Json(resp.data).into_response(),
                Err(e) => json_error(StatusCode::BAD_REQUEST, &format!("{e}")),
            };
        }
        _ => {}
    }
    Redirect::to(&format!("{base}/messages")).into_response()
}

pub(crate) async fn system_dms_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/system-dms"),
                FlashData::error("Invalid form data"),
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
    let content = form.clean("content");
    let flash = if let Some(content) = content.as_deref()
        && !user_ids.is_empty()
    {
        match client.send_system_dm(&user_ids, content).await {
            Ok(_) => FlashData::success("System DM sent"),
            Err(error) => {
                tracing::warn!(%error, "admin API request failed: send system DM");
                FlashData::error("Failed to send system DM")
            }
        }
    } else {
        FlashData::error("Recipients and content are required")
    };
    flash::redirect_with_flash(&format!("{base}/system-dms"), flash, config.is_production())
}

pub(crate) async fn bulk_actions_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/bulk-actions"),
                FlashData::error("Invalid form data"),
                config.is_production(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let audit_log_reason = form.clean("audit_log_reason");
    let result = match action {
        "bulk-update-user-flags" => {
            let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
            let add = form.list_values_any(&["add_flags[]", "add_flags"]);
            let remove = form.list_values_any(&["remove_flags[]", "remove_flags"]);
            client
                .bulk_update_user_flags(&user_ids, &add, &remove, audit_log_reason.as_deref())
                .await
        }
        "bulk-update-suspicious-activity-flags" => {
            let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
            let add = form.list_values_any(&["add_flags[]", "add_flags"]);
            let remove = form.list_values_any(&["remove_flags[]", "remove_flags"]);
            client
                .bulk_update_suspicious_activity_flags(
                    &user_ids,
                    &add,
                    &remove,
                    audit_log_reason.as_deref(),
                )
                .await
        }
        "bulk-update-guild-features" => {
            let guild_ids = form.list_values_any(&["guild_ids[]", "guild_ids"]);
            let mut add = form.list_values_any(&["add_features[]", "add_features"]);
            add.extend(form.list_values_any(&["custom_add_features"]));
            let mut remove = form.list_values_any(&["remove_features[]", "remove_features"]);
            remove.extend(form.list_values_any(&["custom_remove_features"]));
            client
                .bulk_update_guild_features(&guild_ids, &add, &remove, audit_log_reason.as_deref())
                .await
        }
        "bulk-add-guild-members" => {
            let guild_id = form.clean("guild_id").unwrap_or_default();
            let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
            client
                .bulk_add_guild_members(&guild_id, &user_ids, audit_log_reason.as_deref())
                .await
        }
        "bulk-schedule-user-deletion" => {
            let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
            let reason_code = form.parse_u32("reason_code").unwrap_or(2);
            let days = form.parse_u32("days_until_deletion").unwrap_or(14);
            let public_reason = form.clean("public_reason");
            client
                .bulk_schedule_user_deletion(
                    &user_ids,
                    reason_code,
                    days,
                    public_reason.as_deref(),
                    audit_log_reason.as_deref(),
                )
                .await
        }
        "bulk_delete_users" => {
            let user_ids = form.list_values_any(&["user_ids[]", "user_ids"]);
            client
                .bulk_schedule_user_deletion(&user_ids, 0, 30, None, audit_log_reason.as_deref())
                .await
        }
        _ => {
            return flash::redirect_with_flash(
                &format!("{base}/bulk-actions"),
                FlashData::error("Unknown bulk action"),
                config.is_production(),
            );
        }
    };
    match result {
        Ok(response) => {
            if let Some(job_id) = response.job_id.filter(|job_id| !job_id.is_empty()) {
                Redirect::to(&format!("{base}/jobs/{job_id}")).into_response()
            } else {
                flash::redirect_with_flash(
                    &format!("{base}/bulk-actions"),
                    FlashData::success("Bulk action submitted"),
                    config.is_production(),
                )
            }
        }
        Err(error) => {
            tracing::warn!(%error, action, "admin API request failed: submit bulk action");
            flash::redirect_with_flash(
                &format!("{base}/bulk-actions"),
                FlashData::error("Failed to submit bulk action"),
                config.is_production(),
            )
        }
    }
}

pub(crate) async fn messages_browse_fragment(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<BrowseFragmentQuery>,
) -> Response {
    let empty_fragment = || {
        Html(
            r#"<div data-browse-fragment="" data-has-more="false" data-message-count="0"></div>"#
                .to_string(),
        )
        .into_response()
    };
    let channel_id = match query.channel_id.as_deref().filter(|s| !s.is_empty()) {
        Some(id) => id,
        None => return empty_fragment(),
    };
    let config = state.config();
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let can_delete = acl::has_permission(admin_acls, acl::MESSAGE_DELETE);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client
        .browse_channel(
            channel_id,
            query.before.as_deref(),
            query.after.as_deref(),
            None,
        )
        .await;
    match result {
        Ok(resp) => {
            let markup = crate::templates::pages::messages_page::browse_messages_fragment(
                config, &resp.data, can_delete, None,
            );
            Html(markup.into_string()).into_response()
        }
        Err(error) => {
            tracing::warn!(%error, channel_id, "admin API request failed: browse messages fragment");
            (
                StatusCode::BAD_GATEWAY,
                Html(
                    r#"<div data-browse-fragment="" data-has-more="false" data-message-count="0"></div>"#
                        .to_string(),
                ),
            )
                .into_response()
        }
    }
}

fn parse_context_limit(value: Option<&str>) -> u32 {
    value
        .and_then(|s| s.parse::<u32>().ok())
        .filter(|n| *n > 0)
        .unwrap_or(50)
        .min(100)
}

fn encode_opt(value: &Option<String>) -> String {
    urlencoding::encode(value.as_deref().unwrap_or("")).into_owned()
}

fn json_error(status: StatusCode, message: &str) -> Response {
    (
        status,
        Json(serde_json::json!({"success": false, "error": message})),
    )
        .into_response()
}

pub(crate) async fn archives_download(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<ArchiveDownloadQuery>,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let (subject_type, subject_id, archive_id) = match (
        query.subject_type.as_deref().filter(|s| !s.is_empty()),
        query.subject_id.as_deref().filter(|s| !s.is_empty()),
        query.archive_id.as_deref().filter(|s| !s.is_empty()),
    ) {
        (Some(st), Some(si), Some(ai)) => (st, si, ai),
        _ => return Redirect::to(&format!("{base}/archives")).into_response(),
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client
        .get_archive_download_url(subject_type, subject_id, archive_id)
        .await
    {
        Ok(resp) if !resp.download_url.is_empty() => {
            Redirect::temporary(&resp.download_url).into_response()
        }
        Ok(_) => flash::redirect_with_flash(
            &format!("{base}/archives"),
            FlashData::error("Archive download URL was empty"),
            config.is_production(),
        ),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: get archive download URL");
            flash::redirect_with_flash(
                &format!("{base}/archives"),
                FlashData::error("Failed to create archive download URL"),
                config.is_production(),
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn form_list_values_accepts_repeated_comma_and_newline_values() {
        let form = MultiValueForm::parse(b"user_ids%5B%5D=1&user_ids%5B%5D=2%2C3&user_ids=4%0A5");
        assert_eq!(
            form.list_values_any(&["user_ids[]", "user_ids"]),
            vec![
                "1".to_owned(),
                "2".to_owned(),
                "3".to_owned(),
                "4".to_owned(),
                "5".to_owned()
            ]
        );
    }
}
