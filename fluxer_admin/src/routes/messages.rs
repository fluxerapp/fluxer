// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{auth::AuthContext, csrf::CsrfToken},
    state::AppState,
    templates,
};
use axum::{
    Router,
    extract::{Query, State},
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;

#[derive(Deserialize)]
struct MessagesQuery {
    channel_id: Option<String>,
    message_id: Option<String>,
    attachment_id: Option<String>,
    filename: Option<String>,
    context_limit: Option<String>,
    before: Option<String>,
    after: Option<String>,
    search: Option<String>,
}

#[derive(Deserialize)]
struct ArchivesPageQuery {
    subject_type: Option<String>,
    subject_id: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(
            "/messages",
            get(messages_page).post(super::message_actions::messages_post),
        )
        .route(
            "/messages/browse-fragment",
            get(super::message_actions::messages_browse_fragment),
        )
        .route(
            "/system-dms",
            get(system_dms_page).post(super::message_actions::system_dms_post),
        )
        .route("/archives", get(archives_page))
        .route(
            "/archives/download",
            get(super::message_actions::archives_download),
        )
        .route(
            "/bulk-actions",
            get(bulk_actions_page).post(super::message_actions::bulk_actions_post),
        )
}

async fn messages_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<MessagesQuery>,
) -> Response {
    let config = state.config();
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let can_lookup = acl::has_permission(admin_acls, acl::MESSAGE_LOOKUP);
    let can_delete = acl::has_permission(admin_acls, acl::MESSAGE_DELETE);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let channel_id = query.channel_id.as_deref().filter(|s| !s.is_empty());
    let message_id = query.message_id.as_deref().filter(|s| !s.is_empty());
    let attachment_id = query.attachment_id.as_deref().filter(|s| !s.is_empty());
    let filename = query.filename.as_deref().filter(|s| !s.is_empty());
    let before = query.before.as_deref().filter(|s| !s.is_empty());
    let after = query.after.as_deref().filter(|s| !s.is_empty());
    let search = query.search.as_deref().filter(|s| !s.is_empty());
    let context_limit = super::message_actions::parse_context_limit(query.context_limit.as_deref());
    let mut lookup_result = None;
    let mut browse_result = None;
    let mut search_result = None;
    let mut error = None;
    if let (Some(cid), Some(aid), Some(name)) = (channel_id, attachment_id, filename) {
        lookup_result = match client
            .lookup_message_by_attachment(cid, aid, name, context_limit)
            .await
        {
            Ok(response) => Some(response.data),
            Err(err) => {
                tracing::warn!(%err, channel_id = cid, attachment_id = aid, "admin API request failed: lookup message by attachment");
                error = Some("Failed to look up message by attachment.".to_owned());
                None
            }
        };
    } else if let (Some(cid), Some(mid)) = (channel_id, message_id) {
        lookup_result = match client.lookup_message(cid, mid, context_limit).await {
            Ok(response) => Some(response.data),
            Err(err) => {
                tracing::warn!(%err, channel_id = cid, message_id = mid, "admin API request failed: lookup message");
                error = Some("Failed to look up message.".to_owned());
                None
            }
        };
    } else if let (Some(cid), Some(q)) = (channel_id, search) {
        search_result = match client.search_channel_messages(cid, q, None).await {
            Ok(response) => Some(response.data),
            Err(err) => {
                tracing::warn!(%err, channel_id = cid, "admin API request failed: search channel messages");
                error = Some("Failed to search channel messages.".to_owned());
                None
            }
        };
    } else if let Some(cid) = channel_id {
        browse_result = match client
            .browse_channel(cid, before, after, None)
            .await
            .log_error("browse channel messages")
        {
            Some(response) => Some(response.data),
            None => {
                error = Some("Failed to browse channel messages.".to_owned());
                None
            }
        };
    }
    let markup = templates::pages::messages_page::messages_page(
        config,
        &auth.0,
        &templates::pages::messages_page::MessagesPageParams {
            csrf_token: &csrf.0.0,
            prefill_channel_id: channel_id,
            can_lookup,
            can_delete,
            lookup_result: lookup_result.as_ref(),
            browse_result: browse_result.as_ref(),
            search_result: search_result.as_ref(),
            browse_channel_id: channel_id,
            search_query_text: search,
            context_limit,
            error: error.as_deref(),
        },
    );
    Html(markup.into_string()).into_response()
}

async fn system_dms_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
) -> Response {
    let config = state.config();
    let markup = templates::pages::system_dm::system_dm_page(
        config,
        &auth.0,
        &templates::pages::system_dm::SystemDmParams {
            form_error: None,
            csrf_token: &csrf.0.0,
        },
    );
    Html(markup.into_string()).into_response()
}

async fn archives_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<ArchivesPageQuery>,
) -> Response {
    let config = state.config();
    let subject_type = query.subject_type.as_deref().unwrap_or("all");
    let subject_id = query.subject_id.as_deref().filter(|s| !s.is_empty());

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = client
        .list_archives(subject_type, subject_id, false, None)
        .await;
    let (archives, error) = match result {
        Ok(resp) => (resp.archives, None),
        Err(e) => (vec![], Some(format!("{e}"))),
    };

    let markup = templates::pages::archives::archives_page(
        config,
        &auth.0,
        subject_type,
        subject_id,
        &archives,
        error.as_deref(),
    );
    Html(markup.into_string()).into_response()
}

async fn bulk_actions_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
) -> Response {
    let config = state.config();
    let markup = templates::pages::bulk_actions::bulk_actions_page(config, &auth.0, &csrf.0.0);
    Html(markup.into_string()).into_response()
}
