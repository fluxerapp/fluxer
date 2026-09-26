// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::ApiResultExt,
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

use super::system_actions;

#[derive(Deserialize)]
struct GatewayQuery {
    leaderboard_limit: Option<String>,
    node_stats: Option<String>,
}

#[derive(Deserialize)]
struct AuditLogsQuery {
    q: Option<String>,
    admin_user_id: Option<String>,
    target_id: Option<String>,
    target_type: Option<String>,
    access: Option<String>,
    sort_by: Option<String>,
    sort_order: Option<String>,
    limit: Option<u32>,
    page: Option<u32>,
}

#[derive(Deserialize)]
struct SearchIndexQuery {
    job_id: Option<String>,
}

#[derive(Deserialize)]
struct LimitConfigQuery {
    rule: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(
            "/gateway",
            get(gateway_page).post(system_actions::gateway_post),
        )
        .route("/audit-logs", get(audit_logs_page))
        .route(
            "/search-index",
            get(search_index_page).post(system_actions::search_index_post),
        )
        .route(
            "/instance-config",
            get(instance_config_page).post(system_actions::instance_config_post),
        )
        .route(
            "/limit-config",
            get(limit_config_page).post(system_actions::limit_config_post),
        )
        .route("/strange-place", get(strange_place_page))
}

async fn gateway_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<GatewayQuery>,
) -> Response {
    let config = state.config();
    let node_stats_expanded = query.node_stats.as_deref() == Some("expanded");
    let leaderboard_limit: u32 = query
        .leaderboard_limit
        .as_deref()
        .and_then(|s| s.parse().ok())
        .unwrap_or(100);
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let node_stats = client
        .get_node_stats()
        .await
        .log_error("load gateway node stats");
    let guild_stats = client
        .get_guild_memory_stats(leaderboard_limit)
        .await
        .log_error("load gateway guild memory stats");
    let voice_counts = client
        .get_gateway_voice_state_counts()
        .await
        .log_error("load gateway voice state counts");
    let gw_params = templates::pages::gateway::GatewayPageParams {
        csrf_token: &csrf.0.0,
        node_stats: node_stats.as_ref(),
        voice_counts: voice_counts.as_ref(),
        guild_stats: guild_stats.as_ref(),
        reload_result: None,
        node_stats_expanded,
        leaderboard_limit,
    };
    let markup = templates::pages::gateway::gateway_page(config, &auth.0, &gw_params);
    Html(markup.into_string()).into_response()
}

async fn audit_logs_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<AuditLogsQuery>,
) -> Response {
    let config = state.config();
    let limit = query.limit.unwrap_or(50);
    let current_page = query.page.unwrap_or(0);
    let params = templates::pages::audit_logs::AuditLogsParams {
        query: query.q.as_deref().unwrap_or(""),
        admin_user_id: query.admin_user_id.as_deref().unwrap_or(""),
        target_id: query.target_id.as_deref().unwrap_or(""),
        target_type: query.target_type.as_deref().unwrap_or(""),
        access: query.access.as_deref().unwrap_or(""),
        sort_by: query.sort_by.as_deref().unwrap_or("createdAt"),
        sort_order: query.sort_order.as_deref().unwrap_or("desc"),
        limit,
        current_page,
    };
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let search_params = crate::api::audit::SearchAuditLogsParams {
        query: nonempty(params.query),
        admin_user_id: nonempty(params.admin_user_id),
        target_id: nonempty(params.target_id),
        target_type: nonempty(params.target_type),
        access: nonempty(params.access),
        sort_by: Some(params.sort_by.to_owned()),
        sort_order: Some(params.sort_order.to_owned()),
        limit,
        offset: u64::from(current_page) * u64::from(limit),
    };
    let result = client
        .search_audit_logs(&search_params)
        .await
        .log_error("load audit logs");
    let markup =
        templates::pages::audit_logs::audit_logs_page(config, &auth.0, &params, result.as_ref());
    Html(markup.into_string()).into_response()
}

async fn search_index_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<SearchIndexQuery>,
) -> Response {
    let config = state.config();
    let job_id_str = query.job_id.unwrap_or_default();

    let mut status: Option<String> = None;
    let mut total: Option<u64> = None;
    let mut indexed: Option<u64> = None;

    if !job_id_str.is_empty() {
        let client =
            crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
        match client.get_index_refresh_status(&job_id_str).await {
            Ok(resp) => match resp {
                crate::api::types::IndexRefreshStatusResponse::NotFound { status: s } => {
                    status = Some(s);
                }
                crate::api::types::IndexRefreshStatusResponse::Progress {
                    status: s,
                    total: t,
                    indexed: i,
                    ..
                } => {
                    status = Some(s);
                    total = t;
                    indexed = i;
                }
            },
            Err(error) => {
                tracing::warn!(%error, job_id = job_id_str, "admin API request failed: get index refresh status");
            }
        }
    }

    let refresh = if !job_id_str.is_empty() {
        Some(templates::pages::search_index::RefreshStatus {
            job_id: &job_id_str,
            status: status.as_deref(),
            total,
            indexed,
            started_at: None,
            completed_at: None,
            error: None,
        })
    } else {
        None
    };

    let markup = templates::pages::search_index::search_index_page(
        config,
        &auth.0,
        &csrf.0.0,
        refresh.as_ref(),
    );
    Html(markup.into_string()).into_response()
}

async fn instance_config_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
) -> Response {
    let config = state.config();
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let instance_config = client
        .get_instance_config()
        .await
        .log_error("load instance config");
    let limit_config = client
        .get_limit_config()
        .await
        .log_error("load limit config");
    let markup = templates::pages::instance_config::instance_config_page(
        config,
        &auth.0,
        &csrf.0.0,
        instance_config.as_ref(),
        limit_config.as_ref(),
    );
    Html(markup.into_string()).into_response()
}

async fn limit_config_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Query(query): Query<LimitConfigQuery>,
) -> Response {
    let config = state.config();
    let client =
        crate::api::client::AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let limit_config = client
        .get_limit_config()
        .await
        .log_error("load limit config page");
    let markup = templates::pages::limit_config::limit_config_page(
        config,
        &auth.0,
        &csrf.0.0,
        query.rule.as_deref(),
        limit_config.as_ref(),
    );
    Html(markup.into_string()).into_response()
}

async fn strange_place_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
) -> Response {
    let config = state.config();
    let markup = templates::pages::strange_place::strange_place_page(config, &auth.0);
    Html(markup.into_string()).into_response()
}

fn nonempty(s: &str) -> Option<String> {
    if s.is_empty() {
        None
    } else {
        Some(s.to_owned())
    }
}
