// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient,
    middleware::{auth::AuthContext, csrf},
    state::AppState,
    templates::{self, pages::voice_servers::VoiceServersPageParams},
};
use axum::{
    Router,
    extract::{Query, Request, State},
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;
use std::collections::HashMap;

#[derive(Deserialize)]
struct VoiceServersQuery {
    region_id: Option<String>,
}

async fn load_server_connection_counts(client: &AdminApiClient) -> HashMap<String, i64> {
    let response = match client.get_gateway_voice_state_counts().await {
        Ok(response) => response,
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: load voice state counts");
            return HashMap::new();
        }
    };
    let Some(servers) = response.data.get("servers").and_then(|v| v.as_array()) else {
        return HashMap::new();
    };
    servers
        .iter()
        .filter_map(|entry| {
            let server_id = entry.get("server_id")?.as_str()?.to_owned();
            let count = entry.get("voice_state_count")?.as_i64()?;
            Some((server_id, count))
        })
        .collect()
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(
            "/voice-regions",
            get(voice_regions_page).post(super::voice_actions::voice_regions_post),
        )
        .route(
            "/voice-servers",
            get(voice_servers_page).post(super::voice_actions::voice_servers_post),
        )
}

async fn voice_regions_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client.list_voice_regions(true).await {
        Ok(resp) => {
            let markup = templates::pages::voice_regions::voice_regions_page(
                config,
                &auth.0,
                Some(&resp.regions),
                None,
                &csrf_token,
            );
            Html(markup.into_string()).into_response()
        }
        Err(e) => {
            let msg = format!("{e:?}");
            let markup = templates::pages::voice_regions::voice_regions_page(
                config,
                &auth.0,
                None,
                Some(&msg),
                &csrf_token,
            );
            Html(markup.into_string()).into_response()
        }
    }
}

async fn voice_servers_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<VoiceServersQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);

    let region_id = match query.region_id.as_deref() {
        Some(id) if !id.is_empty() => id,
        _ => {
            let markup = templates::pages::voice_servers::voice_servers_page(
                config,
                &auth.0,
                &VoiceServersPageParams {
                    region_id: None,
                    region_name: None,
                    servers: None,
                    regions: &[],
                    join_tokens: None,
                    connection_counts: &HashMap::new(),
                    error: None,
                    csrf_token: &csrf_token,
                },
            );
            return Html(markup.into_string()).into_response();
        }
    };

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let connection_counts = load_server_connection_counts(&client).await;

    let regions = match client.list_voice_regions(false).await {
        Ok(resp) => resp.regions,
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: load voice regions");
            Vec::new()
        }
    };
    let region_name = regions
        .iter()
        .find(|r| r.id == region_id)
        .and_then(|r| r.name.clone())
        .unwrap_or_else(|| region_id.to_string());
    let join_tokens = client
        .list_voice_join_tokens()
        .await
        .map(|resp| resp.tokens)
        .inspect_err(
            |error| tracing::warn!(%error, "admin API request failed: load voice join tokens"),
        )
        .ok();

    match client.list_voice_servers(region_id).await {
        Ok(resp) => {
            let markup = templates::pages::voice_servers::voice_servers_page(
                config,
                &auth.0,
                &VoiceServersPageParams {
                    region_id: Some(region_id),
                    region_name: Some(&region_name),
                    servers: Some(&resp.servers),
                    regions: &regions,
                    join_tokens: join_tokens.as_deref(),
                    connection_counts: &connection_counts,
                    error: None,
                    csrf_token: &csrf_token,
                },
            );
            Html(markup.into_string()).into_response()
        }
        Err(e) => {
            let msg = format!("{e:?}");
            let markup = templates::pages::voice_servers::voice_servers_page(
                config,
                &auth.0,
                &VoiceServersPageParams {
                    region_id: Some(region_id),
                    region_name: Some(&region_name),
                    servers: None,
                    regions: &regions,
                    join_tokens: join_tokens.as_deref(),
                    connection_counts: &connection_counts,
                    error: Some(&msg),
                    csrf_token: &csrf_token,
                },
            );
            Html(markup.into_string()).into_response()
        }
    }
}
