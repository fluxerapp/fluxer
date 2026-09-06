// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::{AdminApiClient, ApiResultExt},
    middleware::{
        auth::AuthContext,
        csrf,
        flash::{self, FlashData},
        htmx,
    },
    routes::guild_tabs,
    state::AppState,
    templates,
    utils::forms::MultiValueForm,
};
use axum::{
    Router,
    extract::{Path, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
    routing::get,
};
use serde::Deserialize;
use std::collections::BTreeSet;

#[derive(Deserialize)]
struct GuildListQuery {
    q: Option<String>,
    ids: Option<String>,
    limit: Option<u32>,
    page: Option<u32>,
}

#[derive(Deserialize)]
struct GuildDetailQuery {
    tab: Option<String>,
    page: Option<u32>,
    reports_page: Option<u32>,
    before: Option<String>,
}

#[derive(Deserialize)]
struct ActionQuery {
    action: Option<String>,
    tab: Option<String>,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/guilds", get(guilds_list))
        .route(
            "/guilds/{guild_id}",
            get(guild_detail).post(guild_detail_post),
        )
        .route("/guilds/{guild_id}/tabs/{tab}", get(guild_tab))
}

async fn guilds_list(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<GuildListQuery>,
) -> Response {
    let config = state.config();
    let params = templates::pages::guilds_list::GuildsListParams::from_query(
        query.q,
        query.ids,
        query.limit,
        query.page,
    );
    let is_results_fragment = htmx::targets(&headers, "guilds-results");

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let results = if params.has_id_lookup() {
        let mut guilds = Vec::new();
        for guild_id in &params.requested_ids {
            match client.lookup_guild(guild_id).await {
                Ok(Some(guild)) => guilds.push(guild.into()),
                Ok(None) => {}
                Err(error) => {
                    tracing::warn!(%error, guild_id, "admin API request failed: lookup guild by id");
                }
            }
        }
        Some((guilds, Some(params.requested_ids.len() as u64), false))
    } else if params.has_search() {
        let offset = params.page.saturating_mul(params.limit);
        client
            .search_guilds(params.search_query(), params.limit, offset)
            .await
            .log_error("search guilds")
            .map(|response| {
                let has_more = u64::from(offset) + (response.guilds.len() as u64) < response.total;
                (response.guilds, Some(response.total), has_more)
            })
    } else {
        None
    };
    let result_guilds = results.as_ref().map(|result| result.0.as_slice());
    let total = results.as_ref().and_then(|result| result.1);
    let has_more = results.as_ref().is_some_and(|result| result.2);

    let markup = templates::pages::guilds_list::guilds_list_page(
        config,
        &auth.0,
        &params,
        result_guilds,
        total,
        has_more,
        is_results_fragment,
    );
    Html(markup.into_string()).into_response()
}

async fn guild_detail(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    Path(guild_id): Path<String>,
    Query(query): Query<GuildDetailQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let is_detail_fragment = htmx::targets(&headers, "main-content");
    let active_tab = normalize_guild_tab(query.tab.as_deref().unwrap_or("overview"));
    let csrf_token = csrf::get_csrf_token(&request);

    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let guild = client
        .lookup_guild(&guild_id)
        .await
        .log_error("load guild detail")
        .flatten();

    let tq = to_tab_query(&query);
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);
    let tab_body = if let Some(guild) = guild.as_ref() {
        guild_tabs::render(
            &client,
            config,
            &csrf_token,
            &guild_id,
            active_tab,
            &tq,
            admin_acls,
        )
        .await
        .or_else(|| {
            Some(templates::pages::guild_detail::simple_tab_content(
                config,
                guild,
                active_tab,
                &csrf_token,
                admin_acls,
            ))
        })
    } else {
        None
    };

    let markup = templates::pages::guild_detail::guild_detail_with_tab(
        config,
        &auth.0,
        guild.as_ref(),
        &guild_id,
        active_tab,
        tab_body,
        is_detail_fragment,
    );
    Html(markup.into_string()).into_response()
}

async fn guild_detail_post(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(guild_id): Path<String>,
    Query(aq): Query<ActionQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let base = &config.base_path;
    let tab = aq.tab.as_deref().unwrap_or("overview");
    let form = match MultiValueForm::from_request(request).await {
        Some(form) => form,
        None => {
            return flash::redirect_with_flash(
                &format!("{base}/guilds/{guild_id}"),
                FlashData::error("Invalid form data"),
                config.secure_cookies(),
            );
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let action = aq.action.as_deref().unwrap_or("");
    let flash = dispatch_guild_action(&client, &guild_id, action, &form).await;
    let redirect = if tab == "overview" {
        format!("{base}/guilds/{guild_id}")
    } else {
        format!("{base}/guilds/{guild_id}?tab={tab}")
    };
    flash::redirect_with_flash(&redirect, flash, config.secure_cookies())
}

async fn dispatch_guild_action(
    client: &AdminApiClient,
    guild_id: &str,
    action: &str,
    form: &MultiValueForm,
) -> FlashData {
    let get = |k: &str| form.clean(k);
    match action {
        "update_features" => {
            let (add_features, remove_features) = if form.contains_key("features[]")
                || form.contains_key("features")
                || form.contains_key("custom_features")
            {
                let desired = unique_strings(
                    form.list_values_any(&["features[]", "features"])
                        .into_iter()
                        .chain(form.list_values("custom_features"))
                        .collect(),
                );
                let current = match client.lookup_guild(guild_id).await {
                    Ok(Some(guild)) => guild.features,
                    Ok(None) => {
                        return FlashData::error("Guild not found");
                    }
                    Err(error) => {
                        tracing::warn!(%error, guild_id, "admin API request failed: load guild features");
                        return FlashData::error("Failed to load current guild features");
                    }
                };
                let desired_set: BTreeSet<&str> = desired.iter().map(String::as_str).collect();
                let current_set: BTreeSet<&str> = current.iter().map(String::as_str).collect();
                let add_features = desired
                    .iter()
                    .filter(|feature| !current_set.contains(feature.as_str()))
                    .cloned()
                    .collect();
                let remove_features = current
                    .iter()
                    .filter(|feature| !desired_set.contains(feature.as_str()))
                    .cloned()
                    .collect();
                (add_features, remove_features)
            } else {
                (
                    form.list_values_any(&["add_features[]", "add_features"]),
                    form.list_values_any(&["remove_features[]", "remove_features"]),
                )
            };
            action_result(
                client
                    .update_guild_features(guild_id, &add_features, &remove_features)
                    .await,
                "Guild features updated",
                "Failed to update guild features",
            )
        }
        "clear_fields" => {
            let fields = form.list_values_any(&["fields[]", "fields"]);
            action_result(
                client.clear_guild_fields(guild_id, &fields).await,
                "Guild fields cleared",
                "Failed to clear guild fields",
            )
        }
        "update_name" => {
            let Some(name) = get("name") else {
                return FlashData::error("Guild name is required");
            };
            action_result(
                client.update_guild_name(guild_id, &name).await,
                "Guild name updated",
                "Failed to update guild name",
            )
        }
        "update_vanity" => {
            let vanity = get("vanity_url_code");
            action_result(
                client
                    .update_guild_vanity(guild_id, vanity.as_deref())
                    .await,
                "Guild vanity URL updated",
                "Failed to update guild vanity URL",
            )
        }
        "transfer_ownership" => {
            let Some(new_owner) = get("new_owner_id") else {
                return FlashData::error("New owner user ID is required");
            };
            action_result(
                client.transfer_guild_ownership(guild_id, &new_owner).await,
                "Guild ownership transferred",
                "Failed to transfer guild ownership",
            )
        }
        "reload" => action_result(
            client.reload_guild(guild_id).await,
            "Guild reload requested",
            "Failed to reload guild",
        ),
        "shutdown" => action_result(
            client.shutdown_guild(guild_id).await,
            "Guild shutdown requested",
            "Failed to shut down guild",
        ),
        "delete_guild" => action_result(
            client.delete_guild(guild_id).await,
            "Guild deleted",
            "Failed to delete guild",
        ),
        "update_settings" => {
            let mut settings = serde_json::Map::new();
            for key in &[
                "verification_level",
                "mfa_level",
                "explicit_content_filter",
                "default_message_notifications",
                "disabled_operations",
            ] {
                if let Some(v) = form.parse_i64(key) {
                    settings.insert(key.to_string(), serde_json::json!(v));
                }
            }
            if form.contains_key("nsfw_submitted") {
                settings.insert(
                    "nsfw".to_string(),
                    serde_json::json!(form.bool_value("nsfw")),
                );
            }
            if form.contains_key("content_warning_submitted") {
                let level = if form.bool_value("content_warning_level") {
                    1
                } else {
                    0
                };
                settings.insert(
                    "content_warning_level".to_string(),
                    serde_json::json!(level),
                );
                let text = form.clean("content_warning_text");
                settings.insert("content_warning_text".to_string(), serde_json::json!(text));
            }
            action_result(
                client
                    .update_guild_settings(guild_id, &serde_json::Value::Object(settings))
                    .await,
                "Guild settings updated",
                "Failed to update guild settings",
            )
        }
        "update_disabled_operations" => {
            let disabled_operations = form
                .list_values_any(&["disabled_operations[]", "disabled_operations"])
                .iter()
                .filter_map(|value| value.parse::<i64>().ok())
                .fold(0_i64, |acc, value| acc | value);
            let settings = serde_json::json!({
                "disabled_operations": disabled_operations,
            });
            action_result(
                client.update_guild_settings(guild_id, &settings).await,
                "Guild disabled operations updated",
                "Failed to update disabled operations",
            )
        }
        "force_add_user" => {
            let Some(user_id) = get("user_id") else {
                return FlashData::error("User ID is required");
            };
            action_result(
                client.force_add_user_to_guild(&user_id, guild_id).await,
                "User added to guild",
                "Failed to add user to guild",
            )
        }
        "ban_member" => {
            let Some(user_id) = get("user_id") else {
                return FlashData::error("User ID is required");
            };
            action_result(
                client.ban_guild_member(guild_id, &user_id).await,
                "Guild member banned",
                "Failed to ban guild member",
            )
        }
        "kick_member" => {
            let Some(user_id) = get("user_id") else {
                return FlashData::error("User ID is required");
            };
            action_result(
                client.kick_guild_member(guild_id, &user_id).await,
                "Guild member kicked",
                "Failed to kick guild member",
            )
        }
        "refresh_search_index" => {
            let index_type = get("index_type").unwrap_or_default();
            action_result(
                client
                    .refresh_search_index(&index_type, Some(guild_id))
                    .await,
                "Search index refresh started",
                "Failed to refresh search index",
            )
        }
        "delete_emoji" => {
            let Some(emoji_id) = get("emoji_id") else {
                return FlashData::error("Emoji ID is required");
            };
            action_result(
                client.purge_assets(&[emoji_id]).await,
                "Emoji deleted",
                "Failed to delete emoji",
            )
        }
        "delete_sticker" => {
            let Some(sticker_id) = get("sticker_id") else {
                return FlashData::error("Sticker ID is required");
            };
            action_result(
                client.purge_assets(&[sticker_id]).await,
                "Sticker deleted",
                "Failed to delete sticker",
            )
        }
        "trigger_archive" => {
            let inc = form.bool_value("include_attachments");
            action_result(
                client.trigger_guild_archive(guild_id, inc).await,
                "Guild archive triggered",
                "Failed to trigger guild archive",
            )
        }
        _ => FlashData::error("Unknown guild action"),
    }
}

fn action_result<T, E: std::fmt::Display>(
    result: Result<T, E>,
    success_message: &'static str,
    error_message: &'static str,
) -> FlashData {
    match result {
        Ok(_) => FlashData::success(success_message),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: guild action");
            FlashData::error(error_message)
        }
    }
}

fn unique_strings(values: Vec<String>) -> Vec<String> {
    let mut seen = BTreeSet::new();
    values
        .into_iter()
        .filter(|value| seen.insert(value.clone()))
        .collect()
}

async fn guild_tab(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path((guild_id, tab)): Path<(String, String)>,
    Query(query): Query<GuildDetailQuery>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let admin_acls = auth
        .0
        .admin_user
        .as_ref()
        .map(|user| user.acls.as_slice())
        .unwrap_or(&[]);

    let tq = to_tab_query(&query);
    if let Some(markup) = guild_tabs::render(
        &client,
        config,
        &csrf_token,
        &guild_id,
        normalize_guild_tab(&tab),
        &tq,
        admin_acls,
    )
    .await
    {
        return Html(markup.into_string()).into_response();
    }

    let guild = client
        .lookup_guild(&guild_id)
        .await
        .log_error("load guild tab fallback")
        .flatten();
    let markup = match guild {
        Some(ref g) => templates::pages::guild_detail::simple_tab_content(
            config,
            g,
            normalize_guild_tab(&tab),
            &csrf_token,
            admin_acls,
        ),
        None => maud::html! {
            div class="p-4 text-red-600 text-sm" {
                "Failed to load guild data."
            }
        },
    };
    Html(markup.into_string()).into_response()
}

fn to_tab_query(q: &GuildDetailQuery) -> guild_tabs::GuildTabQuery {
    guild_tabs::GuildTabQuery {
        members_page: q.page,
        reports_page: q.reports_page,
        audit_before: q.before.clone(),
    }
}

fn normalize_guild_tab(tab: &str) -> &str {
    match tab {
        "audit-log" => "audit_log",
        "audit-logs" => "audit_logs",
        other => other,
    }
}
