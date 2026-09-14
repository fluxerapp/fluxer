// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    api::client::AdminApiClient,
    middleware::{auth::AuthContext, csrf, htmx},
    state::AppState,
    templates,
};
use axum::{
    Form, Router,
    extract::{FromRequest, Query, Request, State},
    http::HeaderMap,
    response::{Html, IntoResponse, Response},
    routing::get,
};

use super::ActionQuery;
use super::bans_actions::{
    BanFormData, custom_flash, execute_ban, extract_value, flash_response, render_inline_flash,
};

pub fn router() -> Router<AppState> {
    Router::new()
        .route("/ip-bans", get(ip_bans).post(ip_bans_post))
        .route("/email-bans", get(email_bans).post(email_bans_post))
        .route(
            "/suspicious-email-domains",
            get(suspicious_email_domains).post(suspicious_email_domains_post),
        )
        .route("/phrase-bans", get(phrase_bans).post(phrase_bans_post))
        .route("/url-bans", get(url_bans).post(url_bans_post))
        .route(
            "/url-domain-bans",
            get(url_domain_bans).post(url_domain_bans_post),
        )
        .route(
            "/file-sha-bans",
            get(file_sha_bans).post(file_sha_bans_post),
        )
        .route(
            "/avatar-hash-bans",
            get(avatar_hash_bans).post(avatar_hash_bans_post),
        )
        .route(
            "/profile-substring-bans",
            get(profile_substring_bans).post(profile_substring_bans_post),
        )
}

fn render_ban_page(state: &AppState, auth: &AuthContext, key: &str, req: &Request) -> Response {
    let config = state.config();
    let ban_cfg = match templates::pages::bans::get_ban_config(key) {
        Some(c) => c,
        None => return axum::http::StatusCode::NOT_FOUND.into_response(),
    };
    let csrf_token = csrf::get_csrf_token(req);
    let markup = templates::pages::bans::bans_page(config, auth, ban_cfg, None, &csrf_token);
    Html(markup.into_string()).into_response()
}

macro_rules! ban_get {
    ($name:ident, $key:expr) => {
        async fn $name(
            State(state): State<AppState>,
            auth: axum::Extension<AuthContext>,
            request: Request,
        ) -> Response {
            render_ban_page(&state, &auth.0, $key, &request)
        }
    };
}

ban_get!(ip_bans, "ip-bans");
ban_get!(email_bans, "email-bans");
ban_get!(suspicious_email_domains, "suspicious-email-domains");
ban_get!(phrase_bans, "phrase-bans");
ban_get!(url_bans, "url-bans");
ban_get!(file_sha_bans, "file-sha-bans");
ban_get!(avatar_hash_bans, "avatar-hash-bans");

async fn generic_ban_post(
    state: &AppState,
    auth: &AuthContext,
    headers: &HeaderMap,
    ban_key: &str,
    action: &str,
    form: &BanFormData,
    csrf_token: &str,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.session);
    let ban_cfg = match templates::pages::bans::get_ban_config(ban_key) {
        Some(c) => c,
        None => return axum::http::StatusCode::NOT_FOUND.into_response(),
    };
    let value = extract_value(form, ban_cfg.input_name);
    let is_htmx = htmx::is_htmx_request(headers);
    let (level, msg) = execute_ban(
        &client,
        ban_key,
        action,
        &value,
        form.hashes.as_deref(),
        form.sha256_list.as_deref(),
        form.audit_log_reason.as_deref(),
    )
    .await;
    flash_response(config, auth, is_htmx, level, &msg, ban_cfg, csrf_token)
}

macro_rules! ban_post {
    ($name:ident, $key:expr) => {
        async fn $name(
            State(state): State<AppState>,
            headers: HeaderMap,
            auth: axum::Extension<AuthContext>,
            request: Request,
        ) -> Response {
            let csrf_token = csrf::get_csrf_token(&request);
            let Query(aq): Query<ActionQuery> =
                Query::try_from_uri(request.uri()).unwrap_or(Query(ActionQuery { action: None }));
            let action = aq.action.as_deref().unwrap_or("");
            let form: BanFormData = match Form::from_request(request, &state).await {
                Ok(Form(f)) => f,
                Err(_) => {
                    return flash_response(
                        state.config(),
                        &auth.0,
                        htmx::is_htmx_request(&headers),
                        "error",
                        "Invalid form data",
                        templates::pages::bans::get_ban_config($key).unwrap(),
                        &csrf_token,
                    );
                }
            };
            generic_ban_post(&state, &auth.0, &headers, $key, action, &form, &csrf_token).await
        }
    };
}

ban_post!(ip_bans_post, "ip-bans");
ban_post!(email_bans_post, "email-bans");
ban_post!(suspicious_email_domains_post, "suspicious-email-domains");
ban_post!(phrase_bans_post, "phrase-bans");
ban_post!(url_bans_post, "url-bans");
ban_post!(file_sha_bans_post, "file-sha-bans");
ban_post!(avatar_hash_bans_post, "avatar-hash-bans");

async fn url_domain_bans(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let markup =
        templates::pages::url_domain_bans::url_domain_bans_page(config, &auth.0, None, &csrf_token);
    Html(markup.into_string()).into_response()
}

async fn url_domain_bans_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let Query(aq): Query<ActionQuery> =
        Query::try_from_uri(request.uri()).unwrap_or(Query(ActionQuery { action: None }));
    let form: BanFormData = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(_) => return render_inline_flash("error", "Invalid form data"),
    };
    let is_htmx = htmx::is_htmx_request(&headers);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let domain = form.domain.as_deref().unwrap_or("").trim().to_owned();
    if domain.is_empty() {
        return render_inline_flash("error", "Domain is required");
    }
    let action = aq.action.as_deref().unwrap_or("");
    let (level, msg) = match action {
        "ban" => {
            let m_sub = form.match_subdomains.as_deref() == Some("true");
            match client
                .ban_url_domain(&domain, m_sub, form.audit_log_reason.as_deref())
                .await
            {
                Ok(()) => ("success", format!("Domain {domain} banned successfully")),
                Err(error) => {
                    tracing::warn!(%error, domain, "admin API request failed: ban URL domain");
                    ("error", format!("Failed to ban domain {domain}"))
                }
            }
        }
        "unban" => match client
            .unban_url_domain(&domain, form.audit_log_reason.as_deref())
            .await
        {
            Ok(()) => ("success", format!("Domain {domain} unbanned")),
            Err(error) => {
                tracing::warn!(%error, domain, "admin API request failed: unban URL domain");
                ("error", format!("Failed to unban domain {domain}"))
            }
        },
        "check" => match client.check_url_domain_ban(&domain).await {
            Ok(r) if r.banned => ("info", format!("Domain {domain} is banned")),
            Ok(_) => ("info", format!("Domain {domain} is NOT banned")),
            Err(error) => {
                tracing::warn!(%error, domain, "admin API request failed: check URL domain ban");
                ("error", "Error checking ban status".into())
            }
        },
        _ => ("error", "Unknown action".into()),
    };
    custom_flash(
        config,
        &auth.0,
        is_htmx,
        level,
        &msg,
        &csrf_token,
        "url-domain",
    )
}

async fn profile_substring_bans(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let markup = templates::pages::profile_substring_bans::profile_substring_bans_page(
        config,
        &auth.0,
        None,
        &csrf_token,
    );
    Html(markup.into_string()).into_response()
}

async fn profile_substring_bans_post(
    State(state): State<AppState>,
    headers: HeaderMap,
    auth: axum::Extension<AuthContext>,
    request: Request,
) -> Response {
    let config = state.config();
    let csrf_token = csrf::get_csrf_token(&request);
    let Query(aq): Query<ActionQuery> =
        Query::try_from_uri(request.uri()).unwrap_or(Query(ActionQuery { action: None }));
    let form: BanFormData = match Form::from_request(request, &state).await {
        Ok(Form(f)) => f,
        Err(_) => return render_inline_flash("error", "Invalid form data"),
    };
    let is_htmx = htmx::is_htmx_request(&headers);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let scope = form.scope.as_deref().unwrap_or("").trim().to_owned();
    let substring = form.substring.as_deref().unwrap_or("").trim().to_owned();
    if scope.is_empty() || substring.is_empty() {
        return render_inline_flash("error", "Scope and substring required");
    }
    let action = aq.action.as_deref().unwrap_or("");
    let (level, msg) = match action {
        "ban" => match client
            .ban_profile_substring(&scope, &substring, form.audit_log_reason.as_deref())
            .await
        {
            Ok(()) => ("success", format!("\"{substring}\" banned for {scope}")),
            Err(error) => {
                tracing::warn!(%error, scope, substring, "admin API request failed: ban profile substring");
                ("error", format!("Failed to ban substring for {scope}"))
            }
        },
        "unban" => match client
            .unban_profile_substring(&scope, &substring, form.audit_log_reason.as_deref())
            .await
        {
            Ok(()) => ("success", format!("\"{substring}\" unbanned for {scope}")),
            Err(error) => {
                tracing::warn!(%error, scope, substring, "admin API request failed: unban profile substring");
                ("error", format!("Failed to unban substring for {scope}"))
            }
        },
        "check" => match client.check_profile_substring_ban(&scope, &substring).await {
            Ok(r) if r.banned => ("info", format!("\"{substring}\" IS banned for {scope}")),
            Ok(_) => ("info", format!("\"{substring}\" is NOT banned for {scope}")),
            Err(error) => {
                tracing::warn!(%error, scope, substring, "admin API request failed: check profile substring ban");
                ("error", "Error checking ban status".into())
            }
        },
        _ => ("error", "Unknown action".into()),
    };
    custom_flash(
        config,
        &auth.0,
        is_htmx,
        level,
        &msg,
        &csrf_token,
        "profile-substring",
    )
}
