// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    acl,
    api::{
        client::{AdminApiClient, ApiResultExt},
        generated::types::OperatorPassWallDeclineReasonSchema,
        operator_pass::{PassWithdrawal, PerkRemoval, WallDecision},
    },
    middleware::{
        auth::AuthContext,
        csrf::CsrfToken,
        flash::{self, FlashData},
    },
    state::AppState,
    templates::{
        self,
        pages::operator_pass_wall::{
            BUSINESS_CHECKS, BUSINESS_LOGO_MAX_BYTES, BUSINESS_NAME_MAX_CHARS, DELIVERIES_CSV_PATH,
            REISSUE_REASONS, RELEASES_PATH, TOOLS_PATH, VISIONARY_CSV_PATH, WALL_PATH,
            WITHDRAWAL_CHANNELS,
        },
    },
    utils::snowflake::is_snowflake,
};
use axum::{
    Form, Router,
    extract::{DefaultBodyLimit, FromRequest, Multipart, Path, Query, Request, State},
    http::{StatusCode, header},
    response::{Html, IntoResponse, Response},
    routing::{get, post},
};
use base64::{Engine as _, engine::general_purpose::STANDARD};
use chrono::{DateTime, NaiveDate, Utc};
use serde::Deserialize;

#[derive(Deserialize)]
struct DeliveriesQuery {
    #[serde(default)]
    month: String,
}

#[derive(Deserialize)]
struct WallActionForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    back: String,
    #[serde(default)]
    revision_id: String,
    #[serde(default)]
    reason: String,
    #[serde(default)]
    note: String,
    #[serde(default)]
    rule: String,
    #[serde(default)]
    facts: String,
    #[serde(default)]
    serious: Option<String>,
    #[serde(default)]
    duration_days: String,
    #[serde(default)]
    purchase_id: String,
    #[serde(default)]
    name: String,
    #[serde(default)]
    received_at: String,
    #[serde(default)]
    channel: String,
    #[serde(default)]
    pass_index: String,
    #[serde(default)]
    holder_name: String,
    #[serde(default)]
    reissue_reason: String,
}

enum WallAction {
    Revert,
    ForceFallback,
    Hide,
    Show,
    LegalHold,
    LegalRelease,
    RemovalNotice,
    Remove,
    Restore,
    Withdraw,
    Reissue,
}

impl TryFrom<&str> for WallAction {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "revert" => Ok(Self::Revert),
            "force-fallback" => Ok(Self::ForceFallback),
            "hide" => Ok(Self::Hide),
            "show" => Ok(Self::Show),
            "legal-hold" => Ok(Self::LegalHold),
            "legal-release" => Ok(Self::LegalRelease),
            "removal-notice" => Ok(Self::RemovalNotice),
            "remove" => Ok(Self::Remove),
            "restore" => Ok(Self::Restore),
            "withdraw" => Ok(Self::Withdraw),
            "reissue" => Ok(Self::Reissue),
            _ => Err(()),
        }
    }
}

fn parse_removal_days(value: &str) -> Result<Option<u32>, ()> {
    let value = value.trim();
    if value.is_empty() {
        return Ok(None);
    }
    match value.parse::<u32>() {
        Ok(days) if (1..=3650).contains(&days) => Ok(Some(days)),
        _ => Err(()),
    }
}

#[derive(Deserialize)]
struct CsrfOnlyForm {
    #[serde(default)]
    _csrf: Option<String>,
}

#[derive(Deserialize)]
struct TimeTravelQuery {
    #[serde(default)]
    at: String,
}

#[derive(Deserialize)]
struct UserLookupQuery {
    #[serde(default)]
    user_id: String,
}

#[derive(Deserialize)]
struct YearQuery {
    year: Option<i64>,
}

#[derive(Deserialize)]
struct PublishForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    tag: String,
    #[serde(default)]
    released_on: String,
}

#[derive(Deserialize)]
struct EraseForm {
    #[serde(default)]
    _csrf: Option<String>,
    #[serde(default)]
    user_id: String,
}

pub fn router() -> Router<AppState> {
    Router::new()
        .route(VISIONARY_CSV_PATH, get(operator_pass_visionary_csv))
        .route(DELIVERIES_CSV_PATH, get(operator_pass_deliveries_csv))
        .route(WALL_PATH, get(operator_pass_wall_page))
        .route("/operator-pass-wall/users", get(operator_pass_wall_lookup))
        .route(
            "/operator-pass-wall/users/{user_id}",
            get(operator_pass_wall_user_page),
        )
        .route(
            "/operator-pass-wall/users/{user_id}/{action}",
            post(operator_pass_wall_action),
        )
        .route(
            "/operator-pass-wall/users/{user_id}/business",
            post(operator_pass_wall_business)
                .layer(DefaultBodyLimit::max(BUSINESS_LOGO_MAX_BYTES + 64 * 1024)),
        )
        .route(
            "/operator-pass-wall/avatars/{sha256}",
            get(operator_pass_wall_avatar),
        )
        .route(TOOLS_PATH, get(operator_pass_wall_tools_page))
        .route(
            "/operator-pass-wall/tools/{action}",
            post(operator_pass_wall_tools_action),
        )
        .route(RELEASES_PATH, get(operator_pass_releases_page))
        .route(
            "/operator-pass-releases/{year}/publish",
            post(operator_pass_release_publish),
        )
        .route(
            "/operator-pass-releases/{year}/erase",
            post(operator_pass_release_erase),
        )
}

fn can_look_up(auth: &AuthContext) -> bool {
    auth.admin_user
        .as_ref()
        .is_some_and(|admin| acl::has_permission(&admin.acls, acl::USER_LOOKUP))
}

fn can_moderate(auth: &AuthContext) -> bool {
    auth.admin_user
        .as_ref()
        .is_some_and(|admin| acl::has_permission(&admin.acls, acl::USER_TEMP_BAN))
}

fn iso(at: &DateTime<Utc>) -> String {
    serde_json::to_value(at)
        .ok()
        .and_then(|value| value.as_str().map(str::to_owned))
        .unwrap_or_default()
}

fn current_year() -> i64 {
    let seconds = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|duration| duration.as_secs() as i64)
        .unwrap_or(0);
    DateTime::<Utc>::from_timestamp(seconds, 0)
        .map(|at| i64::from(chrono::Datelike::year(&at)))
        .unwrap_or(2026)
}

fn parse_release_date(value: &str) -> Option<DateTime<Utc>> {
    NaiveDate::parse_from_str(value.trim(), "%Y-%m-%d")
        .ok()
        .and_then(|date| date.and_hms_opt(12, 0, 0))
        .map(|at| at.and_utc())
}

fn is_purchase_id(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 256
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
}

const MAX_PASSES_PER_PURCHASE: u32 = 100;

fn parse_reissue_pass_id(pass_index: &str, purchase_id: &str) -> Option<String> {
    pass_index
        .trim()
        .parse::<u32>()
        .ok()
        .filter(|index| {
            (1..=MAX_PASSES_PER_PURCHASE).contains(index) && is_purchase_id(purchase_id)
        })
        .map(|index| format!("{purchase_id}:{index}"))
}

fn is_sha256(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn parse_time_travel(value: &str) -> Option<DateTime<Utc>> {
    let value = value.trim();
    DateTime::parse_from_rfc3339(value)
        .map(|at| at.with_timezone(&Utc))
        .ok()
        .or_else(|| {
            chrono::NaiveDateTime::parse_from_str(value, "%Y-%m-%dT%H:%M")
                .ok()
                .map(|at| at.and_utc())
        })
}

async fn operator_pass_visionary_csv(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let Some((_, body)) = client
        .get_operator_pass_visionary_activations_csv()
        .await
        .log_error("load the Visionary Operator Pass turn-ons")
    else {
        return (
            StatusCode::BAD_GATEWAY,
            "Could not load the Visionary turn-ons.",
        )
            .into_response();
    };
    (
        [
            (header::CONTENT_TYPE, "text/csv; charset=utf-8"),
            (
                header::CONTENT_DISPOSITION,
                "attachment; filename=\"operator-pass-visionary-activations.csv\"",
            ),
            (header::CACHE_CONTROL, "private, no-store"),
        ],
        body,
    )
        .into_response()
}

fn is_delivery_month(month: &str) -> bool {
    let bytes = month.as_bytes();
    bytes.len() == 7
        && bytes[4] == b'-'
        && bytes
            .iter()
            .enumerate()
            .all(|(index, byte)| index == 4 || byte.is_ascii_digit())
        && matches!(month[5..].parse::<u8>(), Ok(1..=12))
}

async fn operator_pass_deliveries_csv(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Query(query): Query<DeliveriesQuery>,
) -> Response {
    let month = query.month.trim();
    if !is_delivery_month(month) {
        return (StatusCode::BAD_REQUEST, "Pick a month as YYYY-MM.").into_response();
    }
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let Some((_, body)) = client
        .get_operator_pass_deliveries_csv(month)
        .await
        .log_error("load the Operator Pass deliveries")
    else {
        return (
            StatusCode::BAD_GATEWAY,
            "Could not load the deliveries. The API request failed or this account lacks the user:lookup ACL.",
        )
            .into_response();
    };
    (
        [
            (header::CONTENT_TYPE, "text/csv; charset=utf-8".to_owned()),
            (
                header::CONTENT_DISPOSITION,
                format!("attachment; filename=\"operator-pass-leveranser-{month}.csv\""),
            ),
            (header::CACHE_CONTROL, "private, no-store".to_owned()),
        ],
        body,
    )
        .into_response()
}

async fn operator_pass_wall_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    flash: Option<axum::Extension<FlashData>>,
) -> Response {
    let config = state.config();
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::operator_pass_wall::operator_pass_wall_page(
        config,
        &auth.0,
        flash.as_ref(),
        can_look_up(&auth.0),
    );
    Html(markup.into_string()).into_response()
}

async fn operator_pass_wall_lookup(
    State(state): State<AppState>,
    Query(query): Query<UserLookupQuery>,
) -> Response {
    let config = state.config();
    let user_id = query.user_id.trim();
    if !is_snowflake(user_id) {
        return flash::redirect_error(
            config,
            &format!("{}{WALL_PATH}", config.base_path),
            "Enter a numeric user ID",
        );
    }
    axum::response::Redirect::to(&format!("{}{WALL_PATH}/users/{user_id}", config.base_path))
        .into_response()
}

async fn operator_pass_wall_user_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    flash: Option<axum::Extension<FlashData>>,
    Path(user_id): Path<String>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let (data, timeline) = if is_snowflake(&user_id) {
        (
            client
                .get_user_operator_pass(&user_id)
                .await
                .log_error("load user Operator Pass"),
            client
                .get_operator_pass_wall_timeline(&user_id)
                .await
                .log_error("load Operators' wall timeline"),
        )
    } else {
        (None, None)
    };
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::operator_pass_wall::operator_pass_wall_user_page(
        config,
        &auth.0,
        &csrf.0.0,
        flash.as_ref(),
        &user_id,
        data.as_ref(),
        timeline.as_ref(),
        can_moderate(&auth.0),
    );
    Html(markup.into_string()).into_response()
}

async fn operator_pass_wall_action(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path((user_id, action)): Path<(String, String)>,
    request: Request,
) -> Response {
    let config = state.config();
    let queue = format!("{}{WALL_PATH}", config.base_path);
    let form: WallActionForm = match Form::from_request(request, &state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "failed to parse Operators' wall form");
            return flash::redirect_error(config, &queue, "Invalid form data");
        }
    };
    let back = if form.back == "user" && is_snowflake(&user_id) {
        format!("{queue}/users/{user_id}")
    } else {
        queue
    };
    if !is_snowflake(&user_id) {
        return flash::redirect_error(config, &back, "Invalid user ID");
    }
    let note = form.note.trim();
    let revision_id = form.revision_id.trim();
    let decision = form
        .reason
        .parse::<OperatorPassWallDeclineReasonSchema>()
        .ok()
        .filter(|_| !note.is_empty())
        .map(|reason| WallDecision { reason, note });
    let rule = form.rule.trim();
    let facts = form.facts.trim();
    let removal_ready = !rule.is_empty() && !facts.is_empty();
    let removal_days = parse_removal_days(&form.duration_days);
    let purchase_id = form.purchase_id.trim();
    let buyer_name = form.name.trim();
    let withdrawal = parse_time_travel(&form.received_at)
        .filter(|_| {
            is_purchase_id(purchase_id)
                && !buyer_name.is_empty()
                && WITHDRAWAL_CHANNELS
                    .iter()
                    .any(|(channel, _)| *channel == form.channel)
        })
        .map(|received_at| PassWithdrawal {
            name: buyer_name,
            received_at,
            channel: &form.channel,
        });
    let holder_name = form.holder_name.trim();
    let reissue_pass_id = parse_reissue_pass_id(&form.pass_index, purchase_id).filter(|_| {
        !holder_name.is_empty()
            && REISSUE_REASONS
                .iter()
                .any(|(reason, _)| *reason == form.reissue_reason)
    });
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = match WallAction::try_from(action.as_str()) {
        Ok(WallAction::Revert) if is_snowflake(revision_id) => client
            .revert_operator_pass_wall_revision(&user_id, revision_id)
            .await
            .map(|_| "Put back on the wall".to_owned()),
        Ok(WallAction::ForceFallback) if let Some(decision) = &decision => client
            .force_operator_pass_wall_fallback(&user_id, decision)
            .await
            .map(|_| {
                "The wall shows the fallback and the statement of reasons was sent".to_owned()
            }),
        Ok(WallAction::Hide) if decision.is_some() => client
            .set_operator_pass_wall_hidden(&user_id, true, Some(&form.reason), Some(note))
            .await
            .map(|_| "Hidden from the wall and the statement of reasons was sent".to_owned()),
        Ok(WallAction::Show) => client
            .set_operator_pass_wall_hidden(&user_id, false, None, None)
            .await
            .map(|_| "Put back on the wall".to_owned()),
        Ok(WallAction::LegalHold) => client
            .set_operator_pass_wall_legal_hold(&user_id, true)
            .await
            .map(|_| "Legal hold placed".to_owned()),
        Ok(WallAction::LegalRelease) => client
            .set_operator_pass_wall_legal_hold(&user_id, false)
            .await
            .map(|_| "Legal hold released".to_owned()),
        Ok(WallAction::RemovalNotice) if removal_ready => client
            .send_operator_pass_removal_notice(&user_id, rule, facts)
            .await
            .map(|_| "Sent the 7 day notice before a permanent removal".to_owned()),
        Ok(WallAction::Remove) if removal_ready && removal_days.is_ok() => client
            .remove_operator_pass_perks(
                &user_id,
                &PerkRemoval {
                    rule,
                    facts,
                    serious: form.serious.is_some(),
                    duration_days: removal_days.unwrap_or(None),
                },
            )
            .await
            .map(|_| "Removed the bonus and sent the statement of reasons".to_owned()),
        Ok(WallAction::Restore) => client
            .restore_operator_pass_perks(&user_id)
            .await
            .map(|()| "Restored the bonus".to_owned()),
        Ok(WallAction::Withdraw) if let Some(withdrawal) = &withdrawal => client
            .withdraw_operator_pass_purchase(purchase_id, withdrawal)
            .await
            .map(|_| "Recorded the withdrawal and sent the receipt".to_owned()),
        Ok(WallAction::Reissue) if let Some(pass_id) = &reissue_pass_id => client
            .reissue_operator_pass_certificate(pass_id, holder_name, &form.reissue_reason)
            .await
            .map(|()| "Issued a new copy of the certificate page".to_owned()),
        Ok(WallAction::Revert)
        | Ok(WallAction::ForceFallback)
        | Ok(WallAction::Hide)
        | Ok(WallAction::RemovalNotice)
        | Ok(WallAction::Remove)
        | Ok(WallAction::Withdraw)
        | Ok(WallAction::Reissue) => {
            return flash::redirect_error(config, &back, "Fill in every required field");
        }
        Err(()) => {
            return flash::redirect_error(config, &back, "Unknown action");
        }
    };
    let flash = match result {
        Ok(message) => FlashData::success(message),
        Err(error) => {
            tracing::warn!(%error, user_id, action, "admin API request failed: Operators' wall action");
            FlashData::error(error.refusal_message("The action failed"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

struct BusinessListingForm {
    name: String,
    logo: Vec<u8>,
}

async fn read_business_listing_form(
    mut multipart: Multipart,
) -> Result<BusinessListingForm, &'static str> {
    let mut name = String::new();
    let mut logo = Vec::new();
    let mut checked = Vec::new();
    while let Some(field) = multipart
        .next_field()
        .await
        .map_err(|_| "Invalid form data")?
    {
        let field_name = field.name().unwrap_or_default().to_owned();
        match field_name.as_str() {
            "name" => {
                name = field
                    .text()
                    .await
                    .map_err(|_| "Invalid form data")?
                    .trim()
                    .to_owned();
            }
            "logo" => {
                logo = field
                    .bytes()
                    .await
                    .map_err(|_| "The logo must be under 2 MB")?
                    .to_vec();
            }
            check if BUSINESS_CHECKS.iter().any(|(key, _)| *key == check) => {
                checked.push(field_name);
            }
            _ => {}
        }
    }
    if name.is_empty() || name.chars().count() > BUSINESS_NAME_MAX_CHARS || logo.is_empty() {
        return Err("Fill in the company name and choose a logo");
    }
    if BUSINESS_CHECKS
        .iter()
        .any(|(key, _)| !checked.iter().any(|done| done == key))
    {
        return Err("Tick every check before listing the business");
    }
    Ok(BusinessListingForm { name, logo })
}

async fn operator_pass_wall_business(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(user_id): Path<String>,
    multipart: Multipart,
) -> Response {
    let config = state.config();
    if !is_snowflake(&user_id) {
        return flash::redirect_error(
            config,
            &format!("{}{WALL_PATH}", config.base_path),
            "Invalid user ID",
        );
    }
    let back = format!("{}{WALL_PATH}/users/{user_id}", config.base_path);
    let form = match read_business_listing_form(multipart).await {
        Ok(form) => form,
        Err(message) => {
            return flash::redirect_error(config, &back, message);
        }
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let flash = match client
        .set_operator_pass_wall_business_listing(&user_id, &form.name, &STANDARD.encode(&form.logo))
        .await
    {
        Ok(_) => FlashData::success("The business listing is on the wall"),
        Err(error) => {
            tracing::warn!(%error, user_id, "admin API request failed: Operators' wall business listing");
            FlashData::error(error.detail_message().map_or_else(
                || "The business listing failed".to_owned(),
                |detail| format!("The business listing failed: {detail}"),
            ))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn operator_pass_wall_avatar(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(sha256): Path<String>,
) -> Response {
    if !can_look_up(&auth.0) {
        return (StatusCode::FORBIDDEN, "Forbidden").into_response();
    }
    if !is_sha256(&sha256) {
        return (StatusCode::NOT_FOUND, "Not found").into_response();
    }
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client.get_operator_pass_wall_avatar(&sha256).await {
        Ok((content_type, body)) => (
            [
                (header::CONTENT_TYPE, content_type),
                (header::CACHE_CONTROL, "private, no-store".to_owned()),
            ],
            body,
        )
            .into_response(),
        Err(error) => {
            tracing::warn!(%error, "admin API request failed: Operators' wall avatar");
            (StatusCode::NOT_FOUND, "Not found").into_response()
        }
    }
}

async fn operator_pass_wall_tools_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    flash: Option<axum::Extension<FlashData>>,
    Query(query): Query<TimeTravelQuery>,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let wall_state = client
        .get_operator_pass_wall_state()
        .await
        .log_error("load Operators' wall state");
    let at = parse_time_travel(&query.at);
    let view = match at {
        Some(at) => client
            .get_operator_pass_wall_as_of(&iso(&at))
            .await
            .log_error("rebuild Operators' wall at a time"),
        None => None,
    };
    let at_value = at.map(|at| iso(&at).chars().take(16).collect::<String>());
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::operator_pass_wall::operator_pass_wall_tools_page(
        config,
        &auth.0,
        &csrf.0.0,
        flash.as_ref(),
        wall_state.as_ref(),
        at_value.as_deref(),
        view.as_ref(),
        can_moderate(&auth.0),
    );
    Html(markup.into_string()).into_response()
}

async fn operator_pass_wall_tools_action(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    Path(action): Path<String>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}{TOOLS_PATH}", config.base_path);
    if Form::<CsrfOnlyForm>::from_request(request, &state)
        .await
        .is_err()
    {
        return flash::redirect_error(config, &back, "Invalid form data");
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let result = match action.as_str() {
        "freeze" => client
            .set_operator_pass_wall_frozen(true)
            .await
            .map(|_| "Publishing is frozen, the current snapshot keeps serving".to_owned()),
        "unfreeze" => client
            .set_operator_pass_wall_frozen(false)
            .await
            .map(|state| format!("Publishing again at version {}", state.version)),
        "republish" => match client.republish_operator_pass_wall().await {
            Ok(state) if state.frozen => {
                return flash::redirect_error(
                    config,
                    &back,
                    "Publishing is frozen, so nothing was published. Unfreeze to publish again.",
                );
            }
            result => result.map(|state| format!("Re-published as version {}", state.version)),
        },
        _ => {
            return flash::redirect_error(config, &back, "Unknown action");
        }
    };
    let flash = match result {
        Ok(message) => FlashData::success(message),
        Err(error) => {
            tracing::warn!(%error, action, "admin API request failed: Operators' wall tools");
            FlashData::error(error.refusal_message("The action failed"))
        }
    };
    flash::redirect_with_flash(&back, flash, config.secure_cookies())
}

async fn operator_pass_releases_page(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    flash: Option<axum::Extension<FlashData>>,
    Query(query): Query<YearQuery>,
) -> Response {
    let config = state.config();
    let year = query
        .year
        .filter(|year| (2026..=9999).contains(year))
        .unwrap_or_else(current_year);
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    let preview = client
        .get_operator_pass_release(year)
        .await
        .log_error("load Operators release list");
    let flash = flash.map(|flash| flash.0.to_flash_message());
    let markup = templates::pages::operator_pass_wall::operator_pass_releases_page(
        config,
        &auth.0,
        &csrf.0.0,
        flash.as_ref(),
        year,
        preview.as_ref(),
        None,
        can_moderate(&auth.0),
    );
    Html(markup.into_string()).into_response()
}

async fn render_release_result(
    state: &AppState,
    auth: &AuthContext,
    csrf_token: &str,
    year: i64,
    result: &crate::api::generated::types::AdminOperatorPassReleaseFileResponse,
    message: &str,
) -> Response {
    let config = state.config();
    let client = AdminApiClient::new(state.http_client(), config, &auth.session);
    let preview = client
        .get_operator_pass_release(year)
        .await
        .log_error("load Operators release list");
    let flash = FlashData::success(message).to_flash_message();
    let markup = templates::pages::operator_pass_wall::operator_pass_releases_page(
        config,
        auth,
        csrf_token,
        Some(&flash),
        year,
        preview.as_ref(),
        Some(result),
        can_moderate(auth),
    );
    Html(markup.into_string()).into_response()
}

async fn operator_pass_release_publish(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Path(year): Path<i64>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}{RELEASES_PATH}?year={year}", config.base_path);
    let form: PublishForm = match Form::from_request(request, &state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "failed to parse release publish form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let tag = form.tag.trim();
    let Some(released_at) = parse_release_date(&form.released_on).filter(|_| !tag.is_empty())
    else {
        return flash::redirect_error(config, &back, "Enter the release tag and the release date");
    };
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client
        .publish_operator_pass_release(year, tag, released_at)
        .await
    {
        Ok(result) => {
            let message = format!("Published The Operators of {year} in {tag}");
            render_release_result(&state, &auth.0, &csrf.0.0, year, &result, &message).await
        }
        Err(error) => {
            tracing::warn!(%error, year, "admin API request failed: publish release list");
            flash::redirect_error(config, &back, error.refusal_message("Publishing failed"))
        }
    }
}

async fn operator_pass_release_erase(
    State(state): State<AppState>,
    auth: axum::Extension<AuthContext>,
    csrf: axum::Extension<CsrfToken>,
    Path(year): Path<i64>,
    request: Request,
) -> Response {
    let config = state.config();
    let back = format!("{}{RELEASES_PATH}?year={year}", config.base_path);
    let form: EraseForm = match Form::from_request(request, &state).await {
        Ok(Form(form)) => form,
        Err(error) => {
            tracing::warn!(%error, "failed to parse release erase form");
            return flash::redirect_error(config, &back, "Invalid form data");
        }
    };
    let user_id = form.user_id.trim();
    if !is_snowflake(user_id) {
        return flash::redirect_error(config, &back, "Enter a numeric user ID");
    }
    let client = AdminApiClient::new(state.http_client(), config, &auth.0.session);
    match client.erase_operator_pass_release_name(year, user_id).await {
        Ok(result) => {
            let message = format!("Erased the name of {user_id} from The Operators of {year}");
            render_release_result(&state, &auth.0, &csrf.0.0, year, &result, &message).await
        }
        Err(error) => {
            tracing::warn!(%error, year, user_id, "admin API request failed: erase release name");
            flash::redirect_error(config, &back, error.refusal_message("Erasing failed"))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::parse_reissue_pass_id;

    #[test]
    fn reissue_accepts_every_page_up_to_the_largest_purchase() {
        assert_eq!(
            parse_reissue_pass_id("26", "cs_test_a1").as_deref(),
            Some("cs_test_a1:26")
        );
        assert_eq!(
            parse_reissue_pass_id("100", "cs_test_a1").as_deref(),
            Some("cs_test_a1:100")
        );
        assert_eq!(parse_reissue_pass_id("0", "cs_test_a1"), None);
        assert_eq!(parse_reissue_pass_id("101", "cs_test_a1"), None);
        assert_eq!(parse_reissue_pass_id("3", "cs test"), None);
    }
}
