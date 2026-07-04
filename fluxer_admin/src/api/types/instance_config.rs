// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceConfigResponse {
    pub sso: SsoConfigResponse,
    pub gateway_rollout: GatewayRolloutConfigResponse,
    #[serde(default)]
    pub registration: InstanceRegistrationResponse,
    #[serde(default)]
    pub self_hosted: bool,
    #[serde(default)]
    pub app_public: AppPublicConfigResponse,
    #[serde(default)]
    pub policy: InstancePolicyResponse,
    #[serde(default)]
    pub integrations: InstanceIntegrationsResponse,
    #[serde(default)]
    pub media: InstanceMediaResponse,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstancePolicyResponse {
    #[serde(default)]
    pub single_community_enabled: bool,
    #[serde(default)]
    pub single_community_locked: bool,
    pub single_community_guild_id: Option<String>,
    #[serde(default)]
    pub direct_messages_disabled: bool,
    #[serde(default)]
    pub direct_messages_locked: bool,
    #[serde(default)]
    pub premium_mode: PremiumMode,
    #[serde(default)]
    pub services: InstanceServicesOverrides,
    #[serde(default)]
    pub services_resolved: InstanceServicesResolved,
    #[serde(default)]
    pub services_available: InstanceServicesAvailable,
}

impl Default for InstancePolicyResponse {
    fn default() -> Self {
        Self {
            single_community_enabled: false,
            single_community_locked: false,
            single_community_guild_id: None,
            direct_messages_disabled: false,
            direct_messages_locked: false,
            premium_mode: PremiumMode::Everyone,
            services: InstanceServicesOverrides::default(),
            services_resolved: InstanceServicesResolved::default(),
            services_available: InstanceServicesAvailable::default(),
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceServicesOverrides {
    pub gif_enabled: Option<bool>,
    pub youtube_enabled: Option<bool>,
    pub bluesky_enabled: Option<bool>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceServicesResolved {
    #[serde(default)]
    pub gif_enabled: bool,
    #[serde(default)]
    pub youtube_enabled: bool,
    #[serde(default)]
    pub bluesky_enabled: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceServicesAvailable {
    #[serde(default)]
    pub gif: bool,
    #[serde(default)]
    pub youtube: bool,
    #[serde(default)]
    pub bluesky: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceIntegrationsResponse {
    #[serde(default)]
    pub gif: InstanceGifIntegrationResponse,
    #[serde(default)]
    pub youtube: InstanceYoutubeIntegrationResponse,
    #[serde(default)]
    pub captcha: InstanceCaptchaIntegrationResponse,
    #[serde(default)]
    pub email: InstanceEmailIntegrationResponse,
    #[serde(default)]
    pub bluesky: InstanceBlueskyIntegrationResponse,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceGifIntegrationResponse {
    pub klipy_api_key_set: bool,
    #[serde(default)]
    pub effective_available: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceYoutubeIntegrationResponse {
    #[serde(default)]
    pub api_key_set: bool,
    #[serde(default)]
    pub effective_available: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceCaptchaIntegrationResponse {
    pub provider: Option<String>,
    #[serde(default)]
    pub effective_provider: String,
    pub hcaptcha_site_key: Option<String>,
    #[serde(default)]
    pub hcaptcha_secret_key_set: bool,
    pub turnstile_site_key: Option<String>,
    #[serde(default)]
    pub turnstile_secret_key_set: bool,
    #[serde(default)]
    pub effective_enabled: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceEmailIntegrationResponse {
    pub enabled: Option<bool>,
    #[serde(default)]
    pub effective_enabled: bool,
    pub provider: Option<String>,
    #[serde(default)]
    pub effective_provider: String,
    pub from_email: Option<String>,
    pub from_name: Option<String>,
    #[serde(default)]
    pub smtp: InstanceEmailSmtpIntegrationResponse,
    #[serde(default)]
    pub disable_new_ip_authorization: bool,
    #[serde(default)]
    pub effective_disable_new_ip_authorization: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceEmailSmtpIntegrationResponse {
    pub host: Option<String>,
    pub port: Option<u16>,
    pub username: Option<String>,
    #[serde(default)]
    pub password_set: bool,
    pub secure: Option<bool>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceBlueskyIntegrationResponse {
    pub enabled: Option<bool>,
    #[serde(default)]
    pub effective_enabled: bool,
    pub client_name: Option<String>,
    pub client_uri: Option<String>,
    pub logo_uri: Option<String>,
    pub tos_uri: Option<String>,
    pub policy_uri: Option<String>,
    #[serde(default)]
    pub key_count: u16,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceMediaResponse {
    #[serde(default)]
    pub attachment_decay: InstanceAttachmentDecayResponse,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceAttachmentDecayResponse {
    pub enabled: Option<bool>,
    pub min_size_mb: Option<f64>,
    pub max_size_mb: Option<f64>,
    pub max_eligible_size_mb: Option<f64>,
    pub min_lifetime_days: Option<u32>,
    pub max_lifetime_days: Option<u32>,
    pub curve: Option<f64>,
    pub renew_threshold_days: Option<u32>,
    pub renew_window_days: Option<u32>,
    #[serde(default)]
    pub effective: InstanceAttachmentDecayEffectiveResponse,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceAttachmentDecayEffectiveResponse {
    #[serde(default)]
    pub enabled: bool,
    #[serde(default = "default_attachment_decay_min_size_mb")]
    pub min_size_mb: f64,
    #[serde(default = "default_attachment_decay_max_size_mb")]
    pub max_size_mb: f64,
    #[serde(default = "default_attachment_decay_max_eligible_size_mb")]
    pub max_eligible_size_mb: f64,
    #[serde(default = "default_attachment_decay_min_lifetime_days")]
    pub min_lifetime_days: u32,
    #[serde(default = "default_attachment_decay_max_lifetime_days")]
    pub max_lifetime_days: u32,
    #[serde(default = "default_attachment_decay_curve")]
    pub curve: f64,
    #[serde(default = "default_attachment_decay_renew_threshold_days")]
    pub renew_threshold_days: u32,
    #[serde(default = "default_attachment_decay_renew_window_days")]
    pub renew_window_days: u32,
}

impl Default for InstanceAttachmentDecayEffectiveResponse {
    fn default() -> Self {
        Self {
            enabled: false,
            min_size_mb: default_attachment_decay_min_size_mb(),
            max_size_mb: default_attachment_decay_max_size_mb(),
            max_eligible_size_mb: default_attachment_decay_max_eligible_size_mb(),
            min_lifetime_days: default_attachment_decay_min_lifetime_days(),
            max_lifetime_days: default_attachment_decay_max_lifetime_days(),
            curve: default_attachment_decay_curve(),
            renew_threshold_days: default_attachment_decay_renew_threshold_days(),
            renew_window_days: default_attachment_decay_renew_window_days(),
        }
    }
}

fn default_attachment_decay_min_size_mb() -> f64 {
    5.0
}

fn default_attachment_decay_max_size_mb() -> f64 {
    500.0
}

fn default_attachment_decay_max_eligible_size_mb() -> f64 {
    500.0
}

fn default_attachment_decay_min_lifetime_days() -> u32 {
    14
}

fn default_attachment_decay_max_lifetime_days() -> u32 {
    365 * 3
}

fn default_attachment_decay_curve() -> f64 {
    0.5
}

fn default_attachment_decay_renew_threshold_days() -> u32 {
    30
}

fn default_attachment_decay_renew_window_days() -> u32 {
    30
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum PremiumMode {
    Mirror,
    #[default]
    Everyone,
}

impl PremiumMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Mirror => "mirror",
            Self::Everyone => "everyone",
        }
    }
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AppPublicConfigResponse {
    #[serde(default)]
    pub branding: AppBrandingConfigResponse,
    #[serde(default)]
    pub setup: AppSetupConfigResponse,
    #[serde(default)]
    pub legal: AppLegalConfigResponse,
    #[serde(default)]
    pub registration: AppRegistrationConfigResponse,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AppBrandingConfigResponse {
    #[serde(default = "default_product_name")]
    pub product_name: String,
    pub icon_url: Option<String>,
    pub symbol_url: Option<String>,
    pub logo_url: Option<String>,
    pub wordmark_url: Option<String>,
    pub favicon_url: Option<String>,
    pub theme_color: Option<String>,
}

impl Default for AppBrandingConfigResponse {
    fn default() -> Self {
        Self {
            product_name: default_product_name(),
            icon_url: None,
            symbol_url: None,
            logo_url: None,
            wordmark_url: None,
            favicon_url: None,
            theme_color: None,
        }
    }
}

fn default_product_name() -> String {
    "Fluxer".to_owned()
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AppSetupConfigResponse {
    #[serde(default)]
    pub configured: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct AppLegalConfigResponse {
    pub terms_url: Option<String>,
    pub privacy_url: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct AppRegistrationConfigResponse {
    #[serde(default = "default_collect_date_of_birth")]
    pub collect_date_of_birth: bool,
}

impl Default for AppRegistrationConfigResponse {
    fn default() -> Self {
        Self {
            collect_date_of_birth: default_collect_date_of_birth(),
        }
    }
}

fn default_collect_date_of_birth() -> bool {
    true
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SsoConfigResponse {
    #[serde(default)]
    pub enabled: bool,
    #[serde(default)]
    pub enforced: bool,
    pub display_name: Option<String>,
    pub issuer: Option<String>,
    pub authorization_url: Option<String>,
    pub token_url: Option<String>,
    pub userinfo_url: Option<String>,
    pub jwks_url: Option<String>,
    pub client_id: Option<String>,
    #[serde(default)]
    pub client_secret_set: bool,
    pub scope: Option<String>,
    #[serde(default)]
    pub allowed_domains: Vec<String>,
    #[serde(default)]
    pub auto_provision: bool,
    pub redirect_uri: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct GatewayRolloutConfigResponse {
    pub session_rollout_percentage: f64,
    pub session_rollout_mode: GatewayRolloutMode,
    pub guild_rollout_percentage: f64,
    pub rpc_request_timeout_ms: u64,
    pub max_concurrent_session_starts: u64,
    pub max_concurrent_guild_starts: u64,
    pub voice_e2ee_scope: VoiceE2eeScope,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum GatewayRolloutMode {
    Modulo,
    Random,
}

impl GatewayRolloutMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Modulo => "modulo",
            Self::Random => "random",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum VoiceE2eeScope {
    GuildFeatureOnly,
    PlatformWide,
}

impl VoiceE2eeScope {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::GuildFeatureOnly => "guild_feature_only",
            Self::PlatformWide => "platform_wide",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceRegistrationResponse {
    pub mode: RegistrationMode,
    #[serde(default = "default_registration_urls_enabled")]
    pub admin_registration_urls_enabled: bool,
    #[serde(default)]
    pub urls: Vec<RegistrationUrlResponse>,
    #[serde(default)]
    pub pending_registrations: Vec<PendingRegistrationResponse>,
}

impl Default for InstanceRegistrationResponse {
    fn default() -> Self {
        Self {
            mode: RegistrationMode::Open,
            admin_registration_urls_enabled: true,
            urls: Vec::new(),
            pending_registrations: Vec::new(),
        }
    }
}

fn default_registration_urls_enabled() -> bool {
    true
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum RegistrationMode {
    Open,
    Approval,
    Closed,
}

impl RegistrationMode {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Open => "open",
            Self::Approval => "approval",
            Self::Closed => "closed",
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct RegistrationUrlResponse {
    pub id: String,
    pub label: Option<String>,
    pub created_by_user_id: String,
    pub created_at: String,
    pub expires_at: Option<String>,
    pub max_uses: Option<u64>,
    #[serde(default)]
    pub use_count: u64,
    pub revoked_at: Option<String>,
    #[serde(default)]
    pub approval_required: bool,
    pub last_used_at: Option<String>,
    pub last_used_by_user_id: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct PendingRegistrationResponse {
    pub user_id: String,
    pub username: String,
    pub discriminator: u16,
    pub global_name: Option<String>,
    pub email: Option<String>,
    pub requested_at: String,
    pub registration_url_id: Option<String>,
    pub client_ip: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gateway_rollout: Option<GatewayRolloutConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registration: Option<InstanceRegistrationConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sso: Option<SsoConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub app_public: Option<AppPublicConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy: Option<InstancePolicyUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrations: Option<InstanceIntegrationsUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub media: Option<InstanceMediaUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstancePolicyUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direct_messages_disabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub premium_mode: Option<PremiumMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub services: Option<InstanceServicesUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceServicesUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gif_enabled: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub youtube_enabled: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bluesky_enabled: Option<Option<bool>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceIntegrationsUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub gif: Option<InstanceGifIntegrationUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub youtube: Option<InstanceYoutubeIntegrationUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub captcha: Option<InstanceCaptchaIntegrationUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub email: Option<InstanceEmailIntegrationUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bluesky: Option<InstanceBlueskyIntegrationUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceGifIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub klipy_api_key: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceYoutubeIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub api_key: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceCaptchaIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hcaptcha_site_key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hcaptcha_secret_key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub turnstile_site_key: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub turnstile_secret_key: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceEmailIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub from_email: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub from_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub smtp: Option<InstanceEmailSmtpIntegrationUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub disable_new_ip_authorization: Option<bool>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceEmailSmtpIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub host: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub username: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub password: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub secure: Option<bool>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceBlueskyIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logo_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tos_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy_uri: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keys: Option<Vec<InstanceBlueskyKeyIntegrationUpdateRequest>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceBlueskyKeyIntegrationUpdateRequest {
    pub kid: String,
    pub private_key: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceMediaUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub attachment_decay: Option<InstanceAttachmentDecayUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceAttachmentDecayUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_size_mb: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_size_mb: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_eligible_size_mb: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_lifetime_days: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_lifetime_days: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub curve: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub renew_threshold_days: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub renew_window_days: Option<u32>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceEmailSmtpTestRequest {
    pub host: String,
    pub port: u16,
    pub username: String,
    pub password: String,
    pub secure: bool,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceEmailSmtpTestResponse {
    #[serde(default)]
    pub ok: bool,
    pub error: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppPublicConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branding: Option<AppBrandingConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub setup: Option<AppSetupConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub legal: Option<AppLegalConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registration: Option<AppRegistrationConfigUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppBrandingConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub product_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub icon_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub symbol_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logo_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub wordmark_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub favicon_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub theme_color: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppSetupConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub configured: Option<bool>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppLegalConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub terms_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub privacy_url: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppRegistrationConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub collect_date_of_birth: Option<bool>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceRegistrationConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<RegistrationMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub admin_registration_urls_enabled: Option<bool>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct SsoConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enforced: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub display_name: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub issuer: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authorization_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub token_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub userinfo_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub jwks_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_id: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub client_secret: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scope: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_domains: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auto_provision: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub redirect_uri: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct GatewayRolloutConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_rollout_percentage: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub session_rollout_mode: Option<GatewayRolloutMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guild_rollout_percentage: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rpc_request_timeout_ms: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_concurrent_session_starts: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_concurrent_guild_starts: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub voice_e2ee_scope: Option<VoiceE2eeScope>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct CreateRegistrationUrlRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub label: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_uses: Option<u64>,
    pub approval_required: bool,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CreateRegistrationUrlResponse {
    pub registration_url: RegistrationUrlResponse,
    pub code: String,
    pub url: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct RegistrationUrlActionRequest {
    pub id: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct PendingRegistrationActionRequest {
    pub user_id: String,
}
