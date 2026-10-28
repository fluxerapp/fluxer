// SPDX-License-Identifier: AGPL-3.0-or-later

use serde::{Deserialize, Serialize};

use super::{InstanceBillingResponse, InstanceBillingUpdateRequest};
use crate::api::generated::types as generated_types;

pub use generated_types::{
    AccountIdentityLockReasonSchema as AccountIdentityLockReason,
    AccountsConfigSchema as AccountsConfigResponse, AccountsConfigUpdateRequest,
    AccountsConfigUpdateRequestDobMissingCleanup as DobMissingCleanupUpdateRequest,
    AccountsConfigUpdateRequestInactivityDeletion as InactivityDeletionUpdateRequest,
    AccountsConfigUpdateRequestUnclaimedAccountCleanup as UnclaimedAccountCleanupUpdateRequest,
    CaptchaProviderSchema as CaptchaProvider, CleanupModeSchema as CleanupMode,
    DateOfBirthPolicySchema as DateOfBirthPolicy, EmailDeliveryModeSchema as EmailDeliveryMode,
    EmailDeliveryStateSchema as EmailDeliveryState, GuildJoinGateSchema as GuildJoinGate,
    IdentifierFirstModeSchema as IdentifierFirstMode,
    InstanceConfigResponseAuth as AuthConfigResponse,
    InstantAccountAvailabilitySchema as InstantAccountAvailability,
    InteractionTierModeSchema as InteractionTierMode,
    LastSessionCleanupModeSchema as LastSessionCleanupMode,
    VerificationBasisSchema as VerificationBasis,
};

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceConfigResponse {
    pub sso: SsoConfigResponse,
    pub gateway_rollout: GatewayRolloutConfigResponse,
    #[serde(default)]
    pub registration: InstanceRegistrationResponse,
    #[serde(default)]
    pub self_hosted: bool,
    #[serde(default)]
    pub account_identity: AccountIdentityConfigResponse,
    #[serde(default)]
    pub app_public: AppPublicConfigResponse,
    #[serde(default)]
    pub policy: InstancePolicyResponse,
    pub integrations: InstanceIntegrationsResponse,
    #[serde(default)]
    pub media: InstanceMediaResponse,
    #[serde(default)]
    pub push_relay: PushRelayConfigResponse,
    #[serde(default)]
    pub domain_migration: DomainMigrationConfigResponse,
    #[serde(default)]
    pub voice_p2p: VoiceP2pConfigResponse,
    pub captcha: CaptchaConfigResponse,
    #[serde(default)]
    pub experiment_delivery: ExperimentDeliveryConfigResponse,
    #[serde(default)]
    pub billing: InstanceBillingResponse,
    pub auth: AuthConfigResponse,
    pub accounts: AccountsConfigResponse,
    #[serde(default)]
    pub guild_visits: GuildVisitsConfigResponse,
    #[serde(default)]
    pub notice: Option<InstanceNoticeResponse>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceNoticeResponse {
    pub id: String,
    pub message: String,
    pub url: Option<String>,
    pub starts_at: Option<String>,
    pub ends_at: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceNoticeUpdateRequest {
    pub message: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub starts_at: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ends_at: Option<String>,
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AccountIdentityMode {
    #[default]
    Email,
    Username,
}

impl AccountIdentityMode {
    pub fn is_username(self) -> bool {
        matches!(self, Self::Username)
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Email => "Email",
            Self::Username => "Username",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize, Eq, PartialEq)]
#[serde(rename_all = "snake_case")]
pub enum TagStyle {
    None,
    #[default]
    #[serde(other)]
    Random,
}

impl TagStyle {
    pub fn is_none(self) -> bool {
        matches!(self, Self::None)
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::None => "No tags",
            Self::Random => "Random tags",
        }
    }
}

#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct AccountIdentityConfigResponse {
    #[serde(default)]
    pub mode: AccountIdentityMode,
    #[serde(default)]
    pub locked: bool,
    #[serde(default)]
    pub tag_style: TagStyle,
    #[serde(default)]
    pub lock_reason: Option<AccountIdentityLockReason>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AccountIdentitySettings {
    pub mode: AccountIdentityMode,
    pub tag_style: TagStyle,
    pub emails_enabled: bool,
    pub username_from_provider: bool,
}

impl Default for AccountIdentitySettings {
    fn default() -> Self {
        Self {
            mode: AccountIdentityMode::default(),
            tag_style: TagStyle::default(),
            emails_enabled: true,
            username_from_provider: false,
        }
    }
}

impl AccountIdentitySettings {
    pub fn sso_manages_usernames(self) -> bool {
        self.username_from_provider && self.mode.is_username()
    }

    pub fn admin_reset_links_available(self) -> bool {
        self.mode.is_username() || !self.emails_enabled
    }
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstanceAccountIdentityDiscovery {
    #[serde(default)]
    pub features: InstanceAccountIdentityDiscoveryFeatures,
    #[serde(default)]
    pub sso: InstanceSsoDiscovery,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstanceSsoDiscovery {
    #[serde(default)]
    pub username_from_provider: bool,
}

#[derive(Clone, Debug, Default, Deserialize)]
pub struct InstanceAccountIdentityDiscoveryFeatures {
    #[serde(default)]
    pub emails_enabled: bool,
    #[serde(default)]
    pub account_identity: AccountIdentityMode,
    #[serde(default)]
    pub tag_style: TagStyle,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstancePolicyResponse {
    #[serde(default)]
    pub single_community_enabled: bool,
    pub single_community_guild_id: Option<String>,
    #[serde(default)]
    pub direct_messages_disabled: bool,
    #[serde(default)]
    pub direct_messages_locked: bool,
    #[serde(default)]
    pub friends_disabled: bool,
    #[serde(default)]
    pub personal_notes_enabled: bool,
    #[serde(default)]
    pub premium_mode: PremiumMode,
    #[serde(default = "default_guild_create_access")]
    pub guild_create_access: bool,
    #[serde(default)]
    pub services: InstanceServicesOverrides,
    #[serde(default)]
    pub services_resolved: InstanceServicesResolved,
    #[serde(default)]
    pub services_available: InstanceServicesAvailable,
}

fn default_guild_create_access() -> bool {
    true
}

impl Default for InstancePolicyResponse {
    fn default() -> Self {
        Self {
            single_community_enabled: false,
            single_community_guild_id: None,
            direct_messages_disabled: false,
            direct_messages_locked: false,
            friends_disabled: false,
            personal_notes_enabled: false,
            premium_mode: PremiumMode::Everyone,
            guild_create_access: default_guild_create_access(),
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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceIntegrationsResponse {
    #[serde(default)]
    pub gif: InstanceGifIntegrationResponse,
    #[serde(default)]
    pub youtube: InstanceYoutubeIntegrationResponse,
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

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceEmailIntegrationResponse {
    pub delivery: Option<EmailDeliveryMode>,
    pub delivery_state: EmailDeliveryState,
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
    pub tls_mode: Option<String>,
    pub effective_tls_mode: Option<String>,
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
    #[serde(default)]
    pub favicon_dark_url: Option<String>,
    pub theme_color: Option<String>,
    pub status_page_url: Option<String>,
    pub status_page_incident_history_url: Option<String>,
    #[serde(default = "default_premium_product_name")]
    pub premium_product_name: String,
    pub premium_info_url: Option<String>,
    pub sign_in_backdrop_url: Option<String>,
    pub custom_css: Option<String>,
    #[serde(default)]
    pub upload_limit_message: Option<String>,
    #[serde(default)]
    pub trusted_domains: Vec<String>,
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
            favicon_dark_url: None,
            theme_color: None,
            status_page_url: None,
            status_page_incident_history_url: None,
            premium_product_name: default_premium_product_name(),
            premium_info_url: None,
            sign_in_backdrop_url: None,
            custom_css: None,
            upload_limit_message: None,
            trusted_domains: Vec::new(),
        }
    }
}

fn default_product_name() -> String {
    "Fluxer".to_owned()
}

fn default_premium_product_name() -> String {
    "Premium".to_owned()
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
    #[serde(default)]
    pub guidelines_url: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct SsoConfigResponse {
    #[serde(default)]
    pub enabled: bool,
    #[serde(default)]
    pub enforced: bool,
    pub display_name: Option<String>,
    #[serde(default)]
    pub button_label: Option<String>,
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
    #[serde(default)]
    pub username_from_provider: bool,
    pub redirect_uri: Option<String>,
    #[serde(default)]
    pub groups_claim: Option<String>,
    #[serde(default)]
    pub group_role_mappings: Vec<SsoGroupRoleMapping>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
pub struct SsoGroupRoleMapping {
    pub group: String,
    pub role_id: String,
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

pub const EXPERIMENT_MAX_TARGETED_USERS: usize = 1_000;
pub const EXPERIMENT_MAX_ROLLOUT_COUNTRY_CODES: usize = 250;
pub const DOMAIN_MIGRATION_DEFAULT_SALT: &str = "domain-migration-v1";
pub const VOICE_P2P_DEFAULT_SALT: &str = "voice-p2p-v1";
pub const VOICE_P2P_MAX_PARTICIPANTS_RANGE: std::ops::RangeInclusive<u32> = 2..=4;
pub const CAPTCHA_COST_RANGE: std::ops::RangeInclusive<u32> = 1_000..=20_000;
pub const CAPTCHA_MAX_COUNTER_RANGE: std::ops::RangeInclusive<u32> = 100..=20_000;

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
#[serde(default)]
pub struct PushRelayConfigResponse {
    pub relay_consent_accepted: bool,
    pub relay_consent_accepted_at: Option<String>,
    pub relay_consent_accepted_by: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct PushRelayConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub relay_consent_accepted: Option<bool>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct DomainMigrationConfigResponse {
    pub enabled: bool,
    pub config_version: u64,
    pub rollout_basis_points: u32,
    pub rollout_country_codes: Vec<String>,
    pub rollout_salt: String,
    pub included_user_ids: Vec<String>,
    pub included_guild_ids: Vec<String>,
    pub include_premium_users: bool,
    pub excluded_user_ids: Vec<String>,
    pub anonymous_rollout_basis_points: u32,
    pub standalone_forwarding: bool,
}

impl Default for DomainMigrationConfigResponse {
    fn default() -> Self {
        Self {
            enabled: false,
            config_version: 0,
            rollout_basis_points: 0,
            rollout_country_codes: Vec::new(),
            rollout_salt: DOMAIN_MIGRATION_DEFAULT_SALT.to_owned(),
            included_user_ids: Vec::new(),
            included_guild_ids: Vec::new(),
            include_premium_users: false,
            excluded_user_ids: Vec::new(),
            anonymous_rollout_basis_points: 0,
            standalone_forwarding: false,
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct DomainMigrationConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_basis_points: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_country_codes: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_salt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_user_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_guild_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_premium_users: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_user_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anonymous_rollout_basis_points: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub standalone_forwarding: Option<bool>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct VoiceP2pConfigResponse {
    pub enabled: bool,
    pub config_version: u64,
    pub rollout_basis_points: u32,
    pub rollout_country_codes: Vec<String>,
    pub rollout_salt: String,
    pub included_user_ids: Vec<String>,
    pub included_guild_ids: Vec<String>,
    pub include_premium_users: bool,
    pub excluded_user_ids: Vec<String>,
    pub max_participants: u32,
}

impl Default for VoiceP2pConfigResponse {
    fn default() -> Self {
        Self {
            enabled: false,
            config_version: 0,
            rollout_basis_points: 0,
            rollout_country_codes: Vec::new(),
            rollout_salt: VOICE_P2P_DEFAULT_SALT.to_owned(),
            included_user_ids: Vec::new(),
            included_guild_ids: Vec::new(),
            include_premium_users: false,
            excluded_user_ids: Vec::new(),
            max_participants: *VOICE_P2P_MAX_PARTICIPANTS_RANGE.start(),
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct VoiceP2pConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_basis_points: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_country_codes: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rollout_salt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_user_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_guild_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub include_premium_users: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_user_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_participants: Option<u32>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CaptchaConfigResponse {
    pub provider: CaptchaProvider,
    pub cost: u32,
    pub max_counter: u32,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct CaptchaConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider: Option<CaptchaProvider>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cost: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_counter: Option<u32>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct ExperimentDeliveryConfigResponse {
    pub poll_interval_seconds: u64,
    pub poll_jitter_percent: u32,
}

impl Default for ExperimentDeliveryConfigResponse {
    fn default() -> Self {
        Self {
            poll_interval_seconds: 300,
            poll_jitter_percent: 15,
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct ExperimentDeliveryConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub poll_interval_seconds: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub poll_jitter_percent: Option<u32>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct InstanceRegistrationResponse {
    pub mode: RegistrationMode,
    #[serde(default = "default_registration_urls_enabled")]
    pub admin_registration_urls_enabled: bool,
    #[serde(default)]
    pub allowed_email_domains: Vec<String>,
    #[serde(default)]
    pub urls: Vec<RegistrationUrlResponse>,
}

impl Default for InstanceRegistrationResponse {
    fn default() -> Self {
        Self {
            mode: RegistrationMode::Open,
            admin_registration_urls_enabled: true,
            allowed_email_domains: Vec::new(),
            urls: Vec::new(),
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
    #[serde(default)]
    pub invite_code: Option<String>,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum PendingRegistrationStatus {
    Approved,
    Rejected,
}

impl PendingRegistrationStatus {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Approved => "approved",
            Self::Rejected => "rejected",
        }
    }
}

#[derive(Clone, Debug, Deserialize)]
pub struct PendingRegistrationListResponse {
    pub pending_registrations: Vec<PendingRegistrationResponse>,
}

pub const BRANDING_ASSET_MAX_IMAGE_CHARS: usize = 16_000_000;

pub const CUSTOM_CSS_MAX_CHARS: usize = 32_768;

pub const BRANDING_ASSET_ACCEPT: &str = ".png,.jpeg,.webp,.gif,.apng,.avif,.heic,.heif,.jxl,.svg,\
     image/png,image/jpeg,image/webp,image/gif,image/apng,image/avif,image/heic,image/heif,\
     image/jxl,image/svg+xml";

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum BrandingAssetKind {
    Icon,
    Symbol,
    Logo,
    Wordmark,
    Favicon,
    FaviconDark,
    SignInBackdrop,
}

impl BrandingAssetKind {
    pub const ALL: [Self; 7] = [
        Self::Icon,
        Self::Symbol,
        Self::Logo,
        Self::Wordmark,
        Self::Favicon,
        Self::FaviconDark,
        Self::SignInBackdrop,
    ];

    pub fn as_str(self) -> &'static str {
        match self {
            Self::Icon => "icon",
            Self::Symbol => "symbol",
            Self::Logo => "logo",
            Self::Wordmark => "wordmark",
            Self::Favicon => "favicon",
            Self::FaviconDark => "favicon_dark",
            Self::SignInBackdrop => "sign_in_backdrop",
        }
    }

    pub fn parse(value: &str) -> Option<Self> {
        Self::ALL.into_iter().find(|kind| kind.as_str() == value)
    }

    pub fn label(self) -> &'static str {
        match self {
            Self::Icon => "App icon",
            Self::Symbol => "Symbol",
            Self::Logo => "Logo",
            Self::Wordmark => "Wordmark",
            Self::Favicon => "Favicon",
            Self::FaviconDark => "Dark mode favicon",
            Self::SignInBackdrop => "Sign-in background",
        }
    }

    pub fn url(self, branding: &AppBrandingConfigResponse) -> Option<&str> {
        match self {
            Self::Icon => branding.icon_url.as_deref(),
            Self::Symbol => branding.symbol_url.as_deref(),
            Self::Logo => branding.logo_url.as_deref(),
            Self::Wordmark => branding.wordmark_url.as_deref(),
            Self::Favicon => branding.favicon_url.as_deref(),
            Self::FaviconDark => branding.favicon_dark_url.as_deref(),
            Self::SignInBackdrop => branding.sign_in_backdrop_url.as_deref(),
        }
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct BrandingAssetUploadRequest {
    pub kind: BrandingAssetKind,
    pub image: Option<String>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub push_relay: Option<PushRelayConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub domain_migration: Option<DomainMigrationConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub voice_p2p: Option<VoiceP2pConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub captcha: Option<CaptchaConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub experiment_delivery: Option<ExperimentDeliveryConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub billing: Option<InstanceBillingUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub auth: Option<AuthConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounts: Option<AccountsConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guild_visits: Option<GuildVisitsConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub notice: Option<InstanceNoticeUpdateRequest>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstancePolicyUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_guild_id: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub single_community_add_members: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direct_messages_disabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub direct_messages_locked: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub friends_disabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub personal_notes_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guild_create_access: Option<bool>,
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
pub struct InstanceEmailIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub delivery: Option<Option<EmailDeliveryMode>>,
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
    pub tls_mode: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceBlueskyIntegrationUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<Option<bool>>,
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
    pub enabled: Option<Option<bool>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_size_mb: Option<Option<f64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_size_mb: Option<Option<f64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_eligible_size_mb: Option<Option<f64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_lifetime_days: Option<Option<u32>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_lifetime_days: Option<Option<u32>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub curve: Option<Option<f64>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub renew_threshold_days: Option<Option<u32>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub renew_window_days: Option<Option<u32>>,
}

#[derive(Clone, Debug, Default, Deserialize, Serialize)]
pub struct InstanceEmailSmtpTestResponse {
    #[serde(default)]
    pub ok: bool,
    pub error: Option<String>,
    #[serde(default)]
    pub server_response: Option<String>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AppPublicConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub branding: Option<AppBrandingConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub setup: Option<AppSetupConfigUpdateRequest>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub legal: Option<AppLegalConfigUpdateRequest>,
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
    pub favicon_dark_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub theme_color: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_page_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status_page_incident_history_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub premium_product_name: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub premium_info_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sign_in_backdrop_url: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom_css: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub upload_limit_message: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trusted_domains: Option<Vec<String>>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guidelines_url: Option<Option<String>>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstanceRegistrationConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<RegistrationMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub admin_registration_urls_enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_email_domains: Option<Vec<String>>,
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
    pub button_label: Option<Option<String>>,
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
    pub username_from_provider: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub redirect_uri: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub groups_claim: Option<Option<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub group_role_mappings: Option<Vec<SsoGroupRoleMapping>>,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub invite_code: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub struct CreateRegistrationUrlResponse {
    pub registration_url: RegistrationUrlResponse,
    pub code: String,
    pub url: String,
}

pub const INSTANT_ACCOUNTS_ESCALATION_RANGE: std::ops::RangeInclusive<u32> = 10..=1_000_000;
pub const CLEANUP_INACTIVE_DAYS_RANGE: std::ops::RangeInclusive<u32> = 1..=3_650;
pub const INACTIVITY_WARNING_LEAD_DAYS_RANGE: std::ops::RangeInclusive<u32> = 1..=365;
pub const SCHEDULED_DELETION_GRACE_HOURS: u32 = 24;

#[derive(Clone, Debug, Default, Serialize)]
pub struct InstantAccountsConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pow_escalation_per_hour: Option<u32>,
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct AuthConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub identifier_first: Option<IdentifierFirstMode>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub instant_accounts: Option<InstantAccountsConfigUpdateRequest>,
}

#[derive(Clone, Debug, Deserialize, Serialize)]
#[serde(default)]
pub struct GuildVisitsConfigResponse {
    pub enabled: bool,
    pub user_basis_points: u32,
    pub user_salt: String,
    pub included_user_ids: Vec<String>,
    pub excluded_user_ids: Vec<String>,
}

pub const GUILD_VISITS_DEFAULT_SALT: &str = "guild-visits-user-v1";

impl Default for GuildVisitsConfigResponse {
    fn default() -> Self {
        Self {
            enabled: false,
            user_basis_points: 0,
            user_salt: GUILD_VISITS_DEFAULT_SALT.to_owned(),
            included_user_ids: Vec::new(),
            excluded_user_ids: Vec::new(),
        }
    }
}

#[derive(Clone, Debug, Default, Serialize)]
pub struct GuildVisitsConfigUpdateRequest {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_basis_points: Option<u32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_salt: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub included_user_ids: Option<Vec<String>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub excluded_user_ids: Option<Vec<String>>,
}

impl InstanceConfigResponse {
    pub fn loaded_marker(&self) -> String {
        let mut hashes = Vec::new();
        if let Ok(value) = serde_json::to_value(self) {
            collect_leaves(&value, String::new(), &mut |path, leaf| {
                hashes.push(leaf_hash(path, leaf));
            });
        }
        hashes.sort_unstable();
        hashes.dedup();
        hashes.concat()
    }

    pub fn changed_since(&self, loaded_marker: &str, update: &InstanceConfigUpdateRequest) -> bool {
        let loaded: std::collections::HashSet<&str> = loaded_marker
            .as_bytes()
            .chunks(LEAF_HASH_LEN)
            .filter_map(|chunk| std::str::from_utf8(chunk).ok())
            .collect();
        let (Ok(current), Ok(update)) = (serde_json::to_value(self), serde_json::to_value(update))
        else {
            return false;
        };
        let mut changed = false;
        collect_leaves(&update, String::new(), &mut |path, _| {
            if let Some(leaf) = leaf_at(&current, path).filter(|leaf| is_leaf(leaf)) {
                changed |= !loaded.contains(leaf_hash(path, leaf).as_str());
            }
        });
        changed
    }
}

const LEAF_HASH_LEN: usize = 8;

fn leaf_hash(path: &str, leaf: &serde_json::Value) -> String {
    use sha2::{Digest, Sha256};
    let digest = Sha256::digest(format!("{path}={leaf}").as_bytes());
    digest[..LEAF_HASH_LEN / 2]
        .iter()
        .map(|byte| format!("{byte:02x}"))
        .collect()
}

fn collect_leaves(
    value: &serde_json::Value,
    path: String,
    visit: &mut dyn FnMut(&str, &serde_json::Value),
) {
    match value {
        serde_json::Value::Object(fields) if !is_leaf(value) => {
            for (key, field) in fields {
                let child = if path.is_empty() {
                    key.clone()
                } else {
                    format!("{path}.{key}")
                };
                collect_leaves(field, child, visit);
            }
        }
        _ => visit(&path, value),
    }
}

fn is_leaf(value: &serde_json::Value) -> bool {
    !matches!(value, serde_json::Value::Object(fields) if !fields.is_empty())
}

fn leaf_at<'a>(value: &'a serde_json::Value, path: &str) -> Option<&'a serde_json::Value> {
    path.split('.').try_fold(value, |value, key| value.get(key))
}
