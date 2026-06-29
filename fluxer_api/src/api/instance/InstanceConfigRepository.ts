// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {
	type GatewayRolloutConfig,
	GatewayRolloutConfigSchema,
} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import type {IKVProvider, IKVSubscription} from '@pkgs/kv_client/src/IKVProvider';
import {Config} from '../Config';
import type {APIConfig, BlueskyOAuthConfig, BlueskyOAuthKeyConfig} from '../config/APIConfig';
import {sanitizeLimitConfigForInstance} from '../constants/LimitConfig';
import {fetchMany, fetchOne, upsertOne} from '../database/CassandraQueryExecution';
import type {InstanceConfigurationRow} from '../database/types/InstanceConfigTypes';
import {Logger} from '../Logger';
import {InstanceConfiguration} from '../Tables';
import {DEFAULT_DECAY_CONSTANTS, DEFAULT_RENEWAL_CONSTANTS} from '../utils/AttachmentDecay';
import {isJsonRecord, parseJsonArray, parseJsonRecord} from '../utils/JsonBoundaryUtils';
import {normalizeSsoAllowedEmailDomains} from './SsoConfigValidation';

const GATEWAY_ROLLOUT_CONFIG_KEY = 'gateway_rollout_config';
const REGISTRATION_CONFIG_KEY = 'registration_config';
const REGISTRATION_URLS_KEY = 'registration_urls';
const REGISTRATION_PENDING_APPROVALS_KEY = 'registration_pending_approvals';
const APP_PUBLIC_CONFIG_KEY = 'app_public_config';
const ADMIN_BOOTSTRAP_KEY = 'admin_bootstrapped';
const INSTANCE_POLICY_CONFIG_KEY = 'instance_policy_config';
const INSTANCE_INTEGRATIONS_CONFIG_KEY = 'instance_integrations_config';
const INSTANCE_MEDIA_CONFIG_KEY = 'instance_media_config';
export const INSTANCE_CONFIG_REFRESH_CHANNEL = 'instance-config-refresh';
export const REGISTRATION_PENDING_APPROVAL_TRAIT = 'registration_pending_approval';
export const REGISTRATION_REJECTED_TRAIT = 'registration_rejected';
const DEFAULT_GATEWAY_ROLLOUT_CONFIG: GatewayRolloutConfig = {
	session_rollout_percentage: 100,
	session_rollout_mode: 'modulo',
	guild_rollout_percentage: 100,
	rpc_request_timeout_ms: 10000,
	max_concurrent_session_starts: 512,
	max_concurrent_guild_starts: 256,
	gateway_dispatch_relay_shards: 32,
	gateway_dispatch_relay_max_queue: 50000,
	voice_e2ee_scope: 'guild_feature_only',
	voice_reconciliation_v3_percentage: 100,
	voice_reconciliation_v3_interval_ms: 2000,
};
export type InstanceRegistrationMode = 'open' | 'approval' | 'closed';
export interface InstanceRegistrationConfig {
	mode: InstanceRegistrationMode;
	admin_registration_urls_enabled: boolean;
}

export interface InstanceBrandingConfig {
	product_name: string;
	icon_url: string | null;
	symbol_url: string | null;
	logo_url: string | null;
	wordmark_url: string | null;
	favicon_url: string | null;
	theme_color: string | null;
}

interface InstanceAppPublicConfig {
	branding: InstanceBrandingConfig;
	setup: {
		configured: boolean;
	};
	legal: {
		terms_url: string | null;
		privacy_url: string | null;
	};
	registration: {
		collect_date_of_birth: boolean;
	};
}

export type InstancePremiumMode = 'mirror' | 'everyone';

export interface InstancePolicyConfig {
	single_community_enabled: boolean;
	single_community_locked: boolean;
	single_community_guild_id: string | null;
	direct_messages_disabled: boolean;
	direct_messages_locked: boolean;
	premium_mode: InstancePremiumMode;
	gif_enabled: boolean | null;
	youtube_enabled: boolean | null;
	bluesky_enabled: boolean | null;
}

interface InstanceCommunityPublicConfig {
	single_community: boolean;
	single_community_guild_id: string | null;
	direct_messages_disabled: boolean;
}

interface InstanceServicesPublicConfig {
	gif_enabled: boolean;
	youtube_enabled: boolean;
	bluesky_enabled: boolean;
}

export type InstanceCaptchaProvider = 'hcaptcha' | 'turnstile' | 'none';
type InstanceEmailProvider = 'smtp' | 'none';

interface InstanceGifIntegrationConfig {
	klipy_api_key: string | null;
}

interface InstanceYoutubeIntegrationConfig {
	api_key: string | null;
}

interface InstanceCaptchaIntegrationConfig {
	provider: InstanceCaptchaProvider | null;
	hcaptcha_site_key: string | null;
	hcaptcha_secret_key: string | null;
	turnstile_site_key: string | null;
	turnstile_secret_key: string | null;
}

interface InstanceEmailSmtpIntegrationConfig {
	host: string | null;
	port: number | null;
	username: string | null;
	password: string | null;
	secure: boolean | null;
}

interface InstanceEmailIntegrationConfig {
	enabled: boolean | null;
	provider: InstanceEmailProvider | null;
	from_email: string | null;
	from_name: string | null;
	smtp: InstanceEmailSmtpIntegrationConfig;
}

interface InstanceBlueskyKeyIntegrationConfig {
	kid: string;
	private_key: string | null;
}

interface InstanceBlueskyIntegrationConfig {
	enabled: boolean | null;
	client_name: string | null;
	client_uri: string | null;
	logo_uri: string | null;
	tos_uri: string | null;
	policy_uri: string | null;
	keys: Array<InstanceBlueskyKeyIntegrationConfig>;
}

interface InstanceIntegrationsConfig {
	gif: InstanceGifIntegrationConfig;
	youtube: InstanceYoutubeIntegrationConfig;
	captcha: InstanceCaptchaIntegrationConfig;
	email: InstanceEmailIntegrationConfig;
	bluesky: InstanceBlueskyIntegrationConfig;
}

interface InstanceGifEffectiveConfig {
	klipy_api_key: string | null;
	active_api_key: string | null;
	available: boolean;
}

export interface InstanceCaptchaEffectiveConfig {
	enabled: boolean;
	provider: InstanceCaptchaProvider;
	hcaptcha_site_key: string | null;
	hcaptcha_secret_key: string | null;
	turnstile_site_key: string | null;
	turnstile_secret_key: string | null;
}

interface InstanceIntegrationsAdminConfig {
	gif: {
		klipy_api_key_set: boolean;
		effective_available: boolean;
	};
	youtube: {
		api_key_set: boolean;
		effective_available: boolean;
	};
	captcha: {
		provider: InstanceCaptchaProvider | null;
		effective_provider: InstanceCaptchaProvider;
		hcaptcha_site_key: string | null;
		hcaptcha_secret_key_set: boolean;
		turnstile_site_key: string | null;
		turnstile_secret_key_set: boolean;
		effective_enabled: boolean;
	};
	email: {
		enabled: boolean | null;
		effective_enabled: boolean;
		provider: InstanceEmailProvider | null;
		effective_provider: InstanceEmailProvider;
		from_email: string | null;
		from_name: string | null;
		smtp: {
			host: string | null;
			port: number | null;
			username: string | null;
			password_set: boolean;
			secure: boolean | null;
		};
	};
	bluesky: {
		enabled: boolean | null;
		effective_enabled: boolean;
		client_name: string | null;
		client_uri: string | null;
		logo_uri: string | null;
		tos_uri: string | null;
		policy_uri: string | null;
		key_count: number;
	};
}

interface InstanceAttachmentDecayConfig {
	enabled: boolean | null;
	min_size_mb: number | null;
	max_size_mb: number | null;
	max_eligible_size_mb: number | null;
	min_lifetime_days: number | null;
	max_lifetime_days: number | null;
	curve: number | null;
	renew_threshold_days: number | null;
	renew_window_days: number | null;
}

interface InstanceMediaConfig {
	attachment_decay: InstanceAttachmentDecayConfig;
}

export interface InstanceAttachmentDecayEffectiveConfig {
	enabled: boolean;
	min_size_mb: number;
	max_size_mb: number;
	max_eligible_size_mb: number;
	min_lifetime_days: number;
	max_lifetime_days: number;
	curve: number;
	renew_threshold_days: number;
	renew_window_days: number;
}

interface InstanceMediaAdminConfig {
	attachment_decay: InstanceAttachmentDecayConfig & {
		effective: InstanceAttachmentDecayEffectiveConfig;
	};
}

interface InstanceIntegrationsConfigPatch {
	gif?: Partial<InstanceGifIntegrationConfig>;
	youtube?: Partial<InstanceYoutubeIntegrationConfig>;
	captcha?: Partial<InstanceCaptchaIntegrationConfig>;
	email?: Partial<Omit<InstanceEmailIntegrationConfig, 'smtp'>> & {
		smtp?: Partial<InstanceEmailSmtpIntegrationConfig>;
	};
	bluesky?: Partial<Omit<InstanceBlueskyIntegrationConfig, 'keys'>> & {
		keys?: Array<Partial<InstanceBlueskyKeyIntegrationConfig>>;
	};
}

interface InstanceMediaConfigPatch {
	attachment_decay?: Partial<InstanceAttachmentDecayConfig>;
}

export interface InstanceRegistrationUrl {
	id: string;
	label: string | null;
	code_hash: string;
	created_by_user_id: string;
	created_at: string;
	expires_at: string | null;
	max_uses: number | null;
	use_count: number;
	revoked_at: string | null;
	approval_required: boolean;
	last_used_at: string | null;
	last_used_by_user_id: string | null;
}

type InstanceRegistrationUrlPublic = Omit<InstanceRegistrationUrl, 'code_hash'>;

interface InstancePendingRegistration {
	user_id: string;
	username: string;
	discriminator: number;
	global_name: string | null;
	email: string | null;
	requested_at: string;
	registration_url_id: string | null;
	client_ip: string | null;
}

const DEFAULT_REGISTRATION_CONFIG: InstanceRegistrationConfig = {
	mode: 'open',
	admin_registration_urls_enabled: true,
};
const FETCH_CONFIG_QUERY = InstanceConfiguration.selectCql({
	where: InstanceConfiguration.where.eq('key'),
	limit: 1,
});
const FETCH_ALL_CONFIG_QUERY = InstanceConfiguration.selectCql();

function isStringArray(value: unknown): value is Array<string> {
	return Array.isArray(value) && value.every((entry) => typeof entry === 'string');
}

function isRegistrationMode(value: unknown): value is InstanceRegistrationMode {
	return value === 'open' || value === 'approval' || value === 'closed';
}

function normalizeNullableString(value: unknown): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value : null;
}

function normalizePublicString(value: unknown): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value.trim() : null;
}

function normalizeOptionalPublicString(
	value: Record<string, unknown>,
	key: string,
	fallback: string | null,
): string | null {
	if (!Object.hasOwn(value, key)) {
		return fallback;
	}
	return normalizePublicString(value[key]);
}

function getDefaultAppPublicConfig(): InstanceAppPublicConfig {
	return {
		branding: {
			product_name: Config.instance.branding.productName || 'Fluxer',
			icon_url: normalizePublicString(Config.instance.branding.iconUrl),
			symbol_url: normalizePublicString(Config.instance.branding.symbolUrl),
			logo_url: normalizePublicString(Config.instance.branding.logoUrl),
			wordmark_url: normalizePublicString(Config.instance.branding.wordmarkUrl),
			favicon_url: normalizePublicString(Config.instance.branding.faviconUrl),
			theme_color: normalizePublicString(Config.instance.branding.themeColor),
		},
		setup: {
			configured: !Config.instance.selfHosted || Config.instance.setup.configured,
		},
		legal: {
			terms_url: null,
			privacy_url: null,
		},
		registration: {
			collect_date_of_birth: !Config.instance.selfHosted,
		},
	};
}

function normalizeAppPublicConfig(value: unknown): InstanceAppPublicConfig {
	const defaults = getDefaultAppPublicConfig();
	if (!isJsonRecord(value)) {
		return defaults;
	}
	const branding = isJsonRecord(value.branding) ? value.branding : {};
	const setup = isJsonRecord(value.setup) ? value.setup : {};
	const legal = isJsonRecord(value.legal) ? value.legal : {};
	const registration = isJsonRecord(value.registration) ? value.registration : {};
	return {
		branding: {
			product_name: normalizePublicString(branding.product_name) ?? defaults.branding.product_name,
			icon_url: normalizeOptionalPublicString(branding, 'icon_url', defaults.branding.icon_url),
			symbol_url: normalizeOptionalPublicString(branding, 'symbol_url', defaults.branding.symbol_url),
			logo_url: normalizeOptionalPublicString(branding, 'logo_url', defaults.branding.logo_url),
			wordmark_url: normalizeOptionalPublicString(branding, 'wordmark_url', defaults.branding.wordmark_url),
			favicon_url: normalizeOptionalPublicString(branding, 'favicon_url', defaults.branding.favicon_url),
			theme_color: normalizeOptionalPublicString(branding, 'theme_color', defaults.branding.theme_color),
		},
		setup: {
			configured: typeof setup.configured === 'boolean' ? setup.configured : defaults.setup.configured,
		},
		legal: {
			terms_url: normalizeOptionalPublicString(legal, 'terms_url', defaults.legal.terms_url),
			privacy_url: normalizeOptionalPublicString(legal, 'privacy_url', defaults.legal.privacy_url),
		},
		registration: {
			collect_date_of_birth:
				typeof registration.collect_date_of_birth === 'boolean'
					? registration.collect_date_of_birth
					: defaults.registration.collect_date_of_birth,
		},
	};
}

const DEFAULT_INSTANCE_POLICY_CONFIG: InstancePolicyConfig = {
	single_community_enabled: false,
	single_community_locked: false,
	single_community_guild_id: null,
	direct_messages_disabled: false,
	direct_messages_locked: false,
	premium_mode: 'everyone',
	gif_enabled: null,
	youtube_enabled: null,
	bluesky_enabled: null,
};

function isPremiumMode(value: unknown): value is InstancePremiumMode {
	return value === 'mirror' || value === 'everyone';
}

function normalizeNullableBoolean(value: unknown): boolean | null {
	return typeof value === 'boolean' ? value : null;
}

function normalizeInstancePolicyConfig(value: unknown): InstancePolicyConfig {
	if (!isJsonRecord(value)) {
		return {...DEFAULT_INSTANCE_POLICY_CONFIG};
	}
	return {
		single_community_enabled: value.single_community_enabled === true,
		single_community_locked: value.single_community_locked === true,
		single_community_guild_id: normalizeNullableString(value.single_community_guild_id),
		direct_messages_disabled: value.direct_messages_disabled === true,
		direct_messages_locked: value.direct_messages_locked === true,
		premium_mode: isPremiumMode(value.premium_mode) ? value.premium_mode : DEFAULT_INSTANCE_POLICY_CONFIG.premium_mode,
		gif_enabled: normalizeNullableBoolean(value.gif_enabled),
		youtube_enabled: normalizeNullableBoolean(value.youtube_enabled),
		bluesky_enabled: normalizeNullableBoolean(value.bluesky_enabled),
	};
}

const DEFAULT_INSTANCE_INTEGRATIONS_CONFIG: InstanceIntegrationsConfig = {
	gif: {
		klipy_api_key: null,
	},
	youtube: {
		api_key: null,
	},
	captcha: {
		provider: null,
		hcaptcha_site_key: null,
		hcaptcha_secret_key: null,
		turnstile_site_key: null,
		turnstile_secret_key: null,
	},
	email: {
		enabled: null,
		provider: null,
		from_email: null,
		from_name: null,
		smtp: {
			host: null,
			port: null,
			username: null,
			password: null,
			secure: null,
		},
	},
	bluesky: {
		enabled: null,
		client_name: null,
		client_uri: null,
		logo_uri: null,
		tos_uri: null,
		policy_uri: null,
		keys: [],
	},
};

const DEFAULT_INSTANCE_ATTACHMENT_DECAY_CONFIG: InstanceAttachmentDecayConfig = {
	enabled: null,
	min_size_mb: null,
	max_size_mb: null,
	max_eligible_size_mb: null,
	min_lifetime_days: null,
	max_lifetime_days: null,
	curve: null,
	renew_threshold_days: null,
	renew_window_days: null,
};

const DEFAULT_INSTANCE_MEDIA_CONFIG: InstanceMediaConfig = {
	attachment_decay: DEFAULT_INSTANCE_ATTACHMENT_DECAY_CONFIG,
};

function isCaptchaProvider(value: unknown): value is InstanceCaptchaProvider {
	return value === 'hcaptcha' || value === 'turnstile' || value === 'none';
}

function isEmailProvider(value: unknown): value is InstanceEmailProvider {
	return value === 'smtp' || value === 'none';
}

function normalizeSecretString(value: unknown): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value.trim() : null;
}

function secretIsSet(value: unknown): boolean {
	return typeof value === 'string' && value.trim().length > 0;
}

function normalizeNullablePort(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < 1 || value > 65535) return null;
	return value;
}

function normalizeNullablePositiveNumber(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value <= 0) return null;
	return value;
}

function normalizeNullablePositiveInteger(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < 1) return null;
	return value;
}

function normalizeNullableCurve(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isFinite(value) || value < 0 || value > 1) return null;
	return value;
}

function normalizeBlueskyKey(value: unknown): InstanceBlueskyKeyIntegrationConfig | null {
	if (!isJsonRecord(value)) return null;
	const kid = normalizePublicString(value.kid);
	if (!kid) return null;
	return {
		kid,
		private_key: normalizeSecretString(value.private_key),
	};
}

function normalizeInstanceIntegrationsConfig(value: unknown): InstanceIntegrationsConfig {
	const defaults = DEFAULT_INSTANCE_INTEGRATIONS_CONFIG;
	if (!isJsonRecord(value)) {
		return structuredClone(defaults);
	}
	const gif = isJsonRecord(value.gif) ? value.gif : {};
	const youtube = isJsonRecord(value.youtube) ? value.youtube : {};
	const captcha = isJsonRecord(value.captcha) ? value.captcha : {};
	const email = isJsonRecord(value.email) ? value.email : {};
	const smtp = isJsonRecord(email.smtp) ? email.smtp : {};
	const bluesky = isJsonRecord(value.bluesky) ? value.bluesky : {};
	const blueskyKeys = Array.isArray(bluesky.keys)
		? bluesky.keys.flatMap((entry) => {
				const normalized = normalizeBlueskyKey(entry);
				return normalized ? [normalized] : [];
			})
		: defaults.bluesky.keys;
	return {
		gif: {
			klipy_api_key: normalizeSecretString(gif.klipy_api_key),
		},
		youtube: {
			api_key: normalizeSecretString(youtube.api_key),
		},
		captcha: {
			provider: isCaptchaProvider(captcha.provider) ? captcha.provider : defaults.captcha.provider,
			hcaptcha_site_key: normalizeSecretString(captcha.hcaptcha_site_key),
			hcaptcha_secret_key: normalizeSecretString(captcha.hcaptcha_secret_key),
			turnstile_site_key: normalizeSecretString(captcha.turnstile_site_key),
			turnstile_secret_key: normalizeSecretString(captcha.turnstile_secret_key),
		},
		email: {
			enabled: normalizeNullableBoolean(email.enabled),
			provider: isEmailProvider(email.provider) ? email.provider : defaults.email.provider,
			from_email: normalizePublicString(email.from_email),
			from_name: normalizePublicString(email.from_name),
			smtp: {
				host: normalizePublicString(smtp.host),
				port: normalizeNullablePort(smtp.port),
				username: normalizePublicString(smtp.username),
				password: normalizeSecretString(smtp.password),
				secure: normalizeNullableBoolean(smtp.secure),
			},
		},
		bluesky: {
			enabled: normalizeNullableBoolean(bluesky.enabled),
			client_name: normalizePublicString(bluesky.client_name),
			client_uri: normalizePublicString(bluesky.client_uri),
			logo_uri: normalizePublicString(bluesky.logo_uri),
			tos_uri: normalizePublicString(bluesky.tos_uri),
			policy_uri: normalizePublicString(bluesky.policy_uri),
			keys: blueskyKeys,
		},
	};
}

function normalizeInstanceAttachmentDecayConfig(value: unknown): InstanceAttachmentDecayConfig {
	if (!isJsonRecord(value)) {
		return {...DEFAULT_INSTANCE_ATTACHMENT_DECAY_CONFIG};
	}
	const minSizeMb = normalizeNullablePositiveNumber(value.min_size_mb);
	let maxSizeMb = normalizeNullablePositiveNumber(value.max_size_mb);
	let maxEligibleSizeMb = normalizeNullablePositiveNumber(value.max_eligible_size_mb);
	const minLifetimeDays = normalizeNullablePositiveInteger(value.min_lifetime_days);
	let maxLifetimeDays = normalizeNullablePositiveInteger(value.max_lifetime_days);
	if (minSizeMb !== null && maxSizeMb !== null && maxSizeMb <= minSizeMb) {
		maxSizeMb = null;
	}
	if (maxSizeMb !== null && maxEligibleSizeMb !== null && maxEligibleSizeMb < maxSizeMb) {
		maxEligibleSizeMb = null;
	}
	if (minLifetimeDays !== null && maxLifetimeDays !== null && maxLifetimeDays < minLifetimeDays) {
		maxLifetimeDays = null;
	}
	return {
		enabled: normalizeNullableBoolean(value.enabled),
		min_size_mb: minSizeMb,
		max_size_mb: maxSizeMb,
		max_eligible_size_mb: maxEligibleSizeMb,
		min_lifetime_days: minLifetimeDays,
		max_lifetime_days: maxLifetimeDays,
		curve: normalizeNullableCurve(value.curve),
		renew_threshold_days: normalizeNullablePositiveInteger(value.renew_threshold_days),
		renew_window_days: normalizeNullablePositiveInteger(value.renew_window_days),
	};
}

function normalizeInstanceMediaConfig(value: unknown): InstanceMediaConfig {
	if (!isJsonRecord(value)) {
		return structuredClone(DEFAULT_INSTANCE_MEDIA_CONFIG);
	}
	return {
		attachment_decay: normalizeInstanceAttachmentDecayConfig(value.attachment_decay),
	};
}

function hasCompleteSmtpConfig(config: APIConfig['email']): boolean {
	if (config.provider !== 'smtp' || !config.smtp) return false;
	return Boolean(
		config.fromEmail.trim() &&
			config.smtp.host.trim() &&
			config.smtp.port &&
			config.smtp.username.trim() &&
			config.smtp.password.trim(),
	);
}

function normalizeIsoDateString(value: unknown): string | null {
	if (typeof value !== 'string') return null;
	const time = Date.parse(value);
	return Number.isFinite(time) ? new Date(time).toISOString() : null;
}

function normalizePositiveInteger(value: unknown): number | null {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < 1) return null;
	return value;
}

function normalizeNonNegativeInteger(value: unknown): number {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < 0) return 0;
	return value;
}

function normalizeRegistrationConfig(value: unknown): InstanceRegistrationConfig {
	if (!isJsonRecord(value)) {
		return {...DEFAULT_REGISTRATION_CONFIG};
	}
	const mode = isRegistrationMode(value.mode) ? value.mode : DEFAULT_REGISTRATION_CONFIG.mode;
	const adminRegistrationUrlsEnabled =
		typeof value.admin_registration_urls_enabled === 'boolean'
			? value.admin_registration_urls_enabled
			: typeof value.adminRegistrationUrlsEnabled === 'boolean'
				? value.adminRegistrationUrlsEnabled
				: DEFAULT_REGISTRATION_CONFIG.admin_registration_urls_enabled;
	return {
		mode,
		admin_registration_urls_enabled: adminRegistrationUrlsEnabled,
	};
}

function normalizeRegistrationUrl(value: unknown): InstanceRegistrationUrl | null {
	if (!isJsonRecord(value)) return null;
	if (
		typeof value.id !== 'string' ||
		typeof value.code_hash !== 'string' ||
		typeof value.created_by_user_id !== 'string'
	) {
		return null;
	}
	const createdAt = normalizeIsoDateString(value.created_at);
	if (!createdAt) return null;
	return {
		id: value.id,
		label: normalizeNullableString(value.label),
		code_hash: value.code_hash,
		created_by_user_id: value.created_by_user_id,
		created_at: createdAt,
		expires_at: normalizeIsoDateString(value.expires_at),
		max_uses: normalizePositiveInteger(value.max_uses),
		use_count: normalizeNonNegativeInteger(value.use_count),
		revoked_at: normalizeIsoDateString(value.revoked_at),
		approval_required: value.approval_required === true,
		last_used_at: normalizeIsoDateString(value.last_used_at),
		last_used_by_user_id: normalizeNullableString(value.last_used_by_user_id),
	};
}

function normalizePendingRegistration(value: unknown): InstancePendingRegistration | null {
	if (!isJsonRecord(value)) return null;
	if (typeof value.user_id !== 'string' || typeof value.username !== 'string') return null;
	const requestedAt = normalizeIsoDateString(value.requested_at);
	if (!requestedAt) return null;
	return {
		user_id: value.user_id,
		username: value.username,
		discriminator: typeof value.discriminator === 'number' ? value.discriminator : 0,
		global_name: normalizeNullableString(value.global_name),
		email: normalizeNullableString(value.email),
		requested_at: requestedAt,
		registration_url_id: normalizeNullableString(value.registration_url_id),
		client_ip: normalizeNullableString(value.client_ip),
	};
}

function isRegistrationUrlUsable(registrationUrl: InstanceRegistrationUrl, now: Date): boolean {
	if (registrationUrl.revoked_at) return false;
	if (registrationUrl.expires_at && Date.parse(registrationUrl.expires_at) <= now.getTime()) return false;
	if (registrationUrl.max_uses !== null && registrationUrl.use_count >= registrationUrl.max_uses) return false;
	return true;
}

function redactRegistrationUrl(registrationUrl: InstanceRegistrationUrl): InstanceRegistrationUrlPublic {
	const {code_hash: _codeHash, ...redacted} = registrationUrl;
	return redacted;
}

function isLimitRuleSnapshot(value: unknown): value is LimitConfigSnapshot['rules'][number] {
	if (!isJsonRecord(value) || typeof value.id !== 'string' || !isJsonRecord(value.limits)) return false;
	const filters = value.filters;
	return (
		(filters === undefined ||
			(isJsonRecord(filters) &&
				(filters.traits === undefined || isStringArray(filters.traits)) &&
				(filters.guildFeatures === undefined || isStringArray(filters.guildFeatures)))) &&
		(value.modifiedFields === undefined || isStringArray(value.modifiedFields))
	);
}

function isLimitConfigSnapshot(value: unknown): value is LimitConfigSnapshot {
	return (
		isJsonRecord(value) &&
		(value.version === undefined || typeof value.version === 'number') &&
		isStringArray(value.traitDefinitions) &&
		Array.isArray(value.rules) &&
		value.rules.every(isLimitRuleSnapshot)
	);
}

export interface InstanceSsoConfig {
	enabled: boolean;
	enforced: boolean;
	displayName: string | null;
	issuer: string | null;
	authorizationUrl: string | null;
	tokenUrl: string | null;
	userInfoUrl: string | null;
	jwksUrl: string | null;
	clientId: string | null;
	clientSecret?: string | null;
	clientSecretSet?: boolean;
	scope: string | null;
	allowedEmailDomains: Array<string>;
	autoProvision: boolean;
	redirectUri: string | null;
}

export class InstanceConfigRepository {
	private readonly pubSubSourceId = crypto.randomUUID();
	private readonly kvClient: IKVProvider | null;
	private configCache: Map<string, string> | null = null;
	private cacheInitializationPromise: Promise<void> | null = null;
	private refreshPromise: Promise<void> | null = null;
	private refreshRequested = false;
	private kvSubscription: IKVSubscription | null = null;
	private subscriberInitialized = false;
	private subscriberInitializationPromise: Promise<boolean> | null = null;
	private messageHandler: ((channel: string, message: string) => void) | null = null;

	constructor(kvClient: IKVProvider | null = null) {
		this.kvClient = kvClient;
	}

	async getConfig(key: string): Promise<string | null> {
		if (!(await this.ensureCacheReadable())) {
			return this.fetchConfigFromDatabase(key);
		}
		return this.configCache?.get(key) ?? null;
	}

	async getAllConfigs(): Promise<Map<string, string>> {
		if (!(await this.ensureCacheReadable())) {
			return this.fetchAllConfigsFromDatabase();
		}
		return new Map(this.configCache ?? []);
	}

	async setConfig(key: string, value: string): Promise<void> {
		await this.writeConfig(key, value);
		this.updateCachedConfigs([[key, value]]);
		await this.publishRefresh();
	}

	private async setConfigs(entries: Array<[string, string]>): Promise<void> {
		if (entries.length === 0) return;
		await Promise.all(entries.map(([key, value]) => this.writeConfig(key, value)));
		this.updateCachedConfigs(entries);
		await this.publishRefresh();
	}

	private async writeConfig(key: string, value: string): Promise<void> {
		await upsertOne(
			InstanceConfiguration.upsertAll({
				key,
				value,
				updated_at: new Date(),
			}),
		);
	}

	private async fetchConfigFromDatabase(key: string): Promise<string | null> {
		const row = await fetchOne<InstanceConfigurationRow>(FETCH_CONFIG_QUERY, {key});
		return row?.value ?? null;
	}

	private async fetchAllConfigsFromDatabase(): Promise<Map<string, string>> {
		const rows = await fetchMany<InstanceConfigurationRow>(FETCH_ALL_CONFIG_QUERY, {});
		const configs = new Map<string, string>();
		for (const row of rows) {
			if (row.value != null) {
				configs.set(row.key, row.value);
			}
		}
		return configs;
	}

	private async ensureCacheReadable(): Promise<boolean> {
		if (!this.kvClient) {
			return false;
		}
		if (this.configCache !== null) {
			return true;
		}
		if (!this.cacheInitializationPromise) {
			this.cacheInitializationPromise = (async () => {
				const subscribed = await this.ensureSubscriberInitialized();
				if (!subscribed) {
					return;
				}
				await this.refreshCacheFromDatabase();
			})().finally(() => {
				this.cacheInitializationPromise = null;
			});
		}
		await this.cacheInitializationPromise;
		return this.configCache !== null;
	}

	private async ensureSubscriberInitialized(): Promise<boolean> {
		if (this.subscriberInitialized) {
			return true;
		}
		if (!this.kvClient) {
			return false;
		}
		if (!this.subscriberInitializationPromise) {
			this.subscriberInitializationPromise = this.startSubscriber()
				.then(() => true)
				.catch((error) => {
					Logger.error({error}, 'Failed to subscribe to instance config refresh channel');
					this.closeSubscription();
					return false;
				})
				.finally(() => {
					this.subscriberInitializationPromise = null;
				});
		}
		return this.subscriberInitializationPromise;
	}

	private async startSubscriber(): Promise<void> {
		if (!this.kvClient) {
			return;
		}
		const subscription = this.kvClient.duplicate();
		this.kvSubscription = subscription;
		this.messageHandler = (channel: string, message: string) => {
			if (channel !== INSTANCE_CONFIG_REFRESH_CHANNEL) {
				return;
			}
			if (this.isOwnRefreshMessage(message)) {
				return;
			}
			this.refreshCacheFromDatabase().catch((error) => {
				Logger.error({error}, 'Failed to refresh instance config cache from pubsub');
			});
		};
		subscription.on('message', this.messageHandler);
		await subscription.connect();
		await subscription.subscribe(INSTANCE_CONFIG_REFRESH_CHANNEL);
		this.subscriberInitialized = true;
	}

	private async refreshCacheFromDatabase(): Promise<void> {
		if (this.refreshPromise) {
			this.refreshRequested = true;
			await this.refreshPromise;
			return;
		}
		this.refreshPromise = (async () => {
			do {
				this.refreshRequested = false;
				this.configCache = await this.fetchAllConfigsFromDatabase();
			} while (this.refreshRequested);
		})().finally(() => {
			this.refreshPromise = null;
		});
		await this.refreshPromise;
	}

	private updateCachedConfigs(entries: Array<[string, string]>): void {
		if (!this.configCache) {
			return;
		}
		for (const [key, value] of entries) {
			this.configCache.set(key, value);
		}
	}

	private async publishRefresh(): Promise<void> {
		if (!this.kvClient) {
			return;
		}
		await this.kvClient.publish(
			INSTANCE_CONFIG_REFRESH_CHANNEL,
			JSON.stringify({source_id: this.pubSubSourceId, type: 'refresh'}),
		);
	}

	private isOwnRefreshMessage(message: string): boolean {
		const parsed = parseJsonRecord(message);
		return parsed?.source_id === this.pubSubSourceId;
	}

	private closeSubscription(): void {
		if (this.kvSubscription && this.messageHandler) {
			this.kvSubscription.off('message', this.messageHandler);
		}
		if (this.kvSubscription) {
			this.kvSubscription.disconnect();
			this.kvSubscription = null;
		}
		this.messageHandler = null;
		this.subscriberInitialized = false;
	}

	shutdown(): void {
		this.closeSubscription();
	}

	clearCacheForTesting(): void {
		this.closeSubscription();
		this.configCache = null;
		this.cacheInitializationPromise = null;
		this.refreshPromise = null;
		this.refreshRequested = false;
		this.subscriberInitializationPromise = null;
	}

	async isAdminBootstrapped(): Promise<boolean> {
		return (await this.getConfig(ADMIN_BOOTSTRAP_KEY)) === 'true';
	}

	async markAdminBootstrapped(): Promise<void> {
		await this.setConfig(ADMIN_BOOTSTRAP_KEY, 'true');
	}

	async getGatewayRolloutConfig(): Promise<GatewayRolloutConfig> {
		const raw = await this.getConfig(GATEWAY_ROLLOUT_CONFIG_KEY);
		if (!raw) {
			return {...DEFAULT_GATEWAY_ROLLOUT_CONFIG};
		}
		try {
			const parsed = parseJsonRecord(raw);
			if (!parsed) {
				return {...DEFAULT_GATEWAY_ROLLOUT_CONFIG};
			}
			const normalizedParsed: Record<string, unknown> = {...parsed};
			if (
				normalizedParsed.rpc_request_timeout_ms === undefined &&
				typeof normalizedParsed.nats_request_timeout_ms === 'number'
			) {
				normalizedParsed.rpc_request_timeout_ms = normalizedParsed.nats_request_timeout_ms;
			}
			return GatewayRolloutConfigSchema.parse({...DEFAULT_GATEWAY_ROLLOUT_CONFIG, ...normalizedParsed});
		} catch (error) {
			Logger.error({error}, 'Invalid gateway rollout config');
			throw error;
		}
	}

	async setGatewayRolloutConfig(config: GatewayRolloutConfig): Promise<void> {
		await this.setConfig(GATEWAY_ROLLOUT_CONFIG_KEY, JSON.stringify(config));
	}

	async hasLimitConfig(): Promise<boolean> {
		const raw = await this.getConfig('limit_config');
		return raw !== null;
	}

	async getLimitConfig(): Promise<LimitConfigSnapshot | null> {
		const raw = await this.getConfig('limit_config');
		if (!raw) {
			return null;
		}
		try {
			const parsed: unknown = JSON.parse(raw);
			if (!isLimitConfigSnapshot(parsed)) {
				return null;
			}
			const policy = await this.getInstancePolicyConfig();
			return sanitizeLimitConfigForInstance(parsed, {
				selfHosted: Config.instance.selfHosted,
				premiumMode: policy.premium_mode,
			});
		} catch (error) {
			Logger.warn({error}, 'Invalid limit config JSON, returning null');
			return null;
		}
	}

	async setLimitConfig(config: LimitConfigSnapshot): Promise<void> {
		await this.setConfig('limit_config', JSON.stringify(config));
	}

	async getAppPublicConfig(): Promise<InstanceAppPublicConfig> {
		const raw = await this.getConfig(APP_PUBLIC_CONFIG_KEY);
		if (!raw) {
			return getDefaultAppPublicConfig();
		}
		try {
			const parsed: unknown = JSON.parse(raw);
			return normalizeAppPublicConfig(parsed);
		} catch (error) {
			Logger.warn({error}, 'Invalid app public config JSON, returning defaults');
			return getDefaultAppPublicConfig();
		}
	}

	async setAppPublicConfig(config: {
		branding?: Partial<InstanceBrandingConfig>;
		setup?: Partial<InstanceAppPublicConfig['setup']>;
		legal?: Partial<InstanceAppPublicConfig['legal']>;
		registration?: Partial<InstanceAppPublicConfig['registration']>;
	}): Promise<InstanceAppPublicConfig> {
		const current = await this.getAppPublicConfig();
		const next = normalizeAppPublicConfig({
			branding: {
				...current.branding,
				...(config.branding ?? {}),
			},
			setup: {
				...current.setup,
				...(config.setup ?? {}),
			},
			legal: {
				...current.legal,
				...(config.legal ?? {}),
			},
			registration: {
				...current.registration,
				...(config.registration ?? {}),
			},
		});
		await this.setConfig(APP_PUBLIC_CONFIG_KEY, JSON.stringify(next));
		return next;
	}

	async getInstancePolicyConfig(): Promise<InstancePolicyConfig> {
		const raw = await this.getConfig(INSTANCE_POLICY_CONFIG_KEY);
		if (!raw) {
			return {...DEFAULT_INSTANCE_POLICY_CONFIG};
		}
		return normalizeInstancePolicyConfig(parseJsonRecord(raw));
	}

	async setInstancePolicyConfig(config: Partial<InstancePolicyConfig>): Promise<InstancePolicyConfig> {
		const current = await this.getInstancePolicyConfig();
		const next = normalizeInstancePolicyConfig({...current, ...config});
		await this.setConfig(INSTANCE_POLICY_CONFIG_KEY, JSON.stringify(next));
		return next;
	}

	async getInstanceIntegrationsConfig(): Promise<InstanceIntegrationsConfig> {
		const raw = await this.getConfig(INSTANCE_INTEGRATIONS_CONFIG_KEY);
		if (!raw) {
			return normalizeInstanceIntegrationsConfig(null);
		}
		return normalizeInstanceIntegrationsConfig(parseJsonRecord(raw));
	}

	async setInstanceIntegrationsConfig(config: InstanceIntegrationsConfigPatch): Promise<InstanceIntegrationsConfig> {
		const current = await this.getInstanceIntegrationsConfig();
		const next = normalizeInstanceIntegrationsConfig({
			gif: {
				...current.gif,
				...(config.gif ?? {}),
			},
			youtube: {
				...current.youtube,
				...(config.youtube ?? {}),
			},
			captcha: {
				...current.captcha,
				...(config.captcha ?? {}),
			},
			email: {
				...current.email,
				...(config.email ?? {}),
				smtp: {
					...current.email.smtp,
					...(config.email?.smtp ?? {}),
				},
			},
			bluesky: {
				...current.bluesky,
				...(config.bluesky ?? {}),
				keys: config.bluesky?.keys ?? current.bluesky.keys,
			},
		});
		await this.setConfig(INSTANCE_INTEGRATIONS_CONFIG_KEY, JSON.stringify(next));
		return next;
	}

	async getInstanceMediaConfig(): Promise<InstanceMediaConfig> {
		const raw = await this.getConfig(INSTANCE_MEDIA_CONFIG_KEY);
		if (!raw) {
			return normalizeInstanceMediaConfig(null);
		}
		return normalizeInstanceMediaConfig(parseJsonRecord(raw));
	}

	async setInstanceMediaConfig(config: InstanceMediaConfigPatch): Promise<InstanceMediaConfig> {
		const current = await this.getInstanceMediaConfig();
		const next = normalizeInstanceMediaConfig({
			attachment_decay: {
				...current.attachment_decay,
				...(config.attachment_decay ?? {}),
			},
		});
		await this.setConfig(INSTANCE_MEDIA_CONFIG_KEY, JSON.stringify(next));
		return next;
	}

	async getEffectiveAttachmentDecayConfig(): Promise<InstanceAttachmentDecayEffectiveConfig> {
		const media = await this.getInstanceMediaConfig();
		const attachmentDecay = media.attachment_decay;
		const minSizeMb = attachmentDecay.min_size_mb ?? DEFAULT_DECAY_CONSTANTS.MIN_MB;
		const configuredMaxSizeMb = attachmentDecay.max_size_mb ?? DEFAULT_DECAY_CONSTANTS.MAX_MB;
		const maxSizeMb =
			configuredMaxSizeMb > minSizeMb ? configuredMaxSizeMb : Math.max(DEFAULT_DECAY_CONSTANTS.MAX_MB, minSizeMb + 1);
		const configuredMaxEligibleSizeMb = attachmentDecay.max_eligible_size_mb ?? DEFAULT_DECAY_CONSTANTS.PLAN_MB;
		const maxEligibleSizeMb = Math.max(maxSizeMb, configuredMaxEligibleSizeMb);
		const minLifetimeDays = attachmentDecay.min_lifetime_days ?? DEFAULT_DECAY_CONSTANTS.MIN_DAYS;
		const configuredMaxLifetimeDays = attachmentDecay.max_lifetime_days ?? DEFAULT_DECAY_CONSTANTS.MAX_DAYS;
		const maxLifetimeDays =
			configuredMaxLifetimeDays >= minLifetimeDays
				? configuredMaxLifetimeDays
				: Math.max(DEFAULT_DECAY_CONSTANTS.MAX_DAYS, minLifetimeDays);
		return {
			enabled: attachmentDecay.enabled ?? Config.attachmentDecayEnabled,
			min_size_mb: minSizeMb,
			max_size_mb: maxSizeMb,
			max_eligible_size_mb: maxEligibleSizeMb,
			min_lifetime_days: minLifetimeDays,
			max_lifetime_days: maxLifetimeDays,
			curve: attachmentDecay.curve ?? DEFAULT_DECAY_CONSTANTS.CURVE,
			renew_threshold_days: attachmentDecay.renew_threshold_days ?? DEFAULT_RENEWAL_CONSTANTS.RENEW_THRESHOLD_DAYS,
			renew_window_days: attachmentDecay.renew_window_days ?? DEFAULT_RENEWAL_CONSTANTS.RENEW_WINDOW_DAYS,
		};
	}

	async isAttachmentDecayEnabled(): Promise<boolean> {
		return (await this.getEffectiveAttachmentDecayConfig()).enabled;
	}

	async getInstanceMediaAdminConfig(): Promise<InstanceMediaAdminConfig> {
		const [media, attachmentDecay] = await Promise.all([
			this.getInstanceMediaConfig(),
			this.getEffectiveAttachmentDecayConfig(),
		]);
		return {
			attachment_decay: {
				...media.attachment_decay,
				effective: attachmentDecay,
			},
		};
	}

	async getEffectiveGifConfig(): Promise<InstanceGifEffectiveConfig> {
		const integrations = await this.getInstanceIntegrationsConfig();
		const klipyApiKey = integrations.gif.klipy_api_key ?? normalizeSecretString(Config.klipy.apiKey);
		return {
			klipy_api_key: klipyApiKey,
			active_api_key: klipyApiKey,
			available: Boolean(klipyApiKey),
		};
	}

	async getEffectiveYoutubeApiKey(): Promise<string | null> {
		const integrations = await this.getInstanceIntegrationsConfig();
		return integrations.youtube.api_key ?? normalizeSecretString(Config.youtube.apiKey);
	}

	async getEffectiveCaptchaConfig(): Promise<InstanceCaptchaEffectiveConfig> {
		const integrations = await this.getInstanceIntegrationsConfig();
		const provider = integrations.captcha.provider ?? (Config.captcha.enabled ? Config.captcha.provider : 'none');
		const hcaptchaSiteKey =
			integrations.captcha.hcaptcha_site_key ?? normalizeSecretString(Config.captcha.hcaptcha?.siteKey);
		const hcaptchaSecretKey =
			integrations.captcha.hcaptcha_secret_key ?? normalizeSecretString(Config.captcha.hcaptcha?.secretKey);
		const turnstileSiteKey =
			integrations.captcha.turnstile_site_key ?? normalizeSecretString(Config.captcha.turnstile?.siteKey);
		const turnstileSecretKey =
			integrations.captcha.turnstile_secret_key ?? normalizeSecretString(Config.captcha.turnstile?.secretKey);
		const providerReady =
			provider === 'hcaptcha'
				? Boolean(hcaptchaSiteKey && hcaptchaSecretKey)
				: provider === 'turnstile'
					? Boolean(turnstileSiteKey && turnstileSecretKey)
					: false;
		return {
			enabled: provider !== 'none' && providerReady,
			provider,
			hcaptcha_site_key: hcaptchaSiteKey,
			hcaptcha_secret_key: hcaptchaSecretKey,
			turnstile_site_key: turnstileSiteKey,
			turnstile_secret_key: turnstileSecretKey,
		};
	}

	async getEffectiveEmailConfig(): Promise<APIConfig['email']> {
		const integrations = await this.getInstanceIntegrationsConfig();
		const provider = integrations.email.provider ?? Config.email.provider;
		const fromEmail = integrations.email.from_email ?? Config.email.fromEmail;
		const fromName = integrations.email.from_name ?? Config.email.fromName;
		const smtp =
			provider === 'smtp'
				? {
						host: integrations.email.smtp.host ?? Config.email.smtp?.host ?? '',
						port: integrations.email.smtp.port ?? Config.email.smtp?.port ?? 587,
						username: integrations.email.smtp.username ?? Config.email.smtp?.username ?? '',
						password: integrations.email.smtp.password ?? Config.email.smtp?.password ?? '',
						secure: integrations.email.smtp.secure ?? Config.email.smtp?.secure ?? true,
					}
				: undefined;
		const next: APIConfig['email'] = {
			...Config.email,
			enabled: integrations.email.enabled ?? Config.email.enabled,
			provider,
			fromEmail,
			fromName,
			smtp,
		};
		return {
			...next,
			enabled: next.enabled && hasCompleteSmtpConfig(next),
		};
	}

	async isEmailEnabled(): Promise<boolean> {
		return (await this.getEffectiveEmailConfig()).enabled;
	}

	async getEffectiveBlueskyConfig(): Promise<BlueskyOAuthConfig> {
		const integrations = await this.getInstanceIntegrationsConfig();
		const runtimeKeys = integrations.bluesky.keys.flatMap((key): Array<BlueskyOAuthKeyConfig> => {
			if (!key.private_key) return [];
			return [{kid: key.kid, private_key: key.private_key}];
		});
		const keys = runtimeKeys.length > 0 ? runtimeKeys : Config.auth.bluesky.keys;
		const enabled = (integrations.bluesky.enabled ?? Config.auth.bluesky.enabled) && keys.length > 0;
		return {
			...Config.auth.bluesky,
			enabled,
			client_name: integrations.bluesky.client_name ?? Config.auth.bluesky.client_name,
			client_uri: integrations.bluesky.client_uri ?? Config.auth.bluesky.client_uri,
			logo_uri: integrations.bluesky.logo_uri ?? Config.auth.bluesky.logo_uri,
			tos_uri: integrations.bluesky.tos_uri ?? Config.auth.bluesky.tos_uri,
			policy_uri: integrations.bluesky.policy_uri ?? Config.auth.bluesky.policy_uri,
			keys,
		};
	}

	async getInstanceIntegrationsAdminConfig(): Promise<InstanceIntegrationsAdminConfig> {
		const [integrations, gif, youtubeApiKey, captcha, email, bluesky] = await Promise.all([
			this.getInstanceIntegrationsConfig(),
			this.getEffectiveGifConfig(),
			this.getEffectiveYoutubeApiKey(),
			this.getEffectiveCaptchaConfig(),
			this.getEffectiveEmailConfig(),
			this.getEffectiveBlueskyConfig(),
		]);
		return {
			gif: {
				klipy_api_key_set: secretIsSet(integrations.gif.klipy_api_key) || secretIsSet(Config.klipy.apiKey),
				effective_available: gif.available,
			},
			youtube: {
				api_key_set: secretIsSet(integrations.youtube.api_key) || secretIsSet(Config.youtube.apiKey),
				effective_available: Boolean(youtubeApiKey),
			},
			captcha: {
				provider: integrations.captcha.provider,
				effective_provider: captcha.provider,
				hcaptcha_site_key: captcha.hcaptcha_site_key,
				hcaptcha_secret_key_set:
					secretIsSet(integrations.captcha.hcaptcha_secret_key) || secretIsSet(Config.captcha.hcaptcha?.secretKey),
				turnstile_site_key: captcha.turnstile_site_key,
				turnstile_secret_key_set:
					secretIsSet(integrations.captcha.turnstile_secret_key) || secretIsSet(Config.captcha.turnstile?.secretKey),
				effective_enabled: captcha.enabled,
			},
			email: {
				enabled: integrations.email.enabled,
				effective_enabled: email.enabled,
				provider: integrations.email.provider,
				effective_provider: email.provider,
				from_email: email.fromEmail || null,
				from_name: email.fromName || null,
				smtp: {
					host: email.smtp?.host || null,
					port: email.smtp?.port ?? null,
					username: email.smtp?.username || null,
					password_set: secretIsSet(integrations.email.smtp.password) || secretIsSet(Config.email.smtp?.password),
					secure: email.smtp?.secure ?? null,
				},
			},
			bluesky: {
				enabled: integrations.bluesky.enabled,
				effective_enabled: bluesky.enabled,
				client_name: bluesky.client_name || null,
				client_uri: bluesky.client_uri || null,
				logo_uri: bluesky.logo_uri || null,
				tos_uri: bluesky.tos_uri || null,
				policy_uri: bluesky.policy_uri || null,
				key_count: bluesky.keys.length,
			},
		};
	}

	async getInstanceCommunityPublicConfig(): Promise<InstanceCommunityPublicConfig> {
		const policy = await this.getInstancePolicyConfig();
		return {
			single_community: policy.single_community_enabled,
			single_community_guild_id: policy.single_community_enabled ? policy.single_community_guild_id : null,
			direct_messages_disabled: policy.direct_messages_disabled,
		};
	}

	async getResolvedServicesConfig(): Promise<InstanceServicesPublicConfig> {
		const [policy, gif, youtubeApiKey, bluesky] = await Promise.all([
			this.getInstancePolicyConfig(),
			this.getEffectiveGifConfig(),
			this.getEffectiveYoutubeApiKey(),
			this.getEffectiveBlueskyConfig(),
		]);
		return {
			gif_enabled: policy.gif_enabled ?? gif.available,
			youtube_enabled: policy.youtube_enabled ?? Boolean(youtubeApiKey),
			bluesky_enabled: policy.bluesky_enabled ?? bluesky.enabled,
		};
	}

	async getRegistrationConfig(): Promise<InstanceRegistrationConfig> {
		const raw = await this.getConfig(REGISTRATION_CONFIG_KEY);
		if (!raw) {
			return {...DEFAULT_REGISTRATION_CONFIG};
		}
		const parsed = parseJsonRecord(raw);
		return normalizeRegistrationConfig(parsed);
	}

	async setRegistrationConfig(config: Partial<InstanceRegistrationConfig>): Promise<InstanceRegistrationConfig> {
		const current = await this.getRegistrationConfig();
		const next = normalizeRegistrationConfig({
			mode: config.mode ?? current.mode,
			admin_registration_urls_enabled:
				config.admin_registration_urls_enabled ?? current.admin_registration_urls_enabled,
		});
		await this.setConfig(REGISTRATION_CONFIG_KEY, JSON.stringify(next));
		return next;
	}

	async getRegistrationPublicConfig(): Promise<InstanceRegistrationConfig> {
		return this.getRegistrationConfig();
	}

	async getRegistrationUrls(): Promise<Array<InstanceRegistrationUrl>> {
		const raw = await this.getConfig(REGISTRATION_URLS_KEY);
		if (!raw) {
			return [];
		}
		const parsed = parseJsonArray(raw);
		if (!parsed) {
			Logger.warn('Invalid registration URL config JSON, returning empty list');
			return [];
		}
		return parsed.flatMap((entry) => {
			const normalized = normalizeRegistrationUrl(entry);
			return normalized ? [normalized] : [];
		});
	}

	async getRegistrationUrlsForAdmin(): Promise<Array<InstanceRegistrationUrlPublic>> {
		return (await this.getRegistrationUrls())
			.toSorted((a, b) => Date.parse(b.created_at) - Date.parse(a.created_at))
			.map(redactRegistrationUrl);
	}

	async createRegistrationUrl(params: {
		label: string | null;
		createdByUserId: string;
		expiresAt: Date | null;
		maxUses: number | null;
		approvalRequired: boolean;
	}): Promise<{registrationUrl: InstanceRegistrationUrlPublic; code: string}> {
		const id = crypto.randomUUID();
		const code = id;
		const registrationUrl: InstanceRegistrationUrl = {
			id,
			label: params.label,
			code_hash: this.hashRegistrationUrlCode(code),
			created_by_user_id: params.createdByUserId,
			created_at: new Date().toISOString(),
			expires_at: params.expiresAt?.toISOString() ?? null,
			max_uses: params.maxUses,
			use_count: 0,
			revoked_at: null,
			approval_required: params.approvalRequired,
			last_used_at: null,
			last_used_by_user_id: null,
		};
		const registrationUrls = await this.getRegistrationUrls();
		await this.setRegistrationUrls([registrationUrl, ...registrationUrls]);
		return {registrationUrl: redactRegistrationUrl(registrationUrl), code};
	}

	async revokeRegistrationUrl(id: string): Promise<void> {
		const now = new Date().toISOString();
		const registrationUrls = await this.getRegistrationUrls();
		await this.setRegistrationUrls(
			registrationUrls.map((registrationUrl) =>
				registrationUrl.id === id && !registrationUrl.revoked_at
					? {...registrationUrl, revoked_at: now}
					: registrationUrl,
			),
		);
	}

	async resolveRegistrationUrlCode(code: string): Promise<InstanceRegistrationUrl | null> {
		const normalizedCode = code.trim();
		if (!normalizedCode) return null;
		const hash = this.hashRegistrationUrlCode(normalizedCode);
		const now = new Date();
		const registrationUrls = await this.getRegistrationUrls();
		return (
			registrationUrls.find(
				(registrationUrl) =>
					(registrationUrl.id === normalizedCode || registrationUrl.code_hash === hash) &&
					isRegistrationUrlUsable(registrationUrl, now),
			) ?? null
		);
	}

	async recordRegistrationUrlUse(id: string, userId: string): Promise<void> {
		const now = new Date().toISOString();
		const registrationUrls = await this.getRegistrationUrls();
		await this.setRegistrationUrls(
			registrationUrls.map((registrationUrl) =>
				registrationUrl.id === id
					? {
							...registrationUrl,
							use_count: registrationUrl.use_count + 1,
							last_used_at: now,
							last_used_by_user_id: userId,
						}
					: registrationUrl,
			),
		);
	}

	async getPendingRegistrations(): Promise<Array<InstancePendingRegistration>> {
		const raw = await this.getConfig(REGISTRATION_PENDING_APPROVALS_KEY);
		if (!raw) {
			return [];
		}
		const parsed = parseJsonArray(raw);
		if (!parsed) {
			Logger.warn('Invalid pending registration config JSON, returning empty list');
			return [];
		}
		return parsed
			.flatMap((entry) => {
				const normalized = normalizePendingRegistration(entry);
				return normalized ? [normalized] : [];
			})
			.toSorted((a, b) => Date.parse(a.requested_at) - Date.parse(b.requested_at));
	}

	async addPendingRegistration(pendingRegistration: InstancePendingRegistration): Promise<void> {
		const pendingRegistrations = await this.getPendingRegistrations();
		const next = [
			pendingRegistration,
			...pendingRegistrations.filter((entry) => entry.user_id !== pendingRegistration.user_id),
		];
		await this.setConfig(REGISTRATION_PENDING_APPROVALS_KEY, JSON.stringify(next));
	}

	async removePendingRegistration(userId: string): Promise<void> {
		const pendingRegistrations = await this.getPendingRegistrations();
		await this.setConfig(
			REGISTRATION_PENDING_APPROVALS_KEY,
			JSON.stringify(pendingRegistrations.filter((entry) => entry.user_id !== userId)),
		);
	}

	async getSsoConfig(options?: {includeSecret?: boolean}): Promise<InstanceSsoConfig> {
		const configs = await this.getAllConfigs();
		const read = (key: string): string | null => {
			const v = configs.get(key);
			if (!v) return null;
			const trimmed = v.trim();
			return trimmed.length === 0 ? null : trimmed;
		};
		const allowedDomainsRaw = configs.get('sso_allowed_domains');
		let allowedDomains: Array<string> = [];
		if (allowedDomainsRaw) {
			const parsed = parseJsonArray(allowedDomainsRaw);
			if (parsed) {
				allowedDomains = parsed.map((item) => String(item)).filter((item) => item.length > 0);
			} else {
				allowedDomains = allowedDomainsRaw
					.split(',')
					.map((s) => s.trim())
					.filter((s) => s.length > 0);
			}
		}
		const clientSecret = read('sso_client_secret');
		const enabled = configs.get('sso_enabled') === 'true';
		const enforcedRaw = configs.get('sso_enforced');
		return {
			enabled,
			enforced: enforcedRaw == null ? enabled : enforcedRaw === 'true',
			displayName: read('sso_display_name'),
			issuer: read('sso_issuer'),
			authorizationUrl: read('sso_authorization_url'),
			tokenUrl: read('sso_token_url'),
			userInfoUrl: read('sso_userinfo_url'),
			jwksUrl: read('sso_jwks_url'),
			clientId: read('sso_client_id'),
			clientSecret: options?.includeSecret ? clientSecret : undefined,
			clientSecretSet: Boolean(clientSecret),
			scope: read('sso_scope'),
			allowedEmailDomains: allowedDomains,
			autoProvision: configs.get('sso_auto_provision') !== 'false',
			redirectUri: null,
		};
	}

	async setSsoConfig(config: Partial<InstanceSsoConfig>): Promise<InstanceSsoConfig> {
		const current = await this.getSsoConfig({includeSecret: true});
		const definedConfig = Object.fromEntries(
			Object.entries(config).filter(([, value]) => value !== undefined),
		) as Partial<InstanceSsoConfig>;
		const next: InstanceSsoConfig = {
			...current,
			...definedConfig,
			clientSecret: config.clientSecret !== undefined ? config.clientSecret : current.clientSecret,
		};
		if (config.enabled === true && config.enforced === undefined && !current.enabled) {
			next.enforced = true;
		}
		let allowedEmailDomains: Array<string>;
		try {
			allowedEmailDomains = normalizeSsoAllowedEmailDomains(next.allowedEmailDomains ?? []);
		} catch (error) {
			if (next.enabled) {
				throw error;
			}
			Logger.warn({error}, 'Clearing invalid SSO allowed domain config while SSO is disabled');
			allowedEmailDomains = [];
		}
		const entries: Array<[string, string]> = [
			['sso_enabled', next.enabled ? 'true' : 'false'],
			['sso_enforced', next.enforced ? 'true' : 'false'],
			['sso_display_name', next.displayName ?? ''],
			['sso_issuer', next.issuer ?? ''],
			['sso_authorization_url', next.authorizationUrl ?? ''],
			['sso_token_url', next.tokenUrl ?? ''],
			['sso_userinfo_url', next.userInfoUrl ?? ''],
			['sso_jwks_url', next.jwksUrl ?? ''],
			['sso_client_id', next.clientId ?? ''],
			['sso_scope', next.scope ?? ''],
			['sso_allowed_domains', JSON.stringify(allowedEmailDomains)],
			['sso_auto_provision', next.autoProvision ? 'true' : 'false'],
			['sso_redirect_uri', ''],
		];
		if (config.clientSecret !== undefined) {
			entries.push(['sso_client_secret', config.clientSecret ?? '']);
		}
		await this.setConfigs(entries);
		return this.getSsoConfig({includeSecret: true});
	}

	private async setRegistrationUrls(registrationUrls: Array<InstanceRegistrationUrl>): Promise<void> {
		await this.setConfig(REGISTRATION_URLS_KEY, JSON.stringify(registrationUrls));
	}

	private hashRegistrationUrlCode(code: string): string {
		return crypto.createHash('sha256').update(code).digest('hex');
	}
}
