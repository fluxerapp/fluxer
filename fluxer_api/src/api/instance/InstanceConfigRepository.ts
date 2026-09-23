// SPDX-License-Identifier: AGPL-3.0-or-later

import crypto from 'node:crypto';
import {Config} from '@app/api/Config';
import type {APIConfig, BlueskyOAuthConfig, BlueskyOAuthKeyConfig} from '@app/api/config/APIConfig';
import {executeConditional, fetchMany, fetchOne, upsertOne} from '@app/api/database/CassandraQueryExecution';
import {Db, type PreparedQuery} from '@app/api/database/CassandraTypes';
import type {InstanceConfigurationRow} from '@app/api/database/types/InstanceConfigTypes';
import {
	getDefaultDateOfBirthCollection,
	setCachedDateOfBirthCollection,
} from '@app/api/instance/DateOfBirthCollectionCache';
import {InstanceConfigCache} from '@app/api/instance/InstanceConfigCache';
import {normalizeSsoAllowedEmailDomains} from '@app/api/instance/SsoConfigValidation';
import {Logger} from '@app/api/Logger';
import {isLimitConfigSnapshot} from '@app/api/limits/LimitConfigValidation';
import {resolveDeferredPhoneGateEnabled, setCachedDeferredPhoneGateEnabled} from '@app/api/risk/DeferredPhoneGateCache';
import {InstanceConfiguration} from '@app/api/Tables';
import {DEFAULT_DECAY_CONSTANTS, DEFAULT_RENEWAL_CONSTANTS} from '@app/api/utils/AttachmentDecay';
import {isJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ConflictError} from '@fluxer/errors/src/domains/core/ConflictError';
import {ServiceUnavailableError} from '@fluxer/errors/src/domains/core/ServiceUnavailableError';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {
	InstanceConfigResponse,
	InstanceConfigUpdateRequest,
	type PendingRegistrationResponse,
	type RegistrationUrlResponse,
} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {
	type GatewayRolloutConfig,
	GatewayRolloutConfigSchema,
} from '@fluxer/schema/src/domains/admin/GatewayRolloutSchemas';
import {
	type PushServiceDeliveryConfig,
	PushServiceDeliveryConfigSchema,
} from '@fluxer/schema/src/domains/admin/PushServiceDeliverySchemas';
import {
	type ScreenShareDeliveryConfig,
	ScreenShareDeliveryConfigSchema,
} from '@fluxer/schema/src/domains/admin/ScreenShareDeliverySchemas';
import {
	type VoiceNoiseSuppressionConfig,
	VoiceNoiseSuppressionConfigSchema,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	type ExperimentDeliveryConfig,
	ExperimentDeliveryConfigSchema,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
import {
	type InstanceAppPublic,
	InstanceAppPublicSchema,
	type InstanceBranding,
	type InstanceCaptchaProvider,
	InstanceCaptchaProviderSchema,
	type InstanceCommunity,
	type InstanceRegistration,
	InstanceRegistrationSchema,
	type InstanceServices,
	type InstanceSetup,
} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {normalizeString, SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {z} from 'zod';

const GATEWAY_ROLLOUT_CONFIG_KEY = 'gateway_rollout_config';
const VOICE_NOISE_SUPPRESSION_CONFIG_KEY = 'voice_noise_suppression_config';
const SCREEN_SHARE_DELIVERY_CONFIG_KEY = 'screen_share_delivery_config';
const PUSH_SERVICE_DELIVERY_CONFIG_KEY = 'push_service_delivery_config';
const EXPERIMENT_DELIVERY_CONFIG_KEY = 'experiment_delivery_config';
const REGISTRATION_CONFIG_KEY = 'registration_config';
const REGISTRATION_URLS_KEY = 'registration_urls';
const REGISTRATION_PENDING_APPROVALS_KEY = 'registration_pending_approvals';
const APP_PUBLIC_CONFIG_KEY = 'app_public_config';
const ADMIN_BOOTSTRAP_KEY = 'admin_bootstrapped';
const INSTANCE_POLICY_CONFIG_KEY = 'instance_policy_config';
const LIMIT_CONFIG_KEY = 'limit_config';
const INSTANCE_INTEGRATIONS_CONFIG_KEY = 'instance_integrations_config';
const INSTANCE_MEDIA_CONFIG_KEY = 'instance_media_config';
export const INSTANCE_CONFIG_REFRESH_CHANNEL = 'instance-config-refresh';
export const REGISTRATION_PENDING_APPROVAL_TRAIT = 'registration_pending_approval';
export const REGISTRATION_REJECTED_TRAIT = 'registration_rejected';
export const INSTANCE_CONFIG_WRITE_ATTEMPTS = 5;

export class InstanceConfigWriteConflictError extends ConflictError {
	constructor(key: string) {
		super({
			code: APIErrorCodes.CONFLICT,
			message: `Instance config "${key}" changed concurrently on all ${INSTANCE_CONFIG_WRITE_ATTEMPTS} write attempts. Nothing was written. Retry the change.`,
		});
		this.name = 'InstanceConfigWriteConflictError';
	}
}

interface StoredValueUpdate<T> {
	value: string | null;
	result: T;
}

export type InstanceRegistrationConfig = InstanceRegistration;

interface InstanceAppPublicConfig extends Omit<InstanceAppPublic, 'setup'> {
	setup: Pick<InstanceSetup, 'configured'>;
}

export type InstancePremiumMode = 'mirror' | 'everyone';

interface LimitConfigInputs {
	config: LimitConfigSnapshot | null;
	premiumMode: InstancePremiumMode;
}

export interface InstancePolicyConfig {
	single_community_enabled: boolean;
	single_community_guild_id: string | null;
	direct_messages_disabled: boolean;
	direct_messages_locked: boolean;
	premium_mode: InstancePremiumMode;
	gif_enabled: boolean | null;
	youtube_enabled: boolean | null;
	bluesky_enabled: boolean | null;
	deferred_phone_gate_enabled: boolean;
	deferred_phone_gate_window_hours: number;
	deferred_phone_gate_member_threshold: number;
}

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
	disable_new_ip_authorization: boolean | null;
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
		disable_new_ip_authorization: boolean;
		effective_disable_new_ip_authorization: boolean;
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

export interface InstanceRegistrationUrl extends RegistrationUrlResponse {
	code_hash: string;
}

type InstanceRegistrationUrlPublic = RegistrationUrlResponse;
type InstancePendingRegistration = PendingRegistrationResponse;

export interface RegistrationUrlClaim {
	registration_url_id: string;
	user_id: string;
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
const FETCH_LIMIT_CONFIG_INPUTS_QUERY = InstanceConfiguration.selectCql({
	where: InstanceConfiguration.where.in('key', 'keys'),
});

function parseStoredLimitConfig(raw: string | null): LimitConfigSnapshot | null {
	if (raw === null) return null;
	let parsed: unknown;
	try {
		parsed = JSON.parse(raw);
	} catch (error) {
		Logger.error({error}, 'Stored limit configuration is not valid JSON, falling back to default limits');
		return null;
	}
	if (!isLimitConfigSnapshot(parsed)) {
		Logger.error('Stored limit configuration has an invalid shape, falling back to default limits');
		return null;
	}
	return parsed;
}

function normalizeOptionalString(value: unknown): string | null {
	return typeof value === 'string' && value.trim().length > 0 ? value.trim() : null;
}

function normalizeOptionalPublicString(value: string | null | undefined, fallback: string | null): string | null {
	return value === undefined ? fallback : normalizeOptionalString(value);
}

function getDefaultAppPublicConfig(): InstanceAppPublicConfig {
	return {
		branding: {
			product_name: Config.instance.branding.productName || 'Fluxer',
			icon_url: normalizeOptionalString(Config.instance.branding.iconUrl),
			symbol_url: normalizeOptionalString(Config.instance.branding.symbolUrl),
			logo_url: normalizeOptionalString(Config.instance.branding.logoUrl),
			wordmark_url: normalizeOptionalString(Config.instance.branding.wordmarkUrl),
			favicon_url: normalizeOptionalString(Config.instance.branding.faviconUrl),
			theme_color: normalizeOptionalString(Config.instance.branding.themeColor),
			status_page_url: normalizeOptionalString(Config.instance.branding.statusPageUrl),
			status_page_incident_history_url: normalizeOptionalString(Config.instance.branding.statusPageIncidentHistoryUrl),
		},
		setup: {
			configured: !Config.instance.selfHosted || Config.instance.setup.configured,
		},
		legal: {
			terms_url: null,
			privacy_url: null,
		},
		registration: {
			collect_date_of_birth: getDefaultDateOfBirthCollection(),
		},
	};
}

type StoredConfigSection =
	| 'app public'
	| 'gateway rollout'
	| 'voice noise suppression'
	| 'screen share delivery'
	| 'push service delivery'
	| 'experiment delivery'
	| 'instance policy'
	| 'integrations'
	| 'media'
	| 'registration'
	| 'registration URLs'
	| 'pending registrations'
	| 'SSO flags'
	| 'SSO allowed domains';

function parseStoredConfigValue(raw: string | null, section: StoredConfigSection): unknown {
	if (raw === null) return {};
	try {
		return JSON.parse(raw);
	} catch {
		throw new Error(`Stored ${section} configuration is not valid JSON`);
	}
}

function describeInvalidFields(issues: ReadonlyArray<{path: ReadonlyArray<PropertyKey>}>, index?: number): string {
	const maxReportedIssues = 8;
	const invalidFields = new Set(
		issues.slice(0, maxReportedIssues).map((issue) => {
			const path = index === undefined ? issue.path : [index, ...issue.path];
			return path.length === 0 ? '<root>' : path.join('.');
		}),
	);
	const suffix = issues.length > maxReportedIssues ? ', ...' : '';
	return `${[...invalidFields].join(', ')}${suffix}`;
}

function validateStoredConfig<T>(
	schema: z.ZodType<T>,
	value: unknown,
	section: StoredConfigSection,
	index?: number,
): T {
	const result = schema.safeParse(value);
	if (!result.success) {
		throw new Error(
			`Stored ${section} configuration has invalid fields: ${describeInvalidFields(result.error.issues, index)}`,
		);
	}
	return result.data;
}

function removeStoredPaths(value: unknown, paths: ReadonlyArray<ReadonlyArray<PropertyKey>>): void {
	const arrays = new Set<Array<unknown>>();
	for (const path of paths) {
		let parent: unknown = value;
		for (const key of path.slice(0, -1)) {
			if (typeof parent !== 'object' || parent === null) break;
			parent = (parent as Record<PropertyKey, unknown>)[key];
		}
		if (typeof parent !== 'object' || parent === null) continue;
		delete (parent as Record<PropertyKey, unknown>)[path.at(-1)!];
		if (Array.isArray(parent)) arrays.add(parent);
	}
	for (const array of arrays) {
		array.splice(0, array.length, ...array.filter(() => true));
	}
}

function salvageStoredConfig<T>(schema: z.ZodType<T>, value: unknown, section: StoredConfigSection): T {
	const strict = schema.safeParse(value);
	if (strict.success) return strict.data;
	Logger.error(
		{section, invalidFields: describeInvalidFields(strict.error.issues)},
		'Invalid stored instance configuration, falling back to defaults for the invalid fields',
	);
	const signature = (issues: ReadonlyArray<{path: ReadonlyArray<PropertyKey>}>) =>
		issues.map((issue) => issue.path.join('.')).join('|');
	const salvaged = structuredClone(value);
	let issues = strict.error.issues;
	let trim = 0;
	for (let attempt = 0; attempt < 64; attempt++) {
		const paths = issues.map((issue) => issue.path.slice(0, Math.max(0, issue.path.length - trim)));
		if (paths.some((path) => path.length === 0)) break;
		removeStoredPaths(salvaged, paths);
		const result = schema.safeParse(salvaged);
		if (result.success) return result.data;
		trim = signature(result.error.issues) === signature(issues) ? trim + 1 : 0;
		issues = result.error.issues;
	}
	return schema.parse({});
}

function parseStoredConfig<T>(schema: z.ZodType<T>, raw: string | null, section: StoredConfigSection): T {
	return validateStoredConfig(schema, parseStoredConfigValue(raw, section), section);
}

function readStoredConfigOrDefault<T>(section: StoredConfigSection, decode: () => T, fallback: () => T): T {
	try {
		return decode();
	} catch (error) {
		Logger.error({error, section}, 'Invalid stored instance configuration, falling back to defaults');
		return fallback();
	}
}

function parseStoredConfigOrDefault<T>(schema: z.ZodType<T>, raw: string | null, section: StoredConfigSection): T {
	return readStoredConfigOrDefault(
		section,
		() => parseStoredConfig(schema, raw, section),
		() => parseStoredConfig(schema, null, section),
	);
}

function readStoredConfigValue(raw: string | null, section: StoredConfigSection): unknown {
	return readStoredConfigOrDefault(
		section,
		() => parseStoredConfigValue(raw, section),
		() => ({}),
	);
}

function checkStoredConfig(section: StoredConfigSection, decode: () => unknown): void {
	readStoredConfigOrDefault(section, decode, () => null);
}

function decodeGatewayRolloutConfig(value: unknown): GatewayRolloutConfig {
	const input =
		isJsonRecord(value) &&
		!Object.hasOwn(value, 'rpc_request_timeout_ms') &&
		Object.hasOwn(value, 'nats_request_timeout_ms')
			? {...value, rpc_request_timeout_ms: value.nats_request_timeout_ms}
			: value;
	return validateStoredConfig(GatewayRolloutConfigSchema, input, 'gateway rollout');
}

function parseStoredGatewayRolloutConfig(raw: string | null): GatewayRolloutConfig {
	return decodeGatewayRolloutConfig(parseStoredConfigValue(raw, 'gateway rollout'));
}

function parseStoredVoiceNoiseSuppressionConfig(raw: string | null): VoiceNoiseSuppressionConfig {
	return parseStoredConfigOrDefault(VoiceNoiseSuppressionConfigSchema, raw, 'voice noise suppression');
}

function parseStoredScreenShareDeliveryConfig(raw: string | null): ScreenShareDeliveryConfig {
	return parseStoredConfigOrDefault(ScreenShareDeliveryConfigSchema, raw, 'screen share delivery');
}

function parseStoredPushServiceDeliveryConfig(raw: string | null): PushServiceDeliveryConfig {
	return parseStoredConfigOrDefault(PushServiceDeliveryConfigSchema, raw, 'push service delivery');
}

function parseStoredExperimentDeliveryConfig(raw: string | null): ExperimentDeliveryConfig {
	return parseStoredConfigOrDefault(ExperimentDeliveryConfigSchema, raw, 'experiment delivery');
}

function validateStoredCollection<T>(schema: z.ZodType<T>, value: unknown, section: StoredConfigSection): Array<T> {
	if (!Array.isArray(value)) {
		throw new Error(`Stored ${section} configuration must be an array`);
	}
	return Array.from(value, (entry, index) => validateStoredConfig(schema, entry, section, index));
}

function parseStoredCollection<T>(schema: z.ZodType<T>, raw: string | null, section: StoredConfigSection): Array<T> {
	return raw === null ? [] : validateStoredCollection(schema, parseStoredConfigValue(raw, section), section);
}

const StoredInstanceAppPublicSchema = InstanceAppPublicSchema.extend({
	branding: InstanceAppPublicSchema.shape.branding.partial().optional(),
	setup: InstanceAppPublicSchema.shape.setup.pick({configured: true}).partial().optional(),
	legal: InstanceAppPublicSchema.shape.legal.partial().optional(),
	registration: InstanceAppPublicSchema.shape.registration.partial().optional(),
});

function parseStoredAppPublicConfig(raw: string | null): InstanceAppPublicConfig {
	return buildAppPublicConfig(
		salvageStoredConfig(StoredInstanceAppPublicSchema, readStoredConfigValue(raw, 'app public'), 'app public'),
	);
}

function decodeAppPublicConfig(value: unknown): InstanceAppPublicConfig {
	return buildAppPublicConfig(validateStoredConfig(StoredInstanceAppPublicSchema, value, 'app public'));
}

function buildAppPublicConfig(config: z.infer<typeof StoredInstanceAppPublicSchema>): InstanceAppPublicConfig {
	const defaults = getDefaultAppPublicConfig();
	const {branding = {}, setup = {}, legal = {}, registration = {}} = config;
	return {
		branding: {
			product_name: normalizeOptionalString(branding.product_name) ?? defaults.branding.product_name,
			icon_url: normalizeOptionalPublicString(branding.icon_url, defaults.branding.icon_url),
			symbol_url: normalizeOptionalPublicString(branding.symbol_url, defaults.branding.symbol_url),
			logo_url: normalizeOptionalPublicString(branding.logo_url, defaults.branding.logo_url),
			wordmark_url: normalizeOptionalPublicString(branding.wordmark_url, defaults.branding.wordmark_url),
			favicon_url: normalizeOptionalPublicString(branding.favicon_url, defaults.branding.favicon_url),
			theme_color: normalizeOptionalPublicString(branding.theme_color, defaults.branding.theme_color),
			status_page_url: normalizeOptionalPublicString(branding.status_page_url, defaults.branding.status_page_url),
			status_page_incident_history_url: normalizeOptionalPublicString(
				branding.status_page_incident_history_url,
				defaults.branding.status_page_incident_history_url,
			),
		},
		setup: {
			configured: setup.configured ?? defaults.setup.configured,
		},
		legal: {
			terms_url: normalizeOptionalPublicString(legal.terms_url, defaults.legal.terms_url),
			privacy_url: normalizeOptionalPublicString(legal.privacy_url, defaults.legal.privacy_url),
		},
		registration: {
			collect_date_of_birth: registration.collect_date_of_birth ?? defaults.registration.collect_date_of_birth,
		},
	};
}

const InstancePolicyUpdateSchema = InstanceConfigUpdateRequest.shape.policy.unwrap().unwrap();
const InstancePolicyServiceUpdateSchema = InstancePolicyUpdateSchema.shape.services.unwrap().unwrap();
const InstancePolicyPhoneGateUpdateSchema = InstancePolicyUpdateSchema.shape.deferred_phone_gate.unwrap().unwrap();
const StoredSnowflakeStringSchema = z
	.string()
	.refine((value) => value.length <= 19 && !/\D/.test(value) && SnowflakeType.safeParse(value).success);
const StoredInstancePolicySchema = z.object({
	single_community_enabled: InstancePolicyUpdateSchema.shape.single_community_enabled.default(false),
	single_community_guild_id: StoredSnowflakeStringSchema.nullable().default(null),
	direct_messages_disabled: InstancePolicyUpdateSchema.shape.direct_messages_disabled.default(false),
	direct_messages_locked: z.boolean().default(false),
	premium_mode: InstancePolicyUpdateSchema.shape.premium_mode.default('everyone'),
	gif_enabled: InstancePolicyServiceUpdateSchema.shape.gif_enabled.default(null),
	youtube_enabled: InstancePolicyServiceUpdateSchema.shape.youtube_enabled.default(null),
	bluesky_enabled: InstancePolicyServiceUpdateSchema.shape.bluesky_enabled.default(null),
	deferred_phone_gate_enabled: InstancePolicyPhoneGateUpdateSchema.shape.enabled.default(false),
	deferred_phone_gate_window_hours: InstancePolicyPhoneGateUpdateSchema.shape.window_hours.default(6),
	deferred_phone_gate_member_threshold: InstancePolicyPhoneGateUpdateSchema.shape.member_threshold.default(50),
}) satisfies z.ZodType<InstancePolicyConfig>;

function decodeInstancePolicyConfig(value: unknown): InstancePolicyConfig {
	return validateStoredConfig(StoredInstancePolicySchema, value, 'instance policy');
}

function parseStoredInstancePolicyConfig(raw: string | null): InstancePolicyConfig {
	return salvageStoredConfig(
		StoredInstancePolicySchema,
		readStoredConfigValue(raw, 'instance policy'),
		'instance policy',
	);
}

const IntegrationUpdateSchema = InstanceConfigUpdateRequest.shape.integrations.unwrap().unwrap();
const EmailIntegrationUpdateSchema = IntegrationUpdateSchema.shape.email.unwrap().unwrap();
const SmtpIntegrationUpdateSchema = EmailIntegrationUpdateSchema.shape.smtp.unwrap().unwrap();
const StoredIntegrationStringSchema = z
	.string()
	.trim()
	.nullable()
	.default(null)
	.transform((value) => value || null);
const StoredNullableBooleanSchema = z.boolean().nullable().default(null);
const StoredBlueskyKeysSchema = z
	.array(z.object({kid: z.string().trim().min(1), private_key: StoredIntegrationStringSchema}))
	.default([])
	.refine((keys) => {
		const activeIds = new Set<string>();
		for (const key of keys) {
			if (key.private_key === null) continue;
			if (activeIds.has(key.kid)) return false;
			activeIds.add(key.kid);
		}
		return true;
	});
const StoredInstanceIntegrationsSchema = z.object({
	gif: z.object({klipy_api_key: StoredIntegrationStringSchema}).prefault({}),
	youtube: z.object({api_key: StoredIntegrationStringSchema}).prefault({}),
	captcha: z
		.object({
			provider: InstanceCaptchaProviderSchema.nullable().default(null),
			hcaptcha_site_key: StoredIntegrationStringSchema,
			hcaptcha_secret_key: StoredIntegrationStringSchema,
			turnstile_site_key: StoredIntegrationStringSchema,
			turnstile_secret_key: StoredIntegrationStringSchema,
		})
		.prefault({}),
	email: z
		.object({
			enabled: StoredNullableBooleanSchema,
			provider: EmailIntegrationUpdateSchema.shape.provider.default(null),
			from_email: StoredIntegrationStringSchema,
			from_name: StoredIntegrationStringSchema,
			smtp: z
				.object({
					host: StoredIntegrationStringSchema,
					port: SmtpIntegrationUpdateSchema.shape.port.default(null),
					username: StoredIntegrationStringSchema,
					password: StoredIntegrationStringSchema,
					secure: StoredNullableBooleanSchema,
				})
				.prefault({}),
			disable_new_ip_authorization: StoredNullableBooleanSchema,
		})
		.prefault({}),
	bluesky: z
		.object({
			enabled: StoredNullableBooleanSchema,
			client_name: StoredIntegrationStringSchema,
			client_uri: StoredIntegrationStringSchema,
			logo_uri: StoredIntegrationStringSchema,
			tos_uri: StoredIntegrationStringSchema,
			policy_uri: StoredIntegrationStringSchema,
			keys: StoredBlueskyKeysSchema,
		})
		.prefault({}),
}) satisfies z.ZodType<InstanceIntegrationsConfig>;

function decodeInstanceIntegrationsConfig(value: unknown): InstanceIntegrationsConfig {
	return validateStoredConfig(StoredInstanceIntegrationsSchema, value, 'integrations');
}

function parseStoredInstanceIntegrationsConfig(raw: string | null): InstanceIntegrationsConfig {
	return salvageStoredConfig(
		StoredInstanceIntegrationsSchema,
		readStoredConfigValue(raw, 'integrations'),
		'integrations',
	);
}

function secretIsSet(value: unknown): boolean {
	return typeof value === 'string' && value.trim().length > 0;
}

const AttachmentDecayUpdateSchema = InstanceConfigUpdateRequest.shape.media
	.unwrap()
	.unwrap()
	.shape.attachment_decay.unwrap()
	.unwrap();
const StoredAttachmentDecaySchema = z
	.object({
		enabled: AttachmentDecayUpdateSchema.shape.enabled.default(null),
		min_size_mb: AttachmentDecayUpdateSchema.shape.min_size_mb.default(null),
		max_size_mb: AttachmentDecayUpdateSchema.shape.max_size_mb.default(null),
		max_eligible_size_mb: AttachmentDecayUpdateSchema.shape.max_eligible_size_mb.default(null),
		min_lifetime_days: AttachmentDecayUpdateSchema.shape.min_lifetime_days.default(null),
		max_lifetime_days: AttachmentDecayUpdateSchema.shape.max_lifetime_days.default(null),
		curve: AttachmentDecayUpdateSchema.shape.curve.default(null),
		renew_threshold_days: AttachmentDecayUpdateSchema.shape.renew_threshold_days.default(null),
		renew_window_days: AttachmentDecayUpdateSchema.shape.renew_window_days.default(null),
	})
	.transform(normalizeAttachmentDecayBounds)
	.refine(
		(value) => {
			const minSizeMb = value.min_size_mb ?? DEFAULT_DECAY_CONSTANTS.MIN_MB;
			const maxSizeMb = computeAttachmentDecayMaxSize(minSizeMb, value.max_size_mb ?? DEFAULT_DECAY_CONSTANTS.MAX_MB);
			return Number.isFinite(maxSizeMb) && maxSizeMb > minSizeMb;
		},
		{path: ['min_size_mb']},
	);
const StoredInstanceMediaSchema = z.object({
	attachment_decay: StoredAttachmentDecaySchema.prefault({}),
}) satisfies z.ZodType<InstanceMediaConfig>;

function normalizeAttachmentDecayBounds(value: InstanceAttachmentDecayConfig): InstanceAttachmentDecayConfig {
	const minSizeMb = value.min_size_mb;
	let maxSizeMb = value.max_size_mb;
	let maxEligibleSizeMb = value.max_eligible_size_mb;
	const minLifetimeDays = value.min_lifetime_days;
	let maxLifetimeDays = value.max_lifetime_days;
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
		...value,
		max_size_mb: maxSizeMb,
		max_eligible_size_mb: maxEligibleSizeMb,
		max_lifetime_days: maxLifetimeDays,
	};
}

function computeAttachmentDecayMaxSize(minSizeMb: number, configuredMaxSizeMb: number): number {
	if (configuredMaxSizeMb > minSizeMb) return configuredMaxSizeMb;
	const incrementedMin = minSizeMb + 1;
	const largerMin =
		incrementedMin > minSizeMb ? incrementedMin : Math.min(Number.MAX_VALUE, minSizeMb + minSizeMb * Number.EPSILON);
	return Math.max(DEFAULT_DECAY_CONSTANTS.MAX_MB, largerMin);
}

function decodeInstanceMediaConfig(value: unknown): InstanceMediaConfig {
	return validateStoredConfig(StoredInstanceMediaSchema, value, 'media');
}

function parseStoredInstanceMediaConfig(raw: string | null): InstanceMediaConfig {
	return decodeInstanceMediaConfig(parseStoredConfigValue(raw, 'media'));
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

const StoredRegistrationConfigSchema = InstanceRegistrationSchema.extend({
	mode: InstanceRegistrationSchema.shape.mode.default(DEFAULT_REGISTRATION_CONFIG.mode),
	admin_registration_urls_enabled: InstanceRegistrationSchema.shape.admin_registration_urls_enabled.default(
		DEFAULT_REGISTRATION_CONFIG.admin_registration_urls_enabled,
	),
});

function decodeRegistrationConfig(value: unknown): InstanceRegistrationConfig {
	const input =
		isJsonRecord(value) &&
		!Object.hasOwn(value, 'admin_registration_urls_enabled') &&
		Object.hasOwn(value, 'adminRegistrationUrlsEnabled')
			? {...value, admin_registration_urls_enabled: value.adminRegistrationUrlsEnabled}
			: value;
	return validateStoredConfig(StoredRegistrationConfigSchema, input, 'registration');
}

function parseStoredRegistrationConfig(raw: string | null): InstanceRegistrationConfig {
	return decodeRegistrationConfig(parseStoredConfigValue(raw, 'registration'));
}

const RegistrationUrlSchema = InstanceConfigResponse.shape.registration.shape.urls.element;
const PendingRegistrationSchema = InstanceConfigResponse.shape.registration.shape.pending_registrations.element;
const StoredRegistrationTimestampSchema = z.iso
	.datetime({offset: true})
	.refine((value) => value === value.trim() && Number.isFinite(Date.parse(value)))
	.transform((value): string => new Date(value).toISOString())
	.pipe(RegistrationUrlSchema.shape.created_at);
const StoredNullableStringSchema = z
	.string()
	.nullable()
	.default(null)
	.transform((value) => (value === null || value.trim().length === 0 ? null : value));
const StoredRegistrationUrlIdSchema = z
	.string()
	.min(1)
	.max(128)
	.refine((value) => normalizeString(value) === value);
const StoredRegistrationUrlSchema = RegistrationUrlSchema.extend({
	id: StoredRegistrationUrlIdSchema,
	label: StoredNullableStringSchema,
	code_hash: z
		.string()
		.length(64)
		.regex(/^[a-f0-9]{64}$/),
	created_by_user_id: StoredSnowflakeStringSchema,
	created_at: StoredRegistrationTimestampSchema,
	expires_at: StoredRegistrationTimestampSchema.nullable().default(null),
	max_uses: RegistrationUrlSchema.shape.max_uses.default(null),
	use_count: RegistrationUrlSchema.shape.use_count.default(0),
	revoked_at: StoredRegistrationTimestampSchema.nullable().default(null),
	approval_required: RegistrationUrlSchema.shape.approval_required.default(false),
	last_used_at: StoredRegistrationTimestampSchema.nullable().default(null),
	last_used_by_user_id: StoredNullableStringSchema.pipe(StoredSnowflakeStringSchema.nullable()),
}) satisfies z.ZodType<InstanceRegistrationUrl>;
const StoredPendingRegistrationSchema = PendingRegistrationSchema.extend({
	user_id: StoredSnowflakeStringSchema,
	discriminator: PendingRegistrationSchema.shape.discriminator.default(0),
	global_name: StoredNullableStringSchema,
	email: StoredNullableStringSchema,
	requested_at: StoredRegistrationTimestampSchema,
	registration_url_id: StoredNullableStringSchema.pipe(StoredRegistrationUrlIdSchema.nullable()),
	client_ip: StoredNullableStringSchema,
}) satisfies z.ZodType<InstancePendingRegistration>;

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

interface InstanceSsoFlags {
	enabled: boolean;
	enforced: boolean;
	autoProvision: boolean;
}

export interface InstanceSsoConfig extends InstanceSsoFlags {
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
	redirectUri: string | null;
}

const MAX_SSO_ALLOWED_DOMAINS = 100;

function readStoredSsoBoolean(
	configs: ReadonlyMap<string, string>,
	key: 'sso_enabled' | 'sso_enforced' | 'sso_auto_provision',
	fallback: boolean,
	options?: {invalidFallback?: boolean; log?: boolean},
): boolean {
	const value = configs.get(key);
	if (value === undefined) return fallback;
	if (value === 'true') return true;
	if (value === 'false') return false;
	if (options?.log) {
		Logger.warn({key}, 'Invalid stored SSO flag, falling back to its default');
	}
	return options?.invalidFallback ?? fallback;
}

function readStoredSsoFlags(configs: ReadonlyMap<string, string>, log = false): InstanceSsoFlags {
	const enabled = readStoredSsoBoolean(configs, 'sso_enabled', false, {log});
	return {
		enabled,
		enforced: readStoredSsoBoolean(configs, 'sso_enforced', enabled, {invalidFallback: false, log}),
		autoProvision: readStoredSsoBoolean(configs, 'sso_auto_provision', true, {log}),
	};
}

function parseStoredSsoAllowedEmailDomains(raw: string | undefined, log = false): Array<string> {
	if (raw === undefined || raw.trim().length === 0) return [];
	let value: unknown;
	try {
		value = JSON.parse(raw);
	} catch {
		value = null;
	}
	const entries: ReadonlyArray<unknown> = Array.isArray(value) ? value : raw.split(',');
	const domains = new Set<string>();
	const unusable = new Set<string>();
	for (const entry of entries) {
		try {
			for (const domain of normalizeSsoAllowedEmailDomains([entry])) {
				domains.add(domain);
			}
		} catch {
			const text = String(entry).trim();
			if (text.length > 0) unusable.add(text);
		}
	}
	if (domains.size === 0 && unusable.size > 0) {
		if (log) {
			Logger.error(
				{unusable: unusable.size},
				'Every stored SSO allowed email domain is invalid, keeping them so the allowlist still matches nothing',
			);
		}
		return Array.from(unusable).slice(0, MAX_SSO_ALLOWED_DOMAINS);
	}
	if (log && unusable.size > 0) {
		Logger.warn({dropped: unusable.size}, 'Dropped invalid stored SSO allowed email domains');
	}
	if (log && domains.size > MAX_SSO_ALLOWED_DOMAINS) {
		Logger.warn(
			{dropped: domains.size - MAX_SSO_ALLOWED_DOMAINS},
			'Truncated the stored SSO allowed email domain list to its maximum length',
		);
	}
	return Array.from(domains).slice(0, MAX_SSO_ALLOWED_DOMAINS);
}

function readStoredSsoConfig(
	configs: ReadonlyMap<string, string>,
	options?: {includeSecret?: boolean},
): InstanceSsoConfig {
	const flags = readStoredSsoFlags(configs);
	const read = (key: string): string | null => {
		const v = configs.get(key);
		if (!v) return null;
		const trimmed = v.trim();
		return trimmed.length === 0 ? null : trimmed;
	};
	const allowedDomains = parseStoredSsoAllowedEmailDomains(configs.get('sso_allowed_domains'));
	const clientSecret = read('sso_client_secret');
	return {
		...flags,
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
		redirectUri: null,
	};
}

interface SsoRowWrite {
	key: string;
	value: string | undefined;
	unset: string;
}

function ssoRow<T>(key: string, value: T | undefined, current: T, format: (value: T) => string): SsoRowWrite {
	return {key, value: value === undefined ? undefined : format(value), unset: format(current)};
}

function nextSsoRowValue(row: SsoRowWrite, raw: string | null): string | null {
	const value = row.value ?? raw ?? row.unset;
	return value === raw ? null : value;
}

function formatSsoBoolean(value: boolean): string {
	return value ? 'true' : 'false';
}

function formatSsoString(value: string | null): string {
	return value ?? '';
}

function formatSsoDomains(value: Array<string>): string {
	return JSON.stringify(value);
}

function normalizeSsoAllowedEmailDomainsForWrite(domains: Array<string>, enabled: boolean): Array<string> {
	try {
		return normalizeSsoAllowedEmailDomains(domains);
	} catch (error) {
		if (enabled) {
			throw error;
		}
		Logger.warn({error}, 'Clearing invalid SSO allowed domain config while SSO is disabled');
		return [];
	}
}

export class InstanceConfigRepository {
	private readonly kvClient: IKVProvider | null;
	private configCache: InstanceConfigCache;
	private effectiveBlueskyConfig: BlueskyOAuthConfig | null = null;
	private effectiveBlueskyConfigSource: string | null = null;

	constructor(kvClient: IKVProvider | null = null) {
		this.kvClient = kvClient;
		this.configCache = this.createConfigCache();
	}

	private createConfigCache(previousShutdown?: Promise<void>): InstanceConfigCache {
		return new InstanceConfigCache(
			{
				provider: this.kvClient,
				channel: INSTANCE_CONFIG_REFRESH_CHANNEL,
				load: () => this.fetchAllConfigsFromDatabase(),
				onRefresh: (snapshot) => this.syncConfigCaches(snapshot),
			},
			previousShutdown,
		);
	}

	async initialize(): Promise<void> {
		const cache = this.configCache;
		const snapshot = await cache.getSnapshot();
		cache.assertActive();
		if (snapshot !== null) return;
		const configs = await this.fetchAllConfigsFromDatabase();
		cache.assertActive();
		this.syncConfigCaches(configs);
	}

	async getConfig(key: string): Promise<string | null> {
		const cache = this.configCache;
		const snapshot = await cache.getSnapshot();
		cache.assertActive();
		return snapshot === null ? this.fetchConfigFromDatabase(key) : (snapshot.get(key) ?? null);
	}

	async getAllConfigs(): Promise<Map<string, string>> {
		const cache = this.configCache;
		const snapshot = await cache.getSnapshot();
		cache.assertActive();
		return snapshot === null ? this.fetchAllConfigsFromDatabase() : new Map(snapshot);
	}

	setConfig(key: string, value: string): Promise<void> {
		return this.setConfigs([[key, value]]);
	}

	private async setConfigs(entries: Array<[string, string]>): Promise<void> {
		if (entries.length === 0) return;
		const cache = this.configCache;
		cache.assertActive();
		const results = await Promise.allSettled(
			entries.map(async ([key, value]) => {
				await this.writeConfig(key, value);
				cache.update(key, value);
			}),
		);
		const errors: Array<unknown> = results.flatMap((result) => (result.status === 'rejected' ? [result.reason] : []));
		if (results.some((result) => result.status === 'fulfilled')) {
			try {
				await this.publishRefresh(cache.sourceId);
			} catch (error) {
				errors.push(error);
			}
		}
		if (errors.length === 1) throw errors[0];
		if (errors.length > 1) throw new AggregateError(errors, 'Failed to write or publish instance config');
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

	private async updateStoredConfig<T>(key: string, next: (raw: string | null) => T): Promise<T> {
		const cache = this.configCache;
		const {result} = await this.compareAndSetStoredValue(cache, key, (raw) => {
			const config = next(raw);
			return {value: JSON.stringify(config), result: config};
		});
		await this.publishRefresh(cache.sourceId);
		return result;
	}

	private async compareAndSetStoredValue<T>(
		cache: InstanceConfigCache,
		key: string,
		next: (raw: string | null) => StoredValueUpdate<T>,
	): Promise<{result: T; written: boolean}> {
		await cache.getSnapshot();
		for (let attempt = 0; attempt < INSTANCE_CONFIG_WRITE_ATTEMPTS; attempt++) {
			cache.assertActive();
			const current = await this.fetchConfigForWrite(key);
			cache.assertActive();
			const {value, result} = next(current);
			if (value === null) return {result, written: false};
			if (await executeConditional(this.compareAndSetConfig(key, current, value))) {
				cache.update(key, value);
				return {result, written: true};
			}
		}
		Logger.error(
			{key, attempts: INSTANCE_CONFIG_WRITE_ATTEMPTS},
			'Instance config write lost to a concurrent write on every attempt',
		);
		throw new InstanceConfigWriteConflictError(key);
	}

	private compareAndSetConfig(key: string, current: string | null, value: string): PreparedQuery {
		const updatedAt = new Date();
		if (current === null) {
			return InstanceConfiguration.insertIfNotExists({key, value, updated_at: updatedAt});
		}
		return InstanceConfiguration.conditionalPatchByPk(
			{key},
			{value: Db.set(value), updated_at: Db.set(updatedAt)},
			{value: current},
		);
	}

	private async fetchConfigForWrite(key: string): Promise<string | null> {
		const [row] = await fetchMany<InstanceConfigurationRow>(FETCH_CONFIG_QUERY, {key}, {consistency: 'serial'});
		return row?.value ?? null;
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

	private syncConfigCaches(snapshot: ReadonlyMap<string, string>): void {
		checkStoredConfig('gateway rollout', () =>
			parseStoredGatewayRolloutConfig(snapshot.get(GATEWAY_ROLLOUT_CONFIG_KEY) ?? null),
		);
		parseStoredVoiceNoiseSuppressionConfig(snapshot.get(VOICE_NOISE_SUPPRESSION_CONFIG_KEY) ?? null);
		parseStoredScreenShareDeliveryConfig(snapshot.get(SCREEN_SHARE_DELIVERY_CONFIG_KEY) ?? null);
		parseStoredPushServiceDeliveryConfig(snapshot.get(PUSH_SERVICE_DELIVERY_CONFIG_KEY) ?? null);
		parseStoredExperimentDeliveryConfig(snapshot.get(EXPERIMENT_DELIVERY_CONFIG_KEY) ?? null);
		const policy = parseStoredInstancePolicyConfig(snapshot.get(INSTANCE_POLICY_CONFIG_KEY) ?? null);
		checkStoredConfig('registration', () =>
			parseStoredRegistrationConfig(snapshot.get(REGISTRATION_CONFIG_KEY) ?? null),
		);
		checkStoredConfig('registration URLs', () =>
			parseStoredCollection(
				StoredRegistrationUrlSchema,
				snapshot.get(REGISTRATION_URLS_KEY) ?? null,
				'registration URLs',
			),
		);
		checkStoredConfig('pending registrations', () =>
			parseStoredCollection(
				StoredPendingRegistrationSchema,
				snapshot.get(REGISTRATION_PENDING_APPROVALS_KEY) ?? null,
				'pending registrations',
			),
		);
		checkStoredConfig('SSO flags', () => readStoredSsoFlags(snapshot, true));
		checkStoredConfig('SSO allowed domains', () =>
			parseStoredSsoAllowedEmailDomains(snapshot.get('sso_allowed_domains'), true),
		);
		checkStoredConfig('integrations', () =>
			parseStoredInstanceIntegrationsConfig(snapshot.get(INSTANCE_INTEGRATIONS_CONFIG_KEY) ?? null),
		);
		checkStoredConfig('media', () => parseStoredInstanceMediaConfig(snapshot.get(INSTANCE_MEDIA_CONFIG_KEY) ?? null));
		const appPublic = parseStoredAppPublicConfig(snapshot.get(APP_PUBLIC_CONFIG_KEY) ?? null);
		setCachedDeferredPhoneGateEnabled(resolveDeferredPhoneGateEnabled(policy));
		setCachedDateOfBirthCollection(appPublic.registration.collect_date_of_birth);
	}

	private async publishRefresh(sourceId: string): Promise<void> {
		if (!this.kvClient) {
			return;
		}
		await this.kvClient.publish(
			INSTANCE_CONFIG_REFRESH_CHANNEL,
			JSON.stringify({source_id: sourceId, type: 'refresh'}),
		);
	}

	shutdown(): Promise<void> {
		return this.configCache.shutdown();
	}

	clearCacheForTesting(): void {
		const shutdown = this.shutdown();
		this.configCache = this.createConfigCache(shutdown);
		void shutdown.catch((error) => {
			Logger.error({error}, 'Failed to clear instance config cache');
		});
	}

	async isAdminBootstrapped(): Promise<boolean> {
		return (await this.getConfig(ADMIN_BOOTSTRAP_KEY)) === 'true';
	}

	async markAdminBootstrapped(): Promise<void> {
		await this.setConfig(ADMIN_BOOTSTRAP_KEY, 'true');
	}

	async getGatewayRolloutConfig(): Promise<GatewayRolloutConfig> {
		const raw = await this.getConfig(GATEWAY_ROLLOUT_CONFIG_KEY);
		return parseStoredGatewayRolloutConfig(raw);
	}

	updateGatewayRolloutConfig(
		update: (current: GatewayRolloutConfig) => GatewayRolloutConfig,
	): Promise<GatewayRolloutConfig> {
		return this.updateStoredConfig(GATEWAY_ROLLOUT_CONFIG_KEY, (raw) =>
			decodeGatewayRolloutConfig(update(parseStoredGatewayRolloutConfig(raw))),
		);
	}

	async getVoiceNoiseSuppressionConfig(): Promise<VoiceNoiseSuppressionConfig> {
		const raw = await this.getConfig(VOICE_NOISE_SUPPRESSION_CONFIG_KEY);
		return parseStoredVoiceNoiseSuppressionConfig(raw);
	}

	async setVoiceNoiseSuppressionConfig(config: VoiceNoiseSuppressionConfig): Promise<void> {
		await this.updateVoiceNoiseSuppressionConfig(() => config);
	}

	updateVoiceNoiseSuppressionConfig(
		update: (current: VoiceNoiseSuppressionConfig) => VoiceNoiseSuppressionConfig,
	): Promise<VoiceNoiseSuppressionConfig> {
		return this.updateStoredConfig(VOICE_NOISE_SUPPRESSION_CONFIG_KEY, (raw) =>
			validateStoredConfig(
				VoiceNoiseSuppressionConfigSchema,
				update(parseStoredVoiceNoiseSuppressionConfig(raw)),
				'voice noise suppression',
			),
		);
	}

	async getScreenShareDeliveryConfig(): Promise<ScreenShareDeliveryConfig> {
		const raw = await this.getConfig(SCREEN_SHARE_DELIVERY_CONFIG_KEY);
		return parseStoredScreenShareDeliveryConfig(raw);
	}

	async setScreenShareDeliveryConfig(config: ScreenShareDeliveryConfig): Promise<void> {
		await this.updateScreenShareDeliveryConfig(() => config);
	}

	updateScreenShareDeliveryConfig(
		update: (current: ScreenShareDeliveryConfig) => ScreenShareDeliveryConfig,
	): Promise<ScreenShareDeliveryConfig> {
		return this.updateStoredConfig(SCREEN_SHARE_DELIVERY_CONFIG_KEY, (raw) =>
			validateStoredConfig(
				ScreenShareDeliveryConfigSchema,
				update(parseStoredScreenShareDeliveryConfig(raw)),
				'screen share delivery',
			),
		);
	}

	async getPushServiceDeliveryConfig(): Promise<PushServiceDeliveryConfig> {
		const raw = await this.getConfig(PUSH_SERVICE_DELIVERY_CONFIG_KEY);
		return parseStoredPushServiceDeliveryConfig(raw);
	}

	updatePushServiceDeliveryConfig(
		update: (current: PushServiceDeliveryConfig) => PushServiceDeliveryConfig,
	): Promise<PushServiceDeliveryConfig> {
		return this.updateStoredConfig(PUSH_SERVICE_DELIVERY_CONFIG_KEY, (raw) =>
			validateStoredConfig(
				PushServiceDeliveryConfigSchema,
				update(parseStoredPushServiceDeliveryConfig(raw)),
				'push service delivery',
			),
		);
	}

	async getExperimentDeliveryConfig(): Promise<ExperimentDeliveryConfig> {
		const raw = await this.getConfig(EXPERIMENT_DELIVERY_CONFIG_KEY);
		return parseStoredExperimentDeliveryConfig(raw);
	}

	async setExperimentDeliveryConfig(config: ExperimentDeliveryConfig): Promise<void> {
		await this.updateExperimentDeliveryConfig(() => config);
	}

	updateExperimentDeliveryConfig(
		update: (current: ExperimentDeliveryConfig) => ExperimentDeliveryConfig,
	): Promise<ExperimentDeliveryConfig> {
		return this.updateStoredConfig(EXPERIMENT_DELIVERY_CONFIG_KEY, (raw) =>
			validateStoredConfig(
				ExperimentDeliveryConfigSchema,
				update(parseStoredExperimentDeliveryConfig(raw)),
				'experiment delivery',
			),
		);
	}

	async readLimitConfigInputs(): Promise<LimitConfigInputs> {
		const cache = this.configCache;
		cache.assertActive();
		const rows = await fetchMany<InstanceConfigurationRow>(FETCH_LIMIT_CONFIG_INPUTS_QUERY, {
			keys: [LIMIT_CONFIG_KEY, INSTANCE_POLICY_CONFIG_KEY],
		});
		cache.assertActive();
		const values = new Map(rows.map((row) => [row.key, row.value]));
		const policyRaw = values.get(INSTANCE_POLICY_CONFIG_KEY) ?? null;
		const policy = parseStoredInstancePolicyConfig(policyRaw);
		return {
			config: parseStoredLimitConfig(values.get(LIMIT_CONFIG_KEY) ?? null),
			premiumMode: policy.premium_mode,
		};
	}

	async setLimitConfig(config: LimitConfigSnapshot): Promise<void> {
		await this.setConfig(LIMIT_CONFIG_KEY, JSON.stringify(config));
	}

	async getAppPublicConfig(): Promise<InstanceAppPublicConfig> {
		const raw = await this.getConfig(APP_PUBLIC_CONFIG_KEY);
		const config = parseStoredAppPublicConfig(raw);
		setCachedDateOfBirthCollection(config.registration.collect_date_of_birth);
		return config;
	}

	async setAppPublicConfig(config: {
		branding?: Partial<InstanceBranding>;
		setup?: Partial<InstanceAppPublicConfig['setup']>;
		legal?: Partial<InstanceAppPublicConfig['legal']>;
		registration?: Partial<InstanceAppPublicConfig['registration']>;
	}): Promise<InstanceAppPublicConfig> {
		const next = await this.updateStoredConfig(APP_PUBLIC_CONFIG_KEY, (raw) => {
			const current = parseStoredAppPublicConfig(raw);
			return decodeAppPublicConfig({
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
		});
		setCachedDateOfBirthCollection(next.registration.collect_date_of_birth);
		return next;
	}

	async getInstancePolicyConfig(): Promise<InstancePolicyConfig> {
		const raw = await this.getConfig(INSTANCE_POLICY_CONFIG_KEY);
		const policy = parseStoredInstancePolicyConfig(raw);
		setCachedDeferredPhoneGateEnabled(resolveDeferredPhoneGateEnabled(policy));
		return policy;
	}

	async readStoredInstancePolicyConfig(): Promise<InstancePolicyConfig> {
		const cache = this.configCache;
		cache.assertActive();
		const raw = await this.fetchConfigFromDatabase(INSTANCE_POLICY_CONFIG_KEY);
		cache.assertActive();
		return parseStoredInstancePolicyConfig(raw);
	}

	setInstancePolicyConfig(config: Partial<InstancePolicyConfig>): Promise<InstancePolicyConfig> {
		return this.updateInstancePolicyConfig(() => config);
	}

	async updateInstancePolicyConfig(
		plan: (current: InstancePolicyConfig) => Partial<InstancePolicyConfig>,
	): Promise<InstancePolicyConfig> {
		const cache = this.configCache;
		const {result: next, written} = await this.compareAndSetStoredValue(cache, INSTANCE_POLICY_CONFIG_KEY, (raw) => {
			const current = parseStoredInstancePolicyConfig(raw);
			const patch = plan(current);
			const config = decodeInstancePolicyConfig({...current, ...patch});
			return {value: Object.keys(patch).length === 0 ? null : JSON.stringify(config), result: config};
		});
		if (written) await this.publishRefresh(cache.sourceId);
		setCachedDeferredPhoneGateEnabled(resolveDeferredPhoneGateEnabled(next));
		return next;
	}

	async getInstanceIntegrationsConfig(): Promise<InstanceIntegrationsConfig> {
		const raw = await this.getConfig(INSTANCE_INTEGRATIONS_CONFIG_KEY);
		return parseStoredInstanceIntegrationsConfig(raw);
	}

	setInstanceIntegrationsConfig(config: InstanceIntegrationsConfigPatch): Promise<InstanceIntegrationsConfig> {
		return this.updateStoredConfig(INSTANCE_INTEGRATIONS_CONFIG_KEY, (raw) => {
			const current = parseStoredInstanceIntegrationsConfig(raw);
			return decodeInstanceIntegrationsConfig({
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
		});
	}

	async getInstanceMediaConfig(): Promise<InstanceMediaConfig> {
		const raw = await this.getConfig(INSTANCE_MEDIA_CONFIG_KEY);
		return parseStoredInstanceMediaConfig(raw);
	}

	setInstanceMediaConfig(config: InstanceMediaConfigPatch): Promise<InstanceMediaConfig> {
		return this.updateStoredConfig(INSTANCE_MEDIA_CONFIG_KEY, (raw) => {
			const current = parseStoredInstanceMediaConfig(raw);
			return decodeInstanceMediaConfig({
				attachment_decay: {
					...current.attachment_decay,
					...(config.attachment_decay ?? {}),
				},
			});
		});
	}

	async getEffectiveAttachmentDecayConfig(): Promise<InstanceAttachmentDecayEffectiveConfig> {
		const media = await this.getInstanceMediaConfig();
		const attachmentDecay = media.attachment_decay;
		const minSizeMb = attachmentDecay.min_size_mb ?? DEFAULT_DECAY_CONSTANTS.MIN_MB;
		const configuredMaxSizeMb = attachmentDecay.max_size_mb ?? DEFAULT_DECAY_CONSTANTS.MAX_MB;
		const maxSizeMb = computeAttachmentDecayMaxSize(minSizeMb, configuredMaxSizeMb);
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
		const klipyApiKey = integrations.gif.klipy_api_key ?? normalizeOptionalString(Config.klipy.apiKey);
		return {
			klipy_api_key: klipyApiKey,
			active_api_key: klipyApiKey,
			available: Boolean(klipyApiKey),
		};
	}

	async getEffectiveYoutubeApiKey(): Promise<string | null> {
		const integrations = await this.getInstanceIntegrationsConfig();
		return integrations.youtube.api_key ?? normalizeOptionalString(Config.youtube.apiKey);
	}

	async getEffectiveCaptchaConfig(): Promise<InstanceCaptchaEffectiveConfig> {
		const integrations = await this.getInstanceIntegrationsConfig();
		const provider = integrations.captcha.provider ?? (Config.captcha.enabled ? Config.captcha.provider : 'none');
		const hcaptchaSiteKey =
			integrations.captcha.hcaptcha_site_key ?? normalizeOptionalString(Config.captcha.hcaptcha?.siteKey);
		const hcaptchaSecretKey =
			integrations.captcha.hcaptcha_secret_key ?? normalizeOptionalString(Config.captcha.hcaptcha?.secretKey);
		const turnstileSiteKey =
			integrations.captcha.turnstile_site_key ?? normalizeOptionalString(Config.captcha.turnstile?.siteKey);
		const turnstileSecretKey =
			integrations.captcha.turnstile_secret_key ?? normalizeOptionalString(Config.captcha.turnstile?.secretKey);
		const providerReady =
			provider === 'hcaptcha'
				? Boolean(hcaptchaSiteKey && hcaptchaSecretKey)
				: provider === 'turnstile'
					? Boolean(turnstileSiteKey && turnstileSecretKey)
					: false;
		return {
			enabled: providerReady,
			provider: providerReady ? provider : 'none',
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
		const raw = await this.getConfig(INSTANCE_INTEGRATIONS_CONFIG_KEY);
		const memoized = this.effectiveBlueskyConfig;
		if (memoized && this.effectiveBlueskyConfigSource === raw) {
			return memoized;
		}
		const integrations = parseStoredInstanceIntegrationsConfig(raw);
		const runtimeKeys = integrations.bluesky.keys.flatMap((key): Array<BlueskyOAuthKeyConfig> => {
			if (!key.private_key) return [];
			return [{kid: key.kid, private_key: key.private_key}];
		});
		const keys = runtimeKeys.length > 0 ? runtimeKeys : Config.auth.bluesky.keys;
		const enabled = (integrations.bluesky.enabled ?? Config.auth.bluesky.enabled) && keys.length > 0;
		const effective: BlueskyOAuthConfig = {
			...Config.auth.bluesky,
			enabled,
			client_name: integrations.bluesky.client_name ?? Config.auth.bluesky.client_name,
			client_uri: integrations.bluesky.client_uri ?? Config.auth.bluesky.client_uri,
			logo_uri: integrations.bluesky.logo_uri ?? Config.auth.bluesky.logo_uri,
			tos_uri: integrations.bluesky.tos_uri ?? Config.auth.bluesky.tos_uri,
			policy_uri: integrations.bluesky.policy_uri ?? Config.auth.bluesky.policy_uri,
			keys,
		};
		this.effectiveBlueskyConfigSource = raw;
		this.effectiveBlueskyConfig = effective;
		return effective;
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
				disable_new_ip_authorization: integrations.email.disable_new_ip_authorization ?? false,
				effective_disable_new_ip_authorization: integrations.email.disable_new_ip_authorization || !email.enabled,
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

	async getInstanceCommunityPublicConfig(): Promise<InstanceCommunity> {
		const policy = await this.getInstancePolicyConfig();
		return {
			single_community: policy.single_community_enabled,
			single_community_guild_id: policy.single_community_enabled ? policy.single_community_guild_id : null,
			direct_messages_disabled: policy.direct_messages_disabled,
		};
	}

	async getResolvedServicesConfig(): Promise<InstanceServices> {
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
		return parseStoredRegistrationConfig(raw);
	}

	setRegistrationConfig(config: Partial<InstanceRegistrationConfig>): Promise<InstanceRegistrationConfig> {
		return this.updateStoredConfig(REGISTRATION_CONFIG_KEY, (raw) => {
			const current = parseStoredRegistrationConfig(raw);
			return decodeRegistrationConfig({
				mode: config.mode ?? current.mode,
				admin_registration_urls_enabled:
					config.admin_registration_urls_enabled ?? current.admin_registration_urls_enabled,
			});
		});
	}

	async getRegistrationPublicConfig(): Promise<InstanceRegistrationConfig> {
		return this.getRegistrationConfig();
	}

	async getRegistrationUrls(): Promise<Array<InstanceRegistrationUrl>> {
		const raw = await this.getConfig(REGISTRATION_URLS_KEY);
		return parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs');
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
		await this.updateStoredConfig(REGISTRATION_URLS_KEY, (raw) =>
			validateStoredCollection(
				StoredRegistrationUrlSchema,
				[registrationUrl, ...parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs')],
				'registration URLs',
			),
		);
		return {registrationUrl: redactRegistrationUrl(registrationUrl), code};
	}

	async revokeRegistrationUrl(id: string): Promise<void> {
		const now = new Date().toISOString();
		await this.updateStoredConfig(REGISTRATION_URLS_KEY, (raw) =>
			parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs').map((registrationUrl) =>
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
		return (
			(await this.fetchRegistrationUrlDefinitions()).find(
				(registrationUrl) =>
					(registrationUrl.id === normalizedCode || registrationUrl.code_hash === hash) &&
					isRegistrationUrlUsable(registrationUrl, now),
			) ?? null
		);
	}

	async claimRegistrationUrlUse(registrationUrlId: string, userId: string): Promise<RegistrationUrlClaim | null> {
		const cache = this.configCache;
		let claimed: {result: RegistrationUrlClaim | null; written: boolean};
		try {
			claimed = await this.compareAndSetStoredValue<RegistrationUrlClaim | null>(
				cache,
				REGISTRATION_URLS_KEY,
				(raw) => {
					const registrationUrls = parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs');
					const now = new Date();
					const claimable = registrationUrls.find(
						(registrationUrl) =>
							registrationUrl.id === registrationUrlId && isRegistrationUrlUsable(registrationUrl, now),
					);
					if (!claimable) return {value: null, result: null};
					const next = registrationUrls.map((registrationUrl) =>
						registrationUrl === claimable
							? {
									...registrationUrl,
									use_count: registrationUrl.use_count + 1,
									last_used_at: now.toISOString(),
									last_used_by_user_id: userId,
								}
							: registrationUrl,
					);
					return {
						value: JSON.stringify(validateStoredCollection(StoredRegistrationUrlSchema, next, 'registration URLs')),
						result: {registration_url_id: registrationUrlId, user_id: userId},
					};
				},
			);
		} catch (error) {
			if (error instanceof InstanceConfigWriteConflictError) throw new ServiceUnavailableError();
			throw error;
		}
		if (claimed.written) await this.publishRefresh(cache.sourceId);
		return claimed.result;
	}

	async releaseRegistrationUrlUse(claim: RegistrationUrlClaim): Promise<void> {
		const cache = this.configCache;
		try {
			const {written} = await this.compareAndSetStoredValue<null>(cache, REGISTRATION_URLS_KEY, (raw) => {
				const registrationUrls = parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs');
				const released = registrationUrls.find(
					(registrationUrl) => registrationUrl.id === claim.registration_url_id && registrationUrl.use_count > 0,
				);
				if (!released) return {value: null, result: null};
				const next = registrationUrls.map((registrationUrl) =>
					registrationUrl === released
						? {...registrationUrl, use_count: registrationUrl.use_count - 1}
						: registrationUrl,
				);
				return {value: JSON.stringify(next), result: null};
			});
			if (written) await this.publishRefresh(cache.sourceId);
		} catch (error) {
			Logger.warn(
				{registrationUrlId: claim.registration_url_id, userId: claim.user_id, error},
				'Releasing a registration URL use failed',
			);
		}
	}

	async getPendingRegistrations(): Promise<Array<InstancePendingRegistration>> {
		const raw = await this.getConfig(REGISTRATION_PENDING_APPROVALS_KEY);
		return parseStoredCollection(StoredPendingRegistrationSchema, raw, 'pending registrations').toSorted(
			(a, b) => Date.parse(a.requested_at) - Date.parse(b.requested_at),
		);
	}

	async addPendingRegistration(pendingRegistration: InstancePendingRegistration): Promise<void> {
		await this.updateStoredConfig(REGISTRATION_PENDING_APPROVALS_KEY, (raw) =>
			validateStoredCollection(
				StoredPendingRegistrationSchema,
				[
					pendingRegistration,
					...parseStoredCollection(StoredPendingRegistrationSchema, raw, 'pending registrations').filter(
						(entry) => entry.user_id !== pendingRegistration.user_id,
					),
				],
				'pending registrations',
			),
		);
	}

	async removePendingRegistration(userId: string): Promise<void> {
		await this.updateStoredConfig(REGISTRATION_PENDING_APPROVALS_KEY, (raw) =>
			parseStoredCollection(StoredPendingRegistrationSchema, raw, 'pending registrations').filter(
				(entry) => entry.user_id !== userId,
			),
		);
	}

	async getSsoConfig(options?: {includeSecret?: boolean}): Promise<InstanceSsoConfig> {
		return readStoredSsoConfig(await this.getAllConfigs(), options);
	}

	async setSsoConfig(config: Partial<InstanceSsoConfig>): Promise<InstanceSsoConfig> {
		const configs = await this.getAllConfigs();
		const current = readStoredSsoConfig(configs, {includeSecret: true});
		const enabled = config.enabled ?? current.enabled;
		const allowedEmailDomains =
			config.allowedEmailDomains === undefined
				? undefined
				: normalizeSsoAllowedEmailDomainsForWrite(config.allowedEmailDomains, enabled);
		const rows: Array<SsoRowWrite> = [
			ssoRow('sso_enabled', config.enabled, current.enabled, formatSsoBoolean),
			ssoRow('sso_enforced', config.enforced, current.enforced, formatSsoBoolean),
			ssoRow('sso_display_name', config.displayName, current.displayName, formatSsoString),
			ssoRow('sso_issuer', config.issuer, current.issuer, formatSsoString),
			ssoRow('sso_authorization_url', config.authorizationUrl, current.authorizationUrl, formatSsoString),
			ssoRow('sso_token_url', config.tokenUrl, current.tokenUrl, formatSsoString),
			ssoRow('sso_userinfo_url', config.userInfoUrl, current.userInfoUrl, formatSsoString),
			ssoRow('sso_jwks_url', config.jwksUrl, current.jwksUrl, formatSsoString),
			ssoRow('sso_client_id', config.clientId, current.clientId, formatSsoString),
			ssoRow('sso_scope', config.scope, current.scope, formatSsoString),
			ssoRow('sso_allowed_domains', allowedEmailDomains, current.allowedEmailDomains, formatSsoDomains),
			ssoRow('sso_auto_provision', config.autoProvision, current.autoProvision, formatSsoBoolean),
			ssoRow('sso_redirect_uri', undefined, null, formatSsoString),
		];
		if (config.clientSecret !== undefined) {
			rows.push(ssoRow('sso_client_secret', config.clientSecret, current.clientSecret ?? null, formatSsoString));
		}
		const cache = this.configCache;
		const results = await Promise.allSettled(
			rows
				.filter((row) => row.value !== undefined || !configs.has(row.key))
				.map((row) =>
					this.compareAndSetStoredValue(cache, row.key, (raw) => ({value: nextSsoRowValue(row, raw), result: null})),
				),
		);
		const errors: Array<unknown> = results.flatMap((result) => (result.status === 'rejected' ? [result.reason] : []));
		if (results.some((result) => result.status === 'fulfilled' && result.value.written)) {
			try {
				await this.publishRefresh(cache.sourceId);
			} catch (error) {
				errors.push(error);
			}
		}
		if (errors.length === 1) throw errors[0];
		if (errors.length > 1) throw new AggregateError(errors, 'Failed to write or publish the SSO config');
		return this.getSsoConfig({includeSecret: true});
	}

	private async fetchRegistrationUrlDefinitions(): Promise<Array<InstanceRegistrationUrl>> {
		const raw = await this.fetchConfigFromDatabase(REGISTRATION_URLS_KEY);
		return parseStoredCollection(StoredRegistrationUrlSchema, raw, 'registration URLs');
	}

	private hashRegistrationUrlCode(code: string): string {
		return crypto.createHash('sha256').update(code).digest('hex');
	}
}
