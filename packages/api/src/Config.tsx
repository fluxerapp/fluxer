/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {APIConfig, BlueskyOAuthConfig} from '@fluxer/api/src/config/APIConfig';
import type {MasterConfig} from '@fluxer/config/src/MasterZodSchema.generated';

function extractHostname(url: string): string {
	try {
		return new URL(url).hostname;
	} catch {
		throw new Error(`Invalid URL: ${url}`);
	}
}

interface CsamIntegrationRaw {
	enabled?: boolean;
	provider?: 'photo_dna' | 'arachnid_shield';
	photo_dna?: {
		hash_service_url?: string;
		hash_service_timeout_ms?: number;
		match_endpoint?: string;
		subscription_key?: string;
		match_enhance?: boolean;
		rate_limit_rps?: number;
	};
	arachnid_shield?: {
		endpoint?: string;
		username?: string;
		password?: string;
		timeout_ms?: number;
		max_retries?: number;
		retry_backoff_ms?: number;
	};
}

interface CsamIntegrationInput {
	photo_dna: MasterConfig['integrations']['photo_dna'];
	csam_integration?: CsamIntegrationRaw;
}

function buildCsamIntegrationConfig(integrations: CsamIntegrationInput): {
	enabled: boolean;
	provider: 'photo_dna' | 'arachnid_shield';
	photoDna: {
		hashServiceUrl: string;
		hashServiceTimeoutMs: number;
		matchEndpoint: string;
		subscriptionKey: string;
		matchEnhance: boolean;
		rateLimitRps: number;
	};
	arachnidShield: {
		endpoint: string;
		username: string;
		password: string;
		timeoutMs: number;
		maxRetries: number;
		retryBackoffMs: number;
	};
} {
	const csam = integrations.csam_integration;
	const photoDnaLegacy = integrations.photo_dna;

	return {
		enabled: csam?.enabled ?? photoDnaLegacy.enabled,
		provider: csam?.provider ?? 'photo_dna',
		photoDna: {
			hashServiceUrl: csam?.photo_dna?.hash_service_url ?? photoDnaLegacy.hash_service_url,
			hashServiceTimeoutMs: csam?.photo_dna?.hash_service_timeout_ms ?? photoDnaLegacy.hash_service_timeout_ms,
			matchEndpoint: csam?.photo_dna?.match_endpoint ?? photoDnaLegacy.match_endpoint,
			subscriptionKey: csam?.photo_dna?.subscription_key ?? photoDnaLegacy.subscription_key,
			matchEnhance: csam?.photo_dna?.match_enhance ?? photoDnaLegacy.match_enhance,
			rateLimitRps: csam?.photo_dna?.rate_limit_rps ?? photoDnaLegacy.rate_limit_rps,
		},
		arachnidShield: {
			endpoint: csam?.arachnid_shield?.endpoint ?? 'https://shield.projectarachnid.com/v1/media',
			username: csam?.arachnid_shield?.username ?? '',
			password: csam?.arachnid_shield?.password ?? '',
			timeoutMs: csam?.arachnid_shield?.timeout_ms ?? 30000,
			maxRetries: csam?.arachnid_shield?.max_retries ?? 3,
			retryBackoffMs: csam?.arachnid_shield?.retry_backoff_ms ?? 1000,
		},
	};
}

export function buildAPIConfigFromMaster(master: MasterConfig): APIConfig {
	if (!master.internal) {
		throw new Error('internal configuration is required for the API');
	}
	const cassandraSource = master.database.cassandra;
	const s3Config = master.s3;
	if (!s3Config) {
		throw new Error('S3 configuration is required for the API');
	}
	const s3Buckets = s3Config.buckets ?? {
		cdn: '',
		uploads: '',
		downloads: '',
		reports: '',
		harvests: '',
		static: '',
	};

	if (master.database.backend === 'cassandra' && !cassandraSource) {
		throw new Error('Cassandra configuration is required when database.backend is "cassandra".');
	}

	return {
		nodeEnv: master.env === 'test' ? 'development' : master.env,
		port: master.services.api.port,

		cassandra: {
			hosts: cassandraSource?.hosts.join(',') ?? '',
			keyspace: cassandraSource?.keyspace ?? '',
			localDc: cassandraSource?.local_dc ?? '',
			username: cassandraSource?.username ?? '',
			password: cassandraSource?.password ?? '',
		},

		database: {
			backend: master.database.backend,
			sqlitePath: master.database.sqlite_path,
		},

		kv: {
			url: master.internal.kv,
			mode: ((master.internal as {kv_mode?: string}).kv_mode ?? 'standalone') as 'standalone' | 'cluster',
			clusterNodes:
				(master.internal as {kv_cluster_nodes?: Array<{host: string; port: number}>}).kv_cluster_nodes ?? [],
			clusterNatMap:
				(master.internal as {kv_cluster_nat_map?: Record<string, {host: string; port: number}>}).kv_cluster_nat_map ??
				{},
		},

		nats: {
			coreUrl: master.services.nats?.core_url ?? 'nats://127.0.0.1:4222',
			jetStreamUrl: master.services.nats?.jetstream_url ?? 'nats://127.0.0.1:4223',
			authToken: master.services.nats?.auth_token ?? '',
		},

		mediaProxy: {
			host: extractHostname(master.internal.media_proxy),
			port: new URL(master.internal.media_proxy).port
				? Number.parseInt(new URL(master.internal.media_proxy).port, 10)
				: 80,
			secretKey: master.services.media_proxy.secret_key,
		},

		geoip: {
			maxmindDbPath: master.geoip.maxmind_db_path,
		},

		proxy: {
			trust_cf_connecting_ip: master.proxy.trust_cf_connecting_ip,
		},

		endpoints: {
			apiPublic: master.endpoints.api,
			apiClient: master.endpoints.api_client,
			webApp: master.endpoints.app,
			gateway: master.endpoints.gateway,
			media: master.endpoints.media,
			marketing: master.endpoints.marketing,
			admin: master.endpoints.admin,
			invite: master.endpoints.invite,
			gift: master.endpoints.gift,
			staticCdn: master.endpoints.static_cdn,
		},

		hosts: {
			invite: extractHostname(master.endpoints.invite),
			gift: extractHostname(master.endpoints.gift),
			marketing: extractHostname(master.endpoints.marketing),
			unfurlIgnored: master.services.api.unfurl_ignored_hosts,
		},

		s3: {
			endpoint: s3Config.endpoint,
			presignedUrlBase: s3Config.presigned_url_base,
			region: s3Config.region,
			accessKeyId: s3Config.access_key_id,
			secretAccessKey: s3Config.secret_access_key,
			buckets: s3Buckets,
		},

		email: {
			enabled: master.integrations.email.enabled,
			provider: master.integrations.email.provider,
			webhookSecret: master.integrations.email.webhook_secret ?? undefined,
			fromEmail: master.integrations.email.from_email,
			fromName: master.integrations.email.from_name,
			smtp: master.integrations.email.smtp
				? {
						host: master.integrations.email.smtp.host,
						port: master.integrations.email.smtp.port,
						username: master.integrations.email.smtp.username,
						password: master.integrations.email.smtp.password,
						secure: master.integrations.email.smtp.secure ?? true,
					}
				: undefined,
		},
		sms: {
			enabled: master.integrations.sms.enabled,
			accountSid: master.integrations.sms.account_sid,
			authToken: master.integrations.sms.auth_token,
			verifyServiceSid: master.integrations.sms.verify_service_sid,
		},
		captcha: {
			enabled: master.integrations.captcha.enabled,
			provider: master.integrations.captcha.provider,
			hcaptcha: master.integrations.captcha.hcaptcha
				? {
						siteKey: master.integrations.captcha.hcaptcha.site_key,
						secretKey: master.integrations.captcha.hcaptcha.secret_key,
					}
				: undefined,
			turnstile: master.integrations.captcha.turnstile
				? {
						siteKey: master.integrations.captcha.turnstile.site_key,
						secretKey: master.integrations.captcha.turnstile.secret_key,
					}
				: undefined,
		},
		voice: {
			enabled: master.integrations.voice.enabled,
			apiKey: master.integrations.voice.api_key,
			apiSecret: master.integrations.voice.api_secret,
			webhookUrl: master.integrations.voice.webhook_url,
			url: master.integrations.voice.url,
			defaultRegion: master.integrations.voice.default_region,
		},
		search: {
			engine: ((master.integrations.search as {engine?: string}).engine ?? 'meilisearch') as
				| 'meilisearch'
				| 'elasticsearch',
			url: master.integrations.search.url,
			apiKey: master.integrations.search.api_key,
			username: (master.integrations.search as {username?: string}).username ?? '',
			password: (master.integrations.search as {password?: string}).password ?? '',
		},
		stripe: {
			enabled: master.integrations.stripe.enabled,
			secretKey: master.integrations.stripe.secret_key,
			webhookSecret: master.integrations.stripe.webhook_secret,
			prices: master.integrations.stripe.prices
				? {
						monthlyUsd: master.integrations.stripe.prices.monthly_usd,
						monthlyEur: master.integrations.stripe.prices.monthly_eur,
						yearlyUsd: master.integrations.stripe.prices.yearly_usd,
						yearlyEur: master.integrations.stripe.prices.yearly_eur,
						gift1MonthUsd: master.integrations.stripe.prices.gift_1_month_usd,
						gift1MonthEur: master.integrations.stripe.prices.gift_1_month_eur,
						gift1YearUsd: master.integrations.stripe.prices.gift_1_year_usd,
						gift1YearEur: master.integrations.stripe.prices.gift_1_year_eur,
					}
				: undefined,
		},
		cloudflare: {
			purgeEnabled: master.integrations.cloudflare.purge_enabled,
			zoneId: master.integrations.cloudflare.zone_id,
			apiToken: master.integrations.cloudflare.api_token,
		},
		clamav: {
			enabled: master.integrations.clamav.enabled,
			host: master.integrations.clamav.host,
			port: master.integrations.clamav.port,
			failOpen: master.integrations.clamav.fail_open,
		},

		photoDna: {
			enabled: master.integrations.photo_dna.enabled,
			hashService: {
				url: master.integrations.photo_dna.hash_service_url,
				timeoutMs: master.integrations.photo_dna.hash_service_timeout_ms,
			},
			api: {
				endpoint: master.integrations.photo_dna.match_endpoint,
				subscriptionKey: master.integrations.photo_dna.subscription_key,
				enhance: master.integrations.photo_dna.match_enhance,
			},
			rateLimit: {
				requestsPerSecond: master.integrations.photo_dna.rate_limit_rps,
			},
		},

		csamIntegration: buildCsamIntegrationConfig(master.integrations as CsamIntegrationInput),

		ncmec: {
			enabled: master.integrations.ncmec.enabled,
			baseUrl: master.integrations.ncmec.base_url,
			username: master.integrations.ncmec.username,
			password: master.integrations.ncmec.password,
		},

		alerts: {
			webhookUrl: master.alerts?.webhook_url,
		},

		admin: {
			basePath: master.services.admin.base_path,
			oauthClientSecret: master.services.admin.oauth_client_secret,
		},

		appPublic: {
			sentryDsn: master.app_public.sentry_dsn,
		},

		auth: {
			sudoModeSecret: master.auth.sudo_mode_secret,
			connectionInitiationSecret: master.auth.connection_initiation_secret,
			passkeys: {
				rpName: master.auth.passkeys.rp_name,
				rpId: master.auth.passkeys.rp_id,
				allowedOrigins: master.auth.passkeys.additional_allowed_origins,
			},
			vapid: {
				publicKey: master.auth.vapid.public_key,
				privateKey: master.auth.vapid.private_key,
				email: master.auth.vapid.email,
			},
			bluesky: master.auth.bluesky as BlueskyOAuthConfig,
		},

		cookie: master.cookie,

		gif: {
			provider: master.integrations.gif.provider,
		},
		klipy: {
			apiKey: master.integrations.klipy.api_key,
		},
		tenor: {
			apiKey: master.integrations.tenor.api_key,
		},
		youtube: {
			apiKey: master.integrations.youtube.api_key,
		},

		instance: {
			selfHosted: master.instance.self_hosted,
			autoJoinInviteCode: master.instance.auto_join_invite_code,
			visionariesGuildId: master.instance.visionaries_guild_id,
			operatorsGuildId: master.instance.operators_guild_id,
			privateKeyPath: master.instance.private_key_path,
		},

		domain: {
			baseDomain: master.domain.base_domain,
		},

		federation: master.federation?.enabled
			? {
					enabled: master.federation.enabled,
				}
			: undefined,
		discovery: {
			enabled: master.discovery.enabled,
			minMemberCount: master.discovery.min_member_count,
		},
		dev: {
			relaxRegistrationRateLimits: master.dev.relax_registration_rate_limits,
			disableRateLimits: master.dev.disable_rate_limits,
			testModeEnabled: master.dev.test_mode_enabled,
			testHarnessToken: master.dev.test_harness_token,
		},
		csam: {
			evidenceRetentionDays: master.csam.evidence_retention_days,
			jobRetentionDays: master.csam.job_retention_days,
			cleanupBatchSize: master.csam.cleanup_batch_size,
			queue: {
				timeoutMs: master.csam.queue?.timeout_ms ?? 30000,
				maxEntriesPerBatch: master.csam.queue?.max_entries_per_batch ?? 5,
				consumerLockTtlSeconds: master.csam.queue?.consumer_lock_ttl_seconds ?? 5,
			},
		},

		attachmentDecayEnabled: master.attachment_decay_enabled,
		deletionGracePeriodHours: master.dev.test_mode_enabled ? 0.01 : master.deletion_grace_period_hours,
		inactivityDeletionThresholdDays: master.inactivity_deletion_threshold_days,

		push: {
			publicVapidKey: master.auth.vapid.public_key,
		},

		queue: {
			baseUrl: 'queue' in master.internal ? String(master.internal.queue) : 'http://localhost:8088/queue',
			authSecret: 'queue' in master.services ? (master.services.queue as {secret?: string}).secret : undefined,
		},
	};
}

let _config: APIConfig | null = null;

export function initializeConfig(config: APIConfig): void {
	if (_config !== null) {
		return;
	}
	_config = config;
}

export function getConfig(): APIConfig {
	if (_config === null) {
		throw new Error('Config has not been initialized. Call initializeConfig() first.');
	}
	return _config;
}

export function resetConfig(): void {
	_config = null;
}

export const Config: APIConfig = new Proxy({} as APIConfig, {
	get(_target, prop: keyof APIConfig | symbol) {
		if (_config === null) {
			throw new Error('Config has not been initialized. Call initializeConfig() first.');
		}
		return _config[prop as keyof APIConfig];
	},
	set() {
		throw new Error('Cannot modify Config directly. Use initializeConfig() instead.');
	},
});
