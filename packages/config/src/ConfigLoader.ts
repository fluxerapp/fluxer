// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildNamedFluxerEnvOverrides} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {deriveEndpointsFromDomain} from '@fluxer/config/src/EndpointDerivation';
import type {MasterConfig} from '@fluxer/config/src/MasterConfig';

type ConfigObject = Record<string, unknown>;

let cachedConfig: MasterConfig | null = null;

const DEFAULT_PASSKEY_ORIGINS = [
	'https://fluxer.app',
	'https://web.fluxer.app',
	'https://web.canary.fluxer.app',
	'android:apk-key-hash:keSY4bimyLqZQV7bKXgpa2xYuqXi0qZJzsYtp6gpx7w',
	'android:apk-key-hash:zRmCKDKo3uCX2GDZISjJx8Rzo3J-Y3Gbp7s7mAaUH28',
];

function defaultConfig(): MasterConfig {
	return {
		env: 'development',
		domain: {
			base_domain: '',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 8088,
			internal_port: 8088,
			static_cdn_domain: '',
			invite_domain: '',
			gift_domain: '',
		},
		endpoints: {
			api: '',
			api_client: '',
			app: '',
			gateway: '',
			media: '',
			static_cdn: '',
			admin: '',
			marketing: '',
			invite: '',
			gift: '',
		},
		internal: {
			kv: 'redis://localhost:6379/0',
			kv_provider: 'redis',
			kv_mode: 'standalone',
			kv_cluster_nodes: [],
			kv_cluster_nat_map: {},
			api: 'http://127.0.0.1:8080',
			media_proxy: 'http://127.0.0.1:8082',
		},
		database: {
			backend: 'postgres',
			cassandra: {
				hosts: ['127.0.0.1'],
				port: 9042,
				keyspace: 'fluxer',
				local_dc: 'datacenter1',
				username: '',
				password: '',
			},
			postgres: {
				url: '',
				host: '127.0.0.1',
				port: 5432,
				database: 'fluxer',
				username: 'fluxer',
				password: 'fluxer',
				ssl: false,
				ssl_ca: '',
				max_connections: 20,
				kv_table: 'fluxer_kv',
			},
		},
		s3: {
			endpoint: 'http://localhost:3900',
			force_path_style: false,
			region: 'local',
			access_key_id: '',
			secret_access_key: '',
			buckets: {
				cdn: 'fluxer',
				uploads: 'fluxer-uploads',
				downloads: 'fluxer-downloads',
				reports: 'fluxer-reports',
				harvests: 'fluxer-harvests',
				static: 'fluxer-static',
			},
		},
		services: {
			api: {
				port: 8080,
				presigned_attachment_uploads_enabled: false,
				unfurl_ignored_hosts: [],
				embeds: {
					oembed_html_enabled: false,
					oembed_html_allow_untrusted_on_self_hosted: false,
					oembed_html_allowed_hosts: [],
					cache_default_ttl_seconds: 86_400,
					cache_max_ttl_seconds: 604_800,
					cache_min_ttl_seconds: 300,
					cache_respect_remote_ttl: true,
				},
				content_moderation: {
					nsfw_threshold: 0.7,
				},
			},
			nats: {
				core_url: 'nats://127.0.0.1:4222',
				jetstream_url: 'nats://127.0.0.1:4222',
				auth_token: '',
			},
			media_proxy: {
				host: '0.0.0.0',
				port: 8082,
				secret_key: '',
				mode: 'upload',
				upload_relay: {
					endpoint: 'http://localhost:8088/media',
					max_body_bytes: 268_435_456,
					token_ttl_secs: 900,
					keep_direct_countries: [],
				},
			},
			gateway: {
				port: 8771,
				push_enabled: false,
				rpc_auth_token: '',
			},
			admin: {
				port: 3020,
				base_path: '/admin',
				secret_key_base: '',
				oauth_client_secret: '',
			},
			marketing: {
				port: 3010,
				host: '0.0.0.0',
				base_path: '/marketing',
				secret_key_base: '',
			},
			app_proxy: {
				port: 8773,
				assets_dir: 'fluxer_app/dist',
			},
		},
		auth: {
			sudo_mode_secret: '',
			connection_initiation_secret: '',
			passkeys: {
				rp_name: 'Fluxer',
				rp_id: 'fluxer.app',
				additional_allowed_origins: DEFAULT_PASSKEY_ORIGINS,
			},
			vapid: {
				public_key: '',
				private_key: '',
				email: '',
			},
			bluesky: {
				enabled: true,
				client_name: 'Fluxer',
				client_uri: '',
				logo_uri: '',
				tos_uri: 'https://fluxer.app/terms',
				policy_uri: 'https://fluxer.app/privacy',
				keys: [],
			},
		},
		cookie: {
			domain: '',
			secure: false,
		},
		integrations: {
			email: {
				enabled: false,
				provider: 'none',
				from_email: '',
				from_name: 'Fluxer',
			},
			sms: {
				enabled: false,
			},
			captcha: {
				enabled: false,
				provider: 'none',
			},
			voice: {
				enabled: false,
				api_key: '',
				api_secret: '',
				url: '',
				webhook_url: '',
			},
			search: {
				engine: 'elasticsearch',
				url: 'http://127.0.0.1:9200',
				api_key: '',
				username: '',
				password: '',
				tls_reject_unauthorized: true,
			},
			stripe: {
				enabled: false,
				secret_key: '',
				webhook_secret: '',
				prices: {},
			},
			ncmec: {
				enabled: false,
				base_url: '',
				username: '',
				password: '',
			},
			clamav: {
				enabled: false,
				host: '127.0.0.1',
				port: 3310,
				fail_open: false,
			},
			klipy: {
				api_key: '',
			},
			youtube: {
				api_key: '',
			},
			bunny: {
				purge_enabled: false,
				api_key: '',
				pull_zone_id: 0,
			},
			risk_integration: {
				enabled: false,
				ipinfo_api_key: '',
				account_policy_dsl: undefined,
				tor: {
					block_all_relays: false,
					reverse_dns_heuristic: false,
					reverse_dns_timeout_ms: 750,
				},
			},
			push: {
				apns: {
					enabled: false,
					apps: [],
				},
				fcm: {
					enabled: false,
					apps: [],
				},
			},
		},
		instance: {
			self_hosted: false,
			branding: {
				product_name: 'Fluxer',
			},
			setup: {
				configured: false,
			},
			abuse_policy: {
				inbound_phone_country_codes: [],
				phone_verification: {
					inbound_required_prefixes: [],
				},
				direct_contact_spam: {
					enabled: false,
					country_codes: [],
					distinct_target_threshold: 25,
					target_window_ms: 2 * 60 * 60 * 1000,
					action: 'flag_spammer',
				},
			},
		},
		dev: {
			relax_registration_rate_limits: false,
			disable_rate_limits: false,
			test_mode_enabled: false,
		},
		geoip: {
			maxmind_db_path: '',
		},
		proxy: {
			trust_client_ip_header: false,
			client_ip_header: 'x-forwarded-for',
		},
		discovery: {
			enabled: true,
			min_member_count: 1,
		},
		attachment_decay_enabled: true,
		deletion_grace_period_hours: 72,
		inactivity_deletion_threshold_days: 365,
	};
}

function isPlainObject(value: unknown): value is ConfigObject {
	return value !== null && typeof value === 'object' && !Array.isArray(value);
}

function mergeConfig<T>(base: T, overrides: unknown): T {
	if (!isPlainObject(base) || !isPlainObject(overrides)) {
		return overrides === undefined ? base : (overrides as T);
	}
	const out: ConfigObject = {...base};
	for (const [key, value] of Object.entries(overrides)) {
		const current = out[key];
		out[key] = isPlainObject(current) && isPlainObject(value) ? mergeConfig(current, value) : value;
	}
	return out as T;
}

function assertOneOf<T extends string>(value: string, allowed: ReadonlyArray<T>, path: string): asserts value is T {
	if (!allowed.includes(value as T)) {
		throw new Error(`Invalid ${path}: ${value}`);
	}
}

function requireString(value: string | undefined, envName: string): void {
	if (!value || value.trim().length === 0) {
		throw new Error(`${envName} is required`);
	}
}

function assertBoolean(value: unknown, envName: string): asserts value is boolean {
	if (typeof value !== 'boolean') {
		throw new Error(`${envName} must be true or false`);
	}
}

function assertIntegerInRange(value: unknown, envName: string, min: number, max: number): asserts value is number {
	if (typeof value !== 'number' || !Number.isInteger(value) || value < min || value > max) {
		throw new Error(`${envName} must be an integer between ${min} and ${max}`);
	}
}

function assertIdentifier(value: string, envName: string): void {
	if (!/^[A-Za-z_][A-Za-z0-9_]*$/u.test(value)) {
		throw new Error(`${envName} must be a safe Postgres identifier`);
	}
}

function validatePostgresConfig(config: MasterConfig): void {
	const postgres = config.database.postgres;
	assertIntegerInRange(postgres.port, 'FLUXER_POSTGRES_PORT', 1, 65535);
	assertIntegerInRange(postgres.max_connections, 'FLUXER_POSTGRES_MAX_CONNECTIONS', 1, 1000);
	assertBoolean(postgres.ssl, 'FLUXER_POSTGRES_SSL');
	assertIdentifier(postgres.kv_table, 'FLUXER_POSTGRES_KV_TABLE');
	if (config.env !== 'production' || config.database.backend !== 'postgres') {
		return;
	}
	if (!postgres.url) {
		requireString(postgres.host, 'FLUXER_POSTGRES_HOST');
		requireString(postgres.database, 'FLUXER_POSTGRES_DATABASE');
		requireString(postgres.username, 'FLUXER_POSTGRES_USERNAME');
		requireString(postgres.password, 'FLUXER_POSTGRES_PASSWORD');
		if (['127.0.0.1', 'localhost'].includes(postgres.host.trim().toLowerCase())) {
			throw new Error('FLUXER_POSTGRES_HOST must be explicitly configured for production');
		}
		if (postgres.password === 'fluxer') {
			throw new Error('FLUXER_POSTGRES_PASSWORD must not use the development default in production');
		}
	}
	if (!postgres.ssl && !config.instance.self_hosted) {
		throw new Error('FLUXER_POSTGRES_SSL must be true in production');
	}
}

function validateApiWorkerConfig(config: MasterConfig): void {
	const worker = config.services.api?.worker;
	if (!worker) {
		return;
	}
	if (worker.mode !== undefined) {
		assertOneOf(worker.mode, ['all_lanes', 'single_lane', 'single_task'], 'FLUXER_API_WORKER_MODE');
	}
	if (worker.lane !== undefined) {
		assertOneOf(worker.lane, ['realtime', 'unfurl', 'lifecycle', 'batch'], 'FLUXER_API_WORKER_LANE');
	}
	if (worker.mode === 'single_task') {
		requireString(worker.task, 'FLUXER_API_WORKER_TASK');
	}
}

function normalizeConfig(config: MasterConfig): MasterConfig {
	assertOneOf(config.env, ['development', 'production', 'test'], 'FLUXER_ENV');
	assertOneOf(config.domain.public_scheme, ['http', 'https'], 'FLUXER_PUBLIC_SCHEME');
	assertOneOf(config.domain.internal_scheme, ['http', 'https'], 'FLUXER_INTERNAL_SCHEME');
	assertOneOf(config.database.backend, ['postgres', 'cassandra'], 'FLUXER_DATABASE_BACKEND');
	assertOneOf(config.internal.kv_provider, ['redis'], 'FLUXER_KV_PROVIDER');
	assertOneOf(config.internal.kv_mode, ['standalone', 'cluster'], 'FLUXER_KV_MODE');
	assertOneOf(config.integrations.email.provider, ['smtp', 'none'], 'FLUXER_EMAIL_PROVIDER');
	assertOneOf(config.integrations.captcha.provider, ['hcaptcha', 'turnstile', 'none'], 'FLUXER_CAPTCHA_PROVIDER');
	assertOneOf(config.integrations.search.engine, ['elasticsearch', 'meilisearch'], 'FLUXER_SEARCH_ENGINE');
	assertOneOf(
		config.instance.abuse_policy.direct_contact_spam.action,
		['flag_spammer', 'suppress_delivery'],
		'FLUXER_ABUSE_DIRECT_CONTACT_SPAM_ACTION',
	);
	validatePostgresConfig(config);
	validateApiWorkerConfig(config);
	requireString(config.domain.base_domain, 'FLUXER_BASE_DOMAIN');
	requireString(config.auth.sudo_mode_secret, 'FLUXER_SUDO_MODE_SECRET');
	requireString(config.auth.connection_initiation_secret, 'FLUXER_CONNECTION_INITIATION_SECRET');
	requireString(config.auth.vapid.public_key, 'FLUXER_VAPID_PUBLIC_KEY');
	requireString(config.auth.vapid.private_key, 'FLUXER_VAPID_PRIVATE_KEY');
	requireString(config.s3?.access_key_id, 'FLUXER_S3_ACCESS_KEY_ID');
	requireString(config.s3?.secret_access_key, 'FLUXER_S3_SECRET_ACCESS_KEY');
	requireString(config.services.media_proxy.secret_key, 'FLUXER_MEDIA_PROXY_SECRET_KEY');
	requireString(config.services.admin.secret_key_base, 'FLUXER_ADMIN_SECRET_KEY_BASE');
	requireString(config.services.admin.oauth_client_secret, 'FLUXER_ADMIN_OAUTH_CLIENT_SECRET');
	if (!config.instance.self_hosted) {
		requireString(config.services.marketing.secret_key_base, 'FLUXER_MARKETING_SECRET_KEY_BASE');
	}
	requireString(config.services.gateway.rpc_auth_token, 'FLUXER_GATEWAY_RPC_AUTH_TOKEN');
	return config;
}

export async function loadConfig(): Promise<MasterConfig> {
	if (cachedConfig) {
		return cachedConfig;
	}
	const merged = mergeConfig(defaultConfig(), buildNamedFluxerEnvOverrides(process.env));
	const normalized = normalizeConfig(merged);
	const derived = deriveEndpointsFromDomain(normalized.domain);
	const endpoints = {...derived, ...(normalized.endpoint_overrides ?? {})};
	cachedConfig = {...normalized, endpoints};
	return cachedConfig;
}

export function getConfig(): MasterConfig {
	if (!cachedConfig) {
		throw new Error('Config not loaded. Call loadConfig() first.');
	}
	return cachedConfig;
}

export function resetConfig(): void {
	cachedConfig = null;
}
