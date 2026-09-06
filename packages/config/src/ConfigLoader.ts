// SPDX-License-Identifier: AGPL-3.0-or-later

import {createPrivateKey, createPublicKey} from 'node:crypto';
import {buildNamedFluxerEnvOverrides} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {
	type DerivedEndpoints,
	deriveEndpointsFromDomain,
	normalizePublicEndpoint,
} from '@fluxer/config/src/EndpointDerivation';
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
			docs: '',
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
				prepared_statements: true,
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
			},
		},
		services: {
			api: {
				port: 8080,
				headers_timeout_ms: 30_000,
				request_timeout_ms: 120_000,
				max_inflight_requests: 512,
				ip_ban_exempt_ips: [],
				desktop_github_redirect_countries: [],
				presigned_attachment_uploads_enabled: false,
				presigned_downloads_enabled: false,
				presigned_harvest_downloads_enabled: true,
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
					secret_base64: '',
					max_body_bytes: 524_288_000,
					token_ttl_secs: 900,
					keep_direct_countries: [],
				},
			},
			gateway: {
				port: 8771,
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
			sso_allow_private_addresses: false,
			passkeys: {
				rp_name: 'Fluxer',
				rp_id: '',
				additional_allowed_origins: DEFAULT_PASSKEY_ORIGINS,
			},
			vapid: {
				public_key: '',
				private_key: '',
				email: '',
			},
			bluesky: {
				enabled: false,
				client_name: 'Fluxer',
				client_uri: '',
				logo_uri: '',
				tos_uri: '',
				policy_uri: '',
				keys: [],
			},
		},
		integrations: {
			email: {
				enabled: false,
				provider: 'none',
				from_email: '',
				from_name: 'Fluxer',
				app_base_url: '',
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
				internal_url: '',
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
			blocklist_feeds: {},
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
		deletion_grace_period_hours: 336,
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

function validateUploadRelaySecret(value: string, mode: string): void {
	const trimmed = value.trim();
	if (trimmed.length === 0) {
		if (mode === 'upload') {
			throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required in upload mode');
		}
		return;
	}
	if (!/^[A-Za-z0-9+/]+={0,2}$/u.test(trimmed)) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be base64');
	}
	if (Buffer.from(trimmed, 'base64').length < 32) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes');
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

function validateVapidConfig(config: MasterConfig): void {
	requireString(config.auth.vapid.public_key, 'FLUXER_VAPID_PUBLIC_KEY');
	requireString(config.auth.vapid.private_key, 'FLUXER_VAPID_PRIVATE_KEY');
	const pub = Buffer.from(config.auth.vapid.public_key, 'base64url');
	const priv = Buffer.from(config.auth.vapid.private_key, 'base64url');
	if (pub.length !== 65 || pub[0] !== 0x04) {
		throw new Error('FLUXER_VAPID_PUBLIC_KEY must be the base64url 65-byte uncompressed P-256 point');
	}
	if (priv.length !== 32) {
		throw new Error('FLUXER_VAPID_PRIVATE_KEY must be the base64url 32-byte P-256 scalar');
	}
	const jwk = {
		kty: 'EC',
		crv: 'P-256',
		x: pub.subarray(1, 33).toString('base64url'),
		y: pub.subarray(33, 65).toString('base64url'),
		d: priv.toString('base64url'),
	};
	let derived: {x?: string; y?: string};
	try {
		derived = createPublicKey(createPrivateKey({key: jwk, format: 'jwk'})).export({format: 'jwk'});
	} catch {
		throw new Error('FLUXER_VAPID_PRIVATE_KEY does not match FLUXER_VAPID_PUBLIC_KEY');
	}
	if (derived.x !== jwk.x || derived.y !== jwk.y) {
		throw new Error('FLUXER_VAPID_PRIVATE_KEY does not match FLUXER_VAPID_PUBLIC_KEY');
	}
}

function validatePostgresConfig(config: MasterConfig): void {
	const postgres = config.database.postgres;
	assertIntegerInRange(postgres.port, 'FLUXER_POSTGRES_PORT', 1, 65535);
	assertIntegerInRange(postgres.max_connections, 'FLUXER_POSTGRES_MAX_CONNECTIONS', 1, 1000);
	assertBoolean(postgres.ssl, 'FLUXER_POSTGRES_SSL');
	assertIdentifier(postgres.kv_table, 'FLUXER_POSTGRES_KV_TABLE');
	assertBoolean(postgres.prepared_statements, 'FLUXER_POSTGRES_PREPARED_STATEMENTS');
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

function validateCaptchaConfig(config: MasterConfig): void {
	const captcha = config.integrations.captcha;
	if (!captcha.enabled) {
		return;
	}
	if (captcha.provider === 'hcaptcha') {
		requireString(captcha.hcaptcha?.site_key, 'FLUXER_CAPTCHA_HCAPTCHA_SITE_KEY');
		requireString(captcha.hcaptcha?.secret_key, 'FLUXER_CAPTCHA_HCAPTCHA_SECRET_KEY');
		return;
	}
	if (captcha.provider === 'turnstile') {
		requireString(captcha.turnstile?.site_key, 'FLUXER_CAPTCHA_TURNSTILE_SITE_KEY');
		requireString(captcha.turnstile?.secret_key, 'FLUXER_CAPTCHA_TURNSTILE_SECRET_KEY');
		return;
	}
	throw new Error('FLUXER_CAPTCHA_PROVIDER must be hcaptcha or turnstile when FLUXER_CAPTCHA_ENABLED is true');
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
	validateCaptchaConfig(config);
	validateApiWorkerConfig(config);
	assertIntegerInRange(config.services.api.max_inflight_requests, 'FLUXER_API_MAX_INFLIGHT_REQUESTS', 1, 100_000);
	assertIntegerInRange(config.services.api.headers_timeout_ms, 'FLUXER_API_HEADERS_TIMEOUT_MS', 1_000, 3_600_000);
	assertIntegerInRange(config.services.api.request_timeout_ms, 'FLUXER_API_REQUEST_TIMEOUT_MS', 1_000, 3_600_000);
	requireString(config.domain.base_domain, 'FLUXER_BASE_DOMAIN');
	requireString(config.auth.sudo_mode_secret, 'FLUXER_SUDO_MODE_SECRET');
	requireString(config.auth.connection_initiation_secret, 'FLUXER_CONNECTION_INITIATION_SECRET');
	validateVapidConfig(config);
	requireString(config.s3?.access_key_id, 'FLUXER_S3_ACCESS_KEY_ID');
	requireString(config.s3?.secret_access_key, 'FLUXER_S3_SECRET_ACCESS_KEY');
	requireString(config.services.media_proxy.secret_key, 'FLUXER_MEDIA_PROXY_SECRET_KEY');
	validateUploadRelaySecret(config.services.media_proxy.upload_relay.secret_base64, config.services.media_proxy.mode);
	requireString(config.services.admin.secret_key_base, 'FLUXER_ADMIN_SECRET_KEY_BASE');
	requireString(config.services.admin.oauth_client_secret, 'FLUXER_ADMIN_OAUTH_CLIENT_SECRET');
	if (!config.instance.self_hosted) {
		requireString(config.services.marketing.secret_key_base, 'FLUXER_MARKETING_SECRET_KEY_BASE');
	}
	requireString(config.services.gateway.rpc_auth_token, 'FLUXER_GATEWAY_RPC_AUTH_TOKEN');
	return config;
}

function applyPublicPort(config: MasterConfig, endpoints: DerivedEndpoints): MasterConfig {
	const {base_domain, public_port} = config.domain;
	const normalize = (url: string) => normalizePublicEndpoint(url, base_domain, public_port);
	const normalizedEndpoints = {...endpoints};
	for (const key of Object.keys(normalizedEndpoints) as Array<keyof DerivedEndpoints>) {
		normalizedEndpoints[key] = normalize(normalizedEndpoints[key]);
	}
	return {
		...config,
		endpoints: normalizedEndpoints,
		services: {
			...config.services,
			media_proxy: {
				...config.services.media_proxy,
				upload_relay: {
					...config.services.media_proxy.upload_relay,
					endpoint: normalize(config.services.media_proxy.upload_relay.endpoint),
				},
			},
		},
		auth: {
			...config.auth,
			passkeys: {
				...config.auth.passkeys,
				additional_allowed_origins: config.auth.passkeys.additional_allowed_origins.map(normalize),
			},
		},
	};
}

function resolveAppOrigin(appEndpoint: string): string {
	try {
		return new URL(appEndpoint).origin;
	} catch {
		throw new Error(`FLUXER_APP_ENDPOINT must be a valid URL: ${appEndpoint}`);
	}
}

function applyPasskeyDefaults(config: MasterConfig, endpoints: DerivedEndpoints): void {
	const passkeys = config.auth.passkeys;
	if (passkeys.rp_id.trim().length === 0) {
		passkeys.rp_id = config.domain.base_domain;
	}
	if (passkeys.additional_allowed_origins.length === 0) {
		passkeys.additional_allowed_origins = [resolveAppOrigin(endpoints.app)];
	}
}

export async function loadConfig(): Promise<MasterConfig> {
	if (cachedConfig) {
		return cachedConfig;
	}
	const overrides = buildNamedFluxerEnvOverrides(process.env);
	const merged = mergeConfig(defaultConfig(), overrides);
	const normalized = normalizeConfig(merged);
	const derived = deriveEndpointsFromDomain(normalized.domain);
	const endpoints = {...derived, ...(normalized.endpoint_overrides ?? {})};
	requireString(endpoints.api_client, 'FLUXER_API_CLIENT_ENDPOINT');
	const withPublicPort = applyPublicPort(normalized, endpoints);
	applyPasskeyDefaults(withPublicPort, withPublicPort.endpoints);
	cachedConfig = withPublicPort;
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
