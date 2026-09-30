// SPDX-License-Identifier: AGPL-3.0-or-later

import {createECDH} from 'node:crypto';
import {isConfigObject} from '@fluxer/config/src/config_loader/ConfigObject';
import {buildNamedFluxerEnvOverrides, readEnvValue} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {
	buildUrl,
	type DerivedEndpoints,
	deriveEndpointsFromDomain,
	normalizePublicEndpoint,
	parsePublicOrigin,
	parseWebOrigin,
} from '@fluxer/config/src/EndpointDerivation';
import {CACHE_PURGE_ADAPTER_NAMES, type MasterConfig, STORE_PRODUCT_SLOT_NAMES} from '@fluxer/config/src/MasterConfig';

let cachedConfig: MasterConfig | null = null;

const DEFAULT_PASSKEY_ORIGINS = [
	'https://fluxer.app',
	'https://web.fluxer.app',
	'https://web.canary.fluxer.app',
	'https://fluxer.com',
	'https://canary.fluxer.com',
	'android:apk-key-hash:keSY4bimyLqZQV7bKXgpa2xYuqXi0qZJzsYtp6gpx7w',
];

function defaultConfig(): MasterConfig {
	return {
		env: 'development',
		domain: {
			base_domain: '',
			public_origin: '',
			public_scheme: 'http',
			public_port: 8088,
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
			kv_mode: 'standalone',
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
				donation_proxy_key: '',
				presigned_attachment_uploads_enabled: false,
				presigned_harvest_downloads_enabled: true,
				unfurl_ignored_hosts: [],
				app_origin_aliases: [],
				storage_change_feed: {
					enabled: false,
					stream: 'STORAGE_CHANGES',
				},
			},
			nats: {
				core_url: 'nats://127.0.0.1:4222',
				jetstream_url: 'nats://127.0.0.1:4222',
				auth_token: '',
			},
			media_proxy: {
				secret_key: '',
				upload_relay: {
					endpoint: 'http://localhost:8088/media',
					secret_base64: '',
					max_body_bytes: 524_288_000,
					token_ttl_secs: 900,
					keep_direct_countries: [],
				},
				attachment_urls: {
					secrets_base64: [],
				},
			},
			gateway: {
				rpc_auth_token: '',
			},
			admin: {
				secret_key_base: '',
				oauth_client_secret: '',
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
			voice: {
				enabled: false,
				api_key: '',
				api_secret: '',
				url: '',
				internal_url: '',
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
				legacy_prices: {},
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
			cache_purge: {
				adapter: 'none',
				http: {
					endpoint: '',
					token: '',
					timeout_ms: 10_000,
				},
			},
			blocklist_feeds: {},
			breached_password_check: {},
			push: {
				apns: {
					enabled: false,
					apps: [],
				},
			},
			app_store: {
				enabled: false,
				apps: [],
				products: {},
			},
			google_play: {
				enabled: false,
				packages: [],
				token_uri: 'https://oauth2.googleapis.com/token',
				products: {},
			},
			store_billing: {
				sandbox_user_ids: [],
				sandbox_entitles_all: false,
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

function mergeConfig<T>(base: T, overrides: unknown): T {
	if (!isConfigObject(base) || !isConfigObject(overrides)) {
		return overrides === undefined ? base : (overrides as T);
	}
	const out = new Map(Object.entries(base));
	for (const [key, value] of Object.entries(overrides)) {
		const current = out.get(key);
		out.set(key, isConfigObject(current) && isConfigObject(value) ? mergeConfig(current, value) : value);
	}
	return Object.fromEntries(out) as T;
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

function validateUploadRelaySecret(value: string): void {
	const trimmed = value.trim();
	if (trimmed.length === 0) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required');
	}
	if (!/^[A-Za-z0-9+/]+={0,2}$/u.test(trimmed)) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be base64');
	}
	if (Buffer.from(trimmed, 'base64').length < 32) {
		throw new Error('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes');
	}
}

function isCanonicalStandardBase64(value: string): boolean {
	if (!/^[A-Za-z0-9+/]+={0,2}$/u.test(value)) {
		return false;
	}
	return Buffer.from(value, 'base64').toString('base64') === value;
}

function validateAttachmentUrlSecrets(values: Array<string>): void {
	for (const value of values) {
		const trimmed = value.trim();
		if (trimmed.length === 0) {
			continue;
		}
		if (!isCanonicalStandardBase64(trimmed)) {
			throw new Error('FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64 entries must be standard base64');
		}
		if (Buffer.from(trimmed, 'base64').length < 32) {
			throw new Error('FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64 entries must decode to at least 32 bytes');
		}
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
	let derived: Buffer;
	try {
		const curve = createECDH('prime256v1');
		curve.setPrivateKey(priv);
		derived = curve.getPublicKey();
	} catch {
		throw new Error('FLUXER_VAPID_PRIVATE_KEY does not match FLUXER_VAPID_PUBLIC_KEY');
	}
	if (!derived.equals(pub)) {
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

function validateStorageChangeFeedConfig(config: MasterConfig): void {
	const feed = config.services.api?.storage_change_feed;
	if (!feed?.enabled) {
		return;
	}
	if (feed.stream === undefined || !/^[A-Za-z0-9_-]+$/u.test(feed.stream)) {
		throw new Error(
			'FLUXER_API_STORAGE_CHANGE_FEED_STREAM must be letters, digits, underscores or hyphens when the storage change feed is enabled',
		);
	}
}

function normalizeAppOriginAliases(config: MasterConfig): void {
	const api = config.services.api;
	api.app_origin_aliases = [
		...new Set(
			api.app_origin_aliases.map((alias, index) => {
				const origin = parseWebOrigin(alias);
				if (!origin) {
					throw new Error(`FLUXER_APP_ORIGIN_ALIASES entry ${index + 1} must be an HTTP(S) origin`);
				}
				return origin.origin;
			}),
		),
	];
}

function validateCachePurgeConfig(config: MasterConfig): void {
	const cachePurge = config.integrations.cache_purge;
	if (cachePurge.adapter !== 'http') {
		return;
	}
	requireString(cachePurge.http.endpoint, 'FLUXER_CACHE_PURGE_HTTP_ENDPOINT');
	const endpoint = URL.parse(cachePurge.http.endpoint);
	if (
		endpoint === null ||
		(endpoint.protocol !== 'http:' && endpoint.protocol !== 'https:') ||
		endpoint.username !== '' ||
		endpoint.password !== ''
	) {
		throw new Error('FLUXER_CACHE_PURGE_HTTP_ENDPOINT must be an absolute http or https URL without credentials');
	}
	requireString(cachePurge.http.token, 'FLUXER_CACHE_PURGE_HTTP_TOKEN');
	if (!/^[\x21-\x7e]+$/u.test(cachePurge.http.token)) {
		throw new Error('FLUXER_CACHE_PURGE_HTTP_TOKEN must contain only visible ASCII characters');
	}
	assertIntegerInRange(cachePurge.http.timeout_ms, 'FLUXER_CACHE_PURGE_HTTP_TIMEOUT_MS', 1_000, 10_000);
}

function validateStoreProductSlots(
	products: Record<string, string> | undefined,
	envName: string,
	isValidKey: (productKey: string, slot: string) => boolean,
): void {
	for (const [productKey, slot] of Object.entries(products ?? {})) {
		assertOneOf(slot, STORE_PRODUCT_SLOT_NAMES, `${envName} slot for ${productKey}`);
		if (!isValidKey(productKey, slot)) {
			throw new Error(`${envName} key ${productKey} does not match its ${slot} slot`);
		}
	}
}

function validateStoreBillingConfig(config: MasterConfig): void {
	const appStore = config.integrations.app_store;
	for (const [index, app] of (appStore.apps ?? []).entries()) {
		if (!isConfigObject(app) || typeof app.bundle_id !== 'string' || app.bundle_id.trim().length === 0) {
			throw new Error(`FLUXER_APP_STORE_APPS entry ${index + 1} must have a bundle_id`);
		}
		if (typeof app.app_apple_id !== 'number' || !Number.isSafeInteger(app.app_apple_id) || app.app_apple_id <= 0) {
			throw new Error(`FLUXER_APP_STORE_APPS entry ${index + 1} must have a numeric app_apple_id`);
		}
	}
	validateStoreProductSlots(appStore.products, 'FLUXER_APP_STORE_PRODUCTS', (productKey) => productKey.length > 0);
	validateStoreProductSlots(
		config.integrations.google_play.products,
		'FLUXER_GOOGLE_PLAY_PRODUCTS',
		(productKey, slot) => {
			const parts = productKey.split(':');
			const isSubscription = slot === 'monthly' || slot === 'yearly';
			if (isSubscription) {
				return parts.length === 2 && parts.every((part) => part.length > 0);
			}
			return parts.length === 1 && productKey.length > 0;
		},
	);
	for (const userId of config.integrations.store_billing.sandbox_user_ids ?? []) {
		if (!/^\d+$/.test(userId)) {
			throw new Error('FLUXER_STORE_BILLING_SANDBOX_USER_IDS must be a comma separated list of user ids');
		}
	}
	assertBoolean(config.integrations.store_billing.sandbox_entitles_all, 'FLUXER_STORE_BILLING_SANDBOX_ENTITLES_ALL');
}

function validateDomain(value: string, envName: string): void {
	if (value === '') return;
	const parsed = URL.parse(`http://${value}/`);
	const hasPort = value.startsWith('[') ? !value.endsWith(']') : value.includes(':');
	if (
		!parsed ||
		hasPort ||
		/[\s\p{Cc}/\\?#@%]/u.test(value) ||
		!parsed.hostname ||
		parsed.port ||
		parsed.username ||
		parsed.password ||
		parsed.pathname !== '/' ||
		parsed.search ||
		parsed.hash
	) {
		throw new Error(`${envName} must be a hostname without a scheme, port, credentials, or path`);
	}
}

function validatePublicEndpoints(endpoints: DerivedEndpoints): void {
	for (const [key, value] of Object.entries(endpoints)) {
		const envName = `FLUXER_${key.toUpperCase()}_ENDPOINT`;
		requireString(value, envName);
		const gateway = key === 'gateway';
		const parsed = URL.parse(value);
		if (parsed?.port === '0') throw new Error(`${envName} must not use port 0`);
		const authority = /^[a-z]+:\/\/([^/?#]+)/i.exec(value)?.[1];
		const allowedProtocols = gateway ? ['ws:', 'wss:'] : ['http:', 'https:'];
		if (
			!parsed ||
			!authority ||
			!allowedProtocols.includes(parsed.protocol) ||
			!parsed.hostname ||
			/[\s\p{Cc}\\]/u.test(value) ||
			authority.includes('@') ||
			authority.endsWith(':') ||
			value.includes('#') ||
			(!gateway && value.includes('?'))
		) {
			const expected = gateway
				? 'ws/wss URL without credentials or a fragment'
				: 'http/https base URL without credentials, a query, or a fragment';
			throw new Error(`${envName} must be an absolute ${expected}`);
		}
	}
}

function normalizeConfig(config: MasterConfig): MasterConfig {
	assertOneOf(config.env, ['development', 'production', 'test'], 'FLUXER_ENV');
	assertOneOf(config.domain.public_scheme, ['http', 'https'], 'FLUXER_PUBLIC_SCHEME');
	assertOneOf(config.database.backend, ['postgres', 'cassandra'], 'FLUXER_DATABASE_BACKEND');
	assertOneOf(config.internal.kv_mode, ['standalone', 'cluster'], 'FLUXER_KV_MODE');
	assertOneOf(config.integrations.email.provider, ['smtp', 'none'], 'FLUXER_EMAIL_PROVIDER');
	assertOneOf(config.integrations.search.engine, ['elasticsearch', 'meilisearch'], 'FLUXER_SEARCH_ENGINE');
	assertOneOf(config.integrations.cache_purge.adapter, CACHE_PURGE_ADAPTER_NAMES, 'FLUXER_CACHE_PURGE_ADAPTER');
	validatePostgresConfig(config);
	validateApiWorkerConfig(config);
	validateStorageChangeFeedConfig(config);
	validateCachePurgeConfig(config);
	validateStoreBillingConfig(config);
	normalizeAppOriginAliases(config);
	assertIntegerInRange(config.services.api.max_inflight_requests, 'FLUXER_API_MAX_INFLIGHT_REQUESTS', 1, 100_000);
	assertIntegerInRange(config.services.api.headers_timeout_ms, 'FLUXER_API_HEADERS_TIMEOUT_MS', 1_000, 3_600_000);
	assertIntegerInRange(config.services.api.request_timeout_ms, 'FLUXER_API_REQUEST_TIMEOUT_MS', 1_000, 3_600_000);
	assertIntegerInRange(config.domain.public_port, 'FLUXER_PUBLIC_PORT', 1, 65_535);
	requireString(config.domain.base_domain, 'FLUXER_BASE_DOMAIN');
	for (const key of ['base_domain', 'static_cdn_domain', 'invite_domain', 'gift_domain'] as const) {
		validateDomain(config.domain[key], `FLUXER_${key.toUpperCase()}`);
	}
	requireString(config.auth.sudo_mode_secret, 'FLUXER_SUDO_MODE_SECRET');
	requireString(config.auth.connection_initiation_secret, 'FLUXER_CONNECTION_INITIATION_SECRET');
	validateVapidConfig(config);
	requireString(config.s3?.access_key_id, 'FLUXER_S3_ACCESS_KEY_ID');
	requireString(config.s3?.secret_access_key, 'FLUXER_S3_SECRET_ACCESS_KEY');
	requireString(config.services.media_proxy.secret_key, 'FLUXER_MEDIA_PROXY_SECRET_KEY');
	validateUploadRelaySecret(config.services.media_proxy.upload_relay.secret_base64);
	validateAttachmentUrlSecrets(config.services.media_proxy.attachment_urls.secrets_base64);
	requireString(config.services.admin.secret_key_base, 'FLUXER_ADMIN_SECRET_KEY_BASE');
	requireString(config.services.admin.oauth_client_secret, 'FLUXER_ADMIN_OAUTH_CLIENT_SECRET');
	requireString(config.services.gateway.rpc_auth_token, 'FLUXER_GATEWAY_RPC_AUTH_TOKEN');
	return config;
}

function applyPublicOrigin(config: MasterConfig): MasterConfig {
	const raw = config.domain.public_origin;
	if (raw.trim().length === 0 && !/\p{Cc}/u.test(raw)) {
		return config;
	}
	const origin = parsePublicOrigin(raw);
	if (!origin) {
		throw new Error(
			'FLUXER_PUBLIC_ORIGIN must be a scheme, host and optional port such as https://chat.example.com:8443',
		);
	}
	return {
		...config,
		domain: {
			...config.domain,
			base_domain: origin.base_domain,
			public_scheme: origin.public_scheme,
			public_port: origin.public_port,
		},
	};
}

function applyPublicPort(config: MasterConfig, endpoints: DerivedEndpoints): MasterConfig {
	const {base_domain, public_port} = config.domain;
	const comparisonHost = new URL(buildUrl('http', base_domain)).hostname;
	const normalize = (url: string) => normalizePublicEndpoint(url, comparisonHost, public_port);
	const normalizeOptional = (url: string | undefined) => (url === undefined ? undefined : normalize(url));
	const normalizedEndpoints = {...endpoints};
	for (const key of Object.keys(normalizedEndpoints) as Array<keyof DerivedEndpoints>) {
		normalizedEndpoints[key] = normalize(normalizedEndpoints[key]);
	}
	const {bluesky, passkeys} = config.auth;
	const {branding} = config.instance;
	const {email, voice} = config.integrations;
	return {
		...config,
		domain: {
			...config.domain,
			public_origin: buildUrl(config.domain.public_scheme, base_domain, public_port),
		},
		endpoints: normalizedEndpoints,
		s3: config.s3 && {...config.s3, presigned_url_base: normalizeOptional(config.s3.presigned_url_base)},
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
				...passkeys,
				additional_allowed_origins: passkeys.additional_allowed_origins.map(normalize),
			},
			bluesky: {
				...bluesky,
				client_uri: normalize(bluesky.client_uri),
				logo_uri: normalize(bluesky.logo_uri),
				tos_uri: normalize(bluesky.tos_uri),
				policy_uri: normalize(bluesky.policy_uri),
			},
		},
		integrations: {
			...config.integrations,
			email: {...email, app_base_url: normalize(email.app_base_url)},
			voice: {...voice, url: normalize(voice.url)},
		},
		instance: {
			...config.instance,
			branding: {
				...branding,
				icon_url: normalizeOptional(branding.icon_url),
				symbol_url: normalizeOptional(branding.symbol_url),
				logo_url: normalizeOptional(branding.logo_url),
				wordmark_url: normalizeOptional(branding.wordmark_url),
				favicon_url: normalizeOptional(branding.favicon_url),
				status_page_url: normalizeOptional(branding.status_page_url),
				status_page_incident_history_url: normalizeOptional(branding.status_page_incident_history_url),
			},
		},
	};
}

function normalizePasskeyOrigin(origin: string, index: number): string {
	const webOrigin = parseWebOrigin(origin);
	if (webOrigin) {
		return webOrigin.origin;
	}
	const fingerprint = /^android:apk-key-hash:([A-Za-z0-9_-]{43})$/.exec(origin)?.[1];
	if (fingerprint) {
		const bytes = Buffer.from(fingerprint, 'base64url');
		if (bytes.length === 32 && bytes.toString('base64url') === fingerprint) {
			return origin;
		}
	}
	throw new Error(
		`FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS entry ${index + 1} must be an HTTP(S) origin or a canonical Android signing-certificate origin`,
	);
}

function normalizePasskeys(config: MasterConfig, useDefaultOrigins: boolean): void {
	const passkeys = config.auth.passkeys;
	passkeys.additional_allowed_origins = passkeys.additional_allowed_origins.map(normalizePasskeyOrigin);
	if (passkeys.rp_id.trim().length === 0) {
		passkeys.rp_id = config.domain.base_domain;
	}
	if (useDefaultOrigins || passkeys.additional_allowed_origins.length === 0) {
		passkeys.additional_allowed_origins = [
			...new Set([...passkeys.additional_allowed_origins, new URL(config.endpoints.app).origin]),
		];
	}
}

export async function loadConfig(): Promise<MasterConfig> {
	if (cachedConfig) {
		return cachedConfig;
	}
	const overrides = buildNamedFluxerEnvOverrides(process.env);
	const merged = applyPublicOrigin(mergeConfig(defaultConfig(), overrides));
	const normalized = normalizeConfig(merged);
	const derived = deriveEndpointsFromDomain(normalized.domain);
	const endpoints = {...derived, ...(normalized.endpoint_overrides ?? {})};
	validatePublicEndpoints(endpoints);
	const withPublicPort = applyPublicPort(normalized, endpoints);
	normalizePasskeys(
		withPublicPort,
		readEnvValue(process.env, 'FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS') === undefined,
	);
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
