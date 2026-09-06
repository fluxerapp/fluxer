// SPDX-License-Identifier: AGPL-3.0-or-later

import {generateKeyPairSync} from 'node:crypto';
import {getConfig, loadConfig, resetConfig} from '@fluxer/config/src/ConfigLoader';
import {afterEach, beforeEach, describe, expect, test, vi} from 'vitest';

const MINIMAL_ENV: Record<string, string> = {
	FLUXER_ENV: 'test',
	FLUXER_BASE_DOMAIN: 'localhost',
	FLUXER_PUBLIC_SCHEME: 'http',
	FLUXER_PUBLIC_PORT: '8088',
	FLUXER_CASSANDRA_HOSTS: '127.0.0.1',
	FLUXER_CASSANDRA_PORT: '9042',
	FLUXER_CASSANDRA_KEYSPACE: 'fluxer_test',
	FLUXER_CASSANDRA_LOCAL_DC: 'datacenter1',
	FLUXER_CASSANDRA_USERNAME: 'test-user',
	FLUXER_CASSANDRA_PASSWORD: 'test-password',
	FLUXER_S3_ENDPOINT: 'http://127.0.0.1:3900',
	FLUXER_S3_ACCESS_KEY_ID: 'test-key',
	FLUXER_S3_SECRET_ACCESS_KEY: 'test-secret',
	FLUXER_MEDIA_PROXY_SECRET_KEY: 'test-media-secret',
	FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: 'AAECAwQFBgcICQoLDA0ODxAREhMUFRYXGBkaGxwdHh8=',
	FLUXER_ADMIN_SECRET_KEY_BASE: 'test-admin-secret',
	FLUXER_ADMIN_OAUTH_CLIENT_SECRET: 'test-admin-oauth-secret',
	FLUXER_MARKETING_SECRET_KEY_BASE: 'test-marketing-secret',
	FLUXER_APP_PROXY_PORT: '8773',
	FLUXER_GATEWAY_MEDIA_PROXY_ENDPOINT: 'http://127.0.0.1:8088/media',
	FLUXER_GATEWAY_RPC_AUTH_TOKEN: 'test-gateway-token',
	FLUXER_SUDO_MODE_SECRET: 'test-sudo-secret',
	FLUXER_CONNECTION_INITIATION_SECRET: 'test-connection-secret',
	FLUXER_VAPID_PUBLIC_KEY: 'BB76bTFIuoqmxJtTfZX0yGTn1f_qu9H03B_nkj8OyExJFkN7Y-HBZZzShnHZoEhXKc5ZRy3jFu7OkBbnaQG-4aw',
	FLUXER_VAPID_PRIVATE_KEY: 'Xgi-3P8J-I3Q6U1HlCcXMuc_tKLGAM9nIfznX3Hz68o',
};

function generateVapidPair(): {publicKey: string; privateKey: string} {
	const {privateKey} = generateKeyPairSync('ec', {namedCurve: 'prime256v1'});
	const jwk = privateKey.export({format: 'jwk'});
	const x = Buffer.from(jwk.x ?? '', 'base64url');
	const y = Buffer.from(jwk.y ?? '', 'base64url');
	const d = Buffer.from(jwk.d ?? '', 'base64url');
	return {
		publicKey: Buffer.concat([Buffer.from([0x04]), x, y]).toString('base64url'),
		privateKey: d.toString('base64url'),
	};
}

function stubMinimalEnv(overrides: Record<string, string> = {}): void {
	for (const [key, value] of Object.entries({...MINIMAL_ENV, ...overrides})) {
		vi.stubEnv(key, value);
	}
}

function clearFluxerEnv(): void {
	for (const key of Object.keys(process.env)) {
		if (key.startsWith('FLUXER_')) {
			vi.stubEnv(key, undefined);
		}
	}
}

describe('ConfigLoader', () => {
	beforeEach(() => {
		resetConfig();
		clearFluxerEnv();
	});

	afterEach(() => {
		resetConfig();
		vi.unstubAllEnvs();
	});

	test('loadConfig builds and caches config from FLUXER environment variables', async () => {
		stubMinimalEnv();
		const config = await loadConfig();
		expect(config.env).toBe('test');
		expect(config.domain.base_domain).toBe('localhost');
		expect(config.database.backend).toBe('postgres');
		expect(config.database.postgres.database).toBe('fluxer');
		expect(config.database.cassandra.hosts).toEqual(['127.0.0.1']);

		vi.stubEnv('FLUXER_BASE_DOMAIN', 'changed.example');
		expect((await loadConfig()).domain.base_domain).toBe('localhost');
	});

	test('getConfig throws when config is not loaded', () => {
		expect(() => getConfig()).toThrow('Config not loaded');
	});

	test('resetConfig clears the cache', async () => {
		stubMinimalEnv();
		await loadConfig();
		expect(() => getConfig()).not.toThrow();
		resetConfig();
		expect(() => getConfig()).toThrow('Config not loaded');
	});

	test('derives endpoints from domain config', async () => {
		stubMinimalEnv();
		const config = await loadConfig();
		expect(config.endpoints.api).toBe('http://localhost:8088/api');
		expect(config.endpoints.gateway).toBe('ws://localhost:8088/gateway');
	});

	test('endpoint overrides take precedence over derived endpoints', async () => {
		stubMinimalEnv({
			FLUXER_API_ENDPOINT: 'https://custom-api.example.com',
			FLUXER_API_CLIENT_ENDPOINT: 'https://custom-api-client.example.com',
			FLUXER_GATEWAY_ENDPOINT: 'wss://custom-gw.example.com',
		});

		const config = await loadConfig();

		expect(config.endpoints.api).toBe('https://custom-api.example.com');
		expect(config.endpoints.api_client).toBe('https://custom-api-client.example.com');
		expect(config.endpoints.gateway).toBe('wss://custom-gw.example.com');
		expect(config.endpoints.app).toBe('http://localhost:8088');
	});

	test('inserts the public port into portless endpoints on a non-standard port', async () => {
		stubMinimalEnv({
			FLUXER_MARKETING_ENDPOINT: 'http://localhost',
			FLUXER_MEDIA_ENDPOINT: 'http://localhost/media',
			FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: 'http://localhost/media',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'http://localhost',
		});

		const config = await loadConfig();

		expect(config.endpoints.marketing).toBe('http://localhost:8088');
		expect(config.endpoints.media).toBe('http://localhost:8088/media');
		expect(config.services.media_proxy.upload_relay.endpoint).toBe('http://localhost:8088/media');
		expect(config.auth.passkeys.additional_allowed_origins).toEqual(['http://localhost:8088']);
	});

	test('leaves a default https install untouched', async () => {
		stubMinimalEnv({
			FLUXER_BASE_DOMAIN: 'chat.example',
			FLUXER_PUBLIC_SCHEME: 'https',
			FLUXER_PUBLIC_PORT: '443',
			FLUXER_MARKETING_ENDPOINT: 'https://chat.example',
			FLUXER_MEDIA_ENDPOINT: 'https://chat.example/media',
			FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: 'https://chat.example/media',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'https://chat.example',
		});

		const config = await loadConfig();

		expect(config.endpoints.marketing).toBe('https://chat.example');
		expect(config.endpoints.media).toBe('https://chat.example/media');
		expect(config.endpoints.api).toBe('https://chat.example/api');
		expect(config.endpoints.gateway).toBe('wss://chat.example/gateway');
		expect(config.services.media_proxy.upload_relay.endpoint).toBe('https://chat.example/media');
		expect(config.auth.passkeys.additional_allowed_origins).toEqual(['https://chat.example']);
	});

	test('leaves a default http install untouched', async () => {
		stubMinimalEnv({
			FLUXER_BASE_DOMAIN: 'chat.example',
			FLUXER_PUBLIC_SCHEME: 'http',
			FLUXER_PUBLIC_PORT: '80',
			FLUXER_MARKETING_ENDPOINT: 'http://chat.example',
			FLUXER_MEDIA_ENDPOINT: 'http://chat.example/media',
			FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: 'http://chat.example/media',
		});

		const config = await loadConfig();

		expect(config.endpoints.marketing).toBe('http://chat.example');
		expect(config.endpoints.media).toBe('http://chat.example/media');
		expect(config.endpoints.gateway).toBe('ws://chat.example/gateway');
		expect(config.services.media_proxy.upload_relay.endpoint).toBe('http://chat.example/media');
	});

	test('leaves foreign hosts and already ported endpoints untouched', async () => {
		stubMinimalEnv({
			FLUXER_STATIC_CDN_ENDPOINT: 'https://cdn.example.net',
			FLUXER_MEDIA_ENDPOINT: 'https://media.example.net/media',
			FLUXER_MARKETING_ENDPOINT: 'http://localhost:9999',
			FLUXER_MEDIA_PROXY_UPLOAD_RELAY_ENDPOINT: 'http://localhost:8088/media',
		});

		const config = await loadConfig();

		expect(config.endpoints.static_cdn).toBe('https://cdn.example.net');
		expect(config.endpoints.media).toBe('https://media.example.net/media');
		expect(config.endpoints.marketing).toBe('http://localhost:9999');
		expect(config.services.media_proxy.upload_relay.endpoint).toBe('http://localhost:8088/media');
	});

	test('normalizes each passkey origin independently', async () => {
		stubMinimalEnv({
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS:
				'http://localhost,http://localhost:3000,https://desktop.example.net,android:apk-key-hash:abc',
		});

		const config = await loadConfig();

		expect(config.auth.passkeys.additional_allowed_origins).toEqual([
			'http://localhost:8088',
			'http://localhost:3000',
			'https://desktop.example.net',
			'android:apk-key-hash:abc',
		]);
	});

	test('leaves the default passkey origins untouched', async () => {
		stubMinimalEnv();
		const config = await loadConfig();
		expect(config.auth.passkeys.additional_allowed_origins).toEqual([
			'https://fluxer.app',
			'https://web.fluxer.app',
			'https://web.canary.fluxer.app',
			'android:apk-key-hash:keSY4bimyLqZQV7bKXgpa2xYuqXi0qZJzsYtp6gpx7w',
			'android:apk-key-hash:zRmCKDKo3uCX2GDZISjJx8Rzo3J-Y3Gbp7s7mAaUH28',
		]);
	});

	test('rejects an empty client API endpoint override', async () => {
		stubMinimalEnv({FLUXER_API_CLIENT_ENDPOINT: ''});
		await expect(loadConfig()).rejects.toThrow('FLUXER_API_CLIENT_ENDPOINT is required');
	});

	test('defaults the passkey relying party to the deployment domain', async () => {
		stubMinimalEnv({
			FLUXER_BASE_DOMAIN: 'chat.example.com',
			FLUXER_PUBLIC_SCHEME: 'https',
			FLUXER_PUBLIC_PORT: '443',
		});

		const config = await loadConfig();

		expect(config.auth.passkeys.rp_id).toBe('chat.example.com');
	});

	test('derives the passkey origin only when the operator clears the default list', async () => {
		stubMinimalEnv({
			FLUXER_BASE_DOMAIN: 'chat.example.com',
			FLUXER_PUBLIC_SCHEME: 'https',
			FLUXER_PUBLIC_PORT: '443',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: '',
		});

		const config = await loadConfig();

		expect(config.auth.passkeys.additional_allowed_origins).toEqual(['https://chat.example.com']);
	});

	test('keeps explicit passkey relying party values', async () => {
		stubMinimalEnv({
			FLUXER_BASE_DOMAIN: 'chat.example.com',
			FLUXER_PUBLIC_SCHEME: 'https',
			FLUXER_PUBLIC_PORT: '443',
			FLUXER_PASSKEY_RP_ID: 'example.com',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'https://example.com,https://app.example.com',
		});

		const config = await loadConfig();

		expect(config.auth.passkeys.rp_id).toBe('example.com');
		expect(config.auth.passkeys.additional_allowed_origins).toEqual(['https://example.com', 'https://app.example.com']);
	});

	test('parses typed named environment variables', async () => {
		stubMinimalEnv({
			FLUXER_API_PORT: '9090',
			FLUXER_CASSANDRA_HOSTS: 'db1,db2',
			FLUXER_POSTGRES_HOST: 'pg1',
			FLUXER_POSTGRES_PORT: '5544',
			FLUXER_POSTGRES_MAX_CONNECTIONS: '7',
			FLUXER_POSTGRES_SSL_CA: '-----BEGIN CERTIFICATE-----\\n-----END CERTIFICATE-----',
			FLUXER_POSTGRES_PREPARED_STATEMENTS: 'false',
			FLUXER_API_WORKER_MODE: 'single_task',
			FLUXER_API_WORKER_TASK: 'processStripeWebhook',
			FLUXER_ACCOUNT_POLICY_DSL: '{"version":1,"id":"env_policy","rules":[]}',
			FLUXER_LIVEKIT_ENABLED: 'true',
			FLUXER_LIVEKIT_DEFAULT_REGION:
				'{"id":"local","name":"Local","emoji":"LC","latitude":59.3293,"longitude":18.0686}',
		});

		const config = await loadConfig();

		expect(config.services.api.port).toBe(9090);
		expect(config.database.cassandra.hosts).toEqual(['db1', 'db2']);
		expect(config.database.postgres.host).toBe('pg1');
		expect(config.database.postgres.port).toBe(5544);
		expect(config.database.postgres.max_connections).toBe(7);
		expect(config.database.postgres.ssl_ca).toBe('-----BEGIN CERTIFICATE-----\\n-----END CERTIFICATE-----');
		expect(config.database.postgres.prepared_statements).toBe(false);
		expect(config.services.api.worker?.mode).toBe('single_task');
		expect(config.services.api.worker?.task).toBe('processStripeWebhook');
		expect(config.integrations.risk_integration.account_policy_dsl).toEqual({
			version: 1,
			id: 'env_policy',
			rules: [],
		});
		expect(config.integrations.voice.default_region?.id).toBe('local');
	});

	test('ignores FLUXER_GATEWAY_PUSH_ENABLED, which only the gateway reads', async () => {
		stubMinimalEnv({FLUXER_GATEWAY_PUSH_ENABLED: 'false'});

		const config = await loadConfig();

		expect(config.services.gateway).not.toHaveProperty('push_enabled');
	});

	test('rejects single task worker mode without task env', async () => {
		stubMinimalEnv({FLUXER_API_WORKER_MODE: 'single_task'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_API_WORKER_TASK');
	});

	test('rejects invalid Postgres typed environment values', async () => {
		stubMinimalEnv({FLUXER_POSTGRES_PORT: 'abc'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_POSTGRES_PORT');
	});

	test('rejects a non-integer port', async () => {
		stubMinimalEnv({FLUXER_API_PORT: '80a'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_API_PORT must be an integer, got "80a"');
	});

	test('rejects malformed JSON for a JSON-shaped variable', async () => {
		stubMinimalEnv({FLUXER_LIVEKIT_DEFAULT_REGION: '{bad'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_LIVEKIT_DEFAULT_REGION must be valid JSON');
	});

	test('keeps Postgres prepared statements on by default', async () => {
		stubMinimalEnv();
		expect((await loadConfig()).database.postgres.prepared_statements).toBe(true);
	});

	test('rejects a non-boolean Postgres prepared statements value', async () => {
		stubMinimalEnv({FLUXER_POSTGRES_PREPARED_STATEMENTS: 'maybe'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_POSTGRES_PREPARED_STATEMENTS');
	});

	test('keeps the api http timeouts at their defaults', async () => {
		stubMinimalEnv();
		const config = await loadConfig();
		expect(config.services.api.headers_timeout_ms).toBe(30_000);
		expect(config.services.api.request_timeout_ms).toBe(120_000);
	});

	test('reads the api http timeouts from the environment', async () => {
		stubMinimalEnv({FLUXER_API_HEADERS_TIMEOUT_MS: '45000', FLUXER_API_REQUEST_TIMEOUT_MS: '600000'});
		const config = await loadConfig();
		expect(config.services.api.headers_timeout_ms).toBe(45_000);
		expect(config.services.api.request_timeout_ms).toBe(600_000);
	});

	test('rejects a non-numeric api header timeout', async () => {
		stubMinimalEnv({FLUXER_API_HEADERS_TIMEOUT_MS: 'soon'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_API_HEADERS_TIMEOUT_MS');
	});

	test('rejects a non-numeric api request timeout', async () => {
		stubMinimalEnv({FLUXER_API_REQUEST_TIMEOUT_MS: 'soon'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_API_REQUEST_TIMEOUT_MS');
	});

	test('applies the KV mode from the environment', async () => {
		stubMinimalEnv({FLUXER_KV_MODE: 'cluster'});
		expect((await loadConfig()).internal.kv_mode).toBe('cluster');
	});

	test('rejects an unknown KV mode', async () => {
		stubMinimalEnv({FLUXER_KV_MODE: 'sentinel'});
		await expect(loadConfig()).rejects.toThrow('Invalid FLUXER_KV_MODE: sentinel');
	});

	test('rejects unsafe production Postgres defaults', async () => {
		stubMinimalEnv({FLUXER_ENV: 'production'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_POSTGRES_HOST');
	});

	test('accepts explicitly configured production Postgres with TLS', async () => {
		stubMinimalEnv({
			FLUXER_ENV: 'production',
			FLUXER_POSTGRES_HOST: 'postgres.internal',
			FLUXER_POSTGRES_DATABASE: 'fluxer_prod',
			FLUXER_POSTGRES_USERNAME: 'fluxer_app',
			FLUXER_POSTGRES_PASSWORD: 'prod-postgres-secret',
			FLUXER_POSTGRES_SSL: 'true',
		});

		const config = await loadConfig();

		expect(config.database.postgres.host).toBe('postgres.internal');
		expect(config.database.postgres.ssl).toBe(true);
	});

	test('allows self-hosted production Postgres without TLS', async () => {
		stubMinimalEnv({
			FLUXER_ENV: 'production',
			FLUXER_SELF_HOSTED: 'true',
			FLUXER_POSTGRES_HOST: 'postgres',
			FLUXER_POSTGRES_DATABASE: 'fluxer',
			FLUXER_POSTGRES_USERNAME: 'fluxer',
			FLUXER_POSTGRES_PASSWORD: 'self-hosted-postgres-secret',
			FLUXER_POSTGRES_SSL: 'false',
		});

		const config = await loadConfig();

		expect(config.instance.self_hosted).toBe(true);
		expect(config.database.postgres.ssl).toBe(false);
	});

	test('does not require a marketing secret for self-hosted instances', async () => {
		stubMinimalEnv({FLUXER_SELF_HOSTED: 'true'});
		vi.stubEnv('FLUXER_MARKETING_SECRET_KEY_BASE', undefined);

		const config = await loadConfig();

		expect(config.instance.self_hosted).toBe(true);
		expect(config.services.marketing.secret_key_base).toBe('');
	});

	test('requires a marketing secret for hosted instances', async () => {
		stubMinimalEnv();
		vi.stubEnv('FLUXER_MARKETING_SECRET_KEY_BASE', undefined);

		await expect(loadConfig()).rejects.toThrow('FLUXER_MARKETING_SECRET_KEY_BASE');
	});

	test('still requires TLS for non-self-hosted production Postgres', async () => {
		stubMinimalEnv({
			FLUXER_ENV: 'production',
			FLUXER_POSTGRES_HOST: 'postgres.internal',
			FLUXER_POSTGRES_DATABASE: 'fluxer_prod',
			FLUXER_POSTGRES_USERNAME: 'fluxer_app',
			FLUXER_POSTGRES_PASSWORD: 'prod-postgres-secret',
			FLUXER_POSTGRES_SSL: 'false',
		});

		await expect(loadConfig()).rejects.toThrow('FLUXER_POSTGRES_SSL must be true');
	});

	test('parses self-host branding, setup, abuse policy, and search engine environment variables', async () => {
		stubMinimalEnv({
			FLUXER_SEARCH_ENGINE: 'meilisearch',
			FLUXER_SEARCH_URL: 'http://meilisearch:7700',
			FLUXER_SEARCH_API_KEY: 'meili-key',
			FLUXER_SELF_HOSTED: 'true',
			FLUXER_APP_PRODUCT_NAME: 'Example Chat',
			FLUXER_APP_ICON_URL: 'https://assets.example/icon.png',
			FLUXER_APP_SYMBOL_URL: 'https://assets.example/symbol.png',
			FLUXER_APP_LOGO_URL: 'https://assets.example/logo.png',
			FLUXER_APP_WORDMARK_URL: 'https://assets.example/wordmark.png',
			FLUXER_APP_FAVICON_URL: 'https://assets.example/favicon.png',
			FLUXER_APP_THEME_COLOR: '#123456',
			FLUXER_INSTANCE_SETUP_CONFIGURED: 'true',
			FLUXER_ABUSE_INBOUND_PHONE_COUNTRY_CODES: 'AA,BB',
			FLUXER_ABUSE_PHONE_INBOUND_REQUIRED_PREFIXES: '+101,+202',
			FLUXER_ABUSE_DIRECT_CONTACT_SPAM_ENABLED: 'true',
			FLUXER_ABUSE_DIRECT_CONTACT_SPAM_COUNTRY_CODES: 'AA,BB',
			FLUXER_ABUSE_DIRECT_CONTACT_SPAM_DISTINCT_TARGET_THRESHOLD: '9',
			FLUXER_ABUSE_DIRECT_CONTACT_SPAM_TARGET_WINDOW_MS: '12345',
			FLUXER_ABUSE_DIRECT_CONTACT_SPAM_ACTION: 'suppress_delivery',
		});

		const config = await loadConfig();

		expect(config.integrations.search.engine).toBe('meilisearch');
		expect(config.integrations.search.url).toBe('http://meilisearch:7700');
		expect(config.integrations.search.api_key).toBe('meili-key');
		expect(config.instance.self_hosted).toBe(true);
		expect(config.instance.branding).toEqual({
			product_name: 'Example Chat',
			icon_url: 'https://assets.example/icon.png',
			symbol_url: 'https://assets.example/symbol.png',
			logo_url: 'https://assets.example/logo.png',
			wordmark_url: 'https://assets.example/wordmark.png',
			favicon_url: 'https://assets.example/favicon.png',
			theme_color: '#123456',
		});
		expect(config.instance.setup.configured).toBe(true);
		expect(config.instance.abuse_policy).toEqual({
			inbound_phone_country_codes: ['AA', 'BB'],
			phone_verification: {
				inbound_required_prefixes: ['+101', '+202'],
			},
			direct_contact_spam: {
				enabled: true,
				country_codes: ['AA', 'BB'],
				distinct_target_threshold: 9,
				target_window_ms: 12345,
				action: 'suppress_delivery',
			},
		});
	});

	test('rejects an enabled captcha with no keys for the selected provider', async () => {
		stubMinimalEnv({FLUXER_CAPTCHA_ENABLED: 'true', FLUXER_CAPTCHA_PROVIDER: 'hcaptcha'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_CAPTCHA_HCAPTCHA_SITE_KEY is required');
	});

	test('rejects an enabled captcha with a site key but no secret key', async () => {
		stubMinimalEnv({
			FLUXER_CAPTCHA_ENABLED: 'true',
			FLUXER_CAPTCHA_PROVIDER: 'turnstile',
			FLUXER_CAPTCHA_TURNSTILE_SITE_KEY: 'turnstile-site-key',
		});
		await expect(loadConfig()).rejects.toThrow('FLUXER_CAPTCHA_TURNSTILE_SECRET_KEY is required');
	});

	test('rejects an enabled captcha with no provider', async () => {
		stubMinimalEnv({FLUXER_CAPTCHA_ENABLED: 'true'});
		await expect(loadConfig()).rejects.toThrow(
			'FLUXER_CAPTCHA_PROVIDER must be hcaptcha or turnstile when FLUXER_CAPTCHA_ENABLED is true',
		);
	});

	test('accepts an enabled captcha with both keys for the selected provider', async () => {
		stubMinimalEnv({
			FLUXER_CAPTCHA_ENABLED: 'true',
			FLUXER_CAPTCHA_PROVIDER: 'hcaptcha',
			FLUXER_CAPTCHA_HCAPTCHA_SITE_KEY: 'hcaptcha-site-key',
			FLUXER_CAPTCHA_HCAPTCHA_SECRET_KEY: 'hcaptcha-secret-key',
		});

		const config = await loadConfig();

		expect(config.integrations.captcha.enabled).toBe(true);
		expect(config.integrations.captcha.hcaptcha?.secret_key).toBe('hcaptcha-secret-key');
	});

	test('leaves a disabled captcha unvalidated', async () => {
		stubMinimalEnv({FLUXER_CAPTCHA_PROVIDER: 'hcaptcha'});
		expect((await loadConfig()).integrations.captcha.enabled).toBe(false);
	});

	test('leaves Bluesky login off with no legal URLs by default', async () => {
		stubMinimalEnv();

		const config = await loadConfig();

		expect(config.auth.bluesky.enabled).toBe(false);
		expect(config.auth.bluesky.tos_uri).toBe('');
		expect(config.auth.bluesky.policy_uri).toBe('');
		expect(config.auth.bluesky.keys).toEqual([]);
	});

	test('applies explicit Bluesky legal URLs from the environment', async () => {
		stubMinimalEnv({
			FLUXER_AUTH_BLUESKY_ENABLED: 'true',
			FLUXER_AUTH_BLUESKY_TOS_URI: 'https://chat.example.com/terms',
			FLUXER_AUTH_BLUESKY_POLICY_URI: 'https://chat.example.com/privacy',
		});

		const config = await loadConfig();

		expect(config.auth.bluesky.enabled).toBe(true);
		expect(config.auth.bluesky.tos_uri).toBe('https://chat.example.com/terms');
		expect(config.auth.bluesky.policy_uri).toBe('https://chat.example.com/privacy');
	});

	test('reads the upload relay secret through the override table', async () => {
		const secret = Buffer.alloc(32, 9).toString('base64');
		stubMinimalEnv({FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: secret});

		const config = await loadConfig();

		expect(config.services.media_proxy.upload_relay.secret_base64).toBe(secret);
	});

	test('defaults the upload relay body limit to the media proxy ceiling', async () => {
		stubMinimalEnv();
		expect((await loadConfig()).services.media_proxy.upload_relay.max_body_bytes).toBe(524_288_000);
	});

	test('rejects a missing upload relay secret in upload mode', async () => {
		stubMinimalEnv();
		vi.stubEnv('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64', '');

		await expect(loadConfig()).rejects.toThrow(
			'FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 is required in upload mode',
		);
	});

	test('rejects a non-base64 upload relay secret', async () => {
		stubMinimalEnv({FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: 'not base64!'});
		await expect(loadConfig()).rejects.toThrow('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must be base64');
	});

	test('rejects an upload relay secret shorter than 32 bytes', async () => {
		stubMinimalEnv({FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64: Buffer.alloc(16, 7).toString('base64')});
		await expect(loadConfig()).rejects.toThrow(
			'FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64 must decode to at least 32 bytes',
		);
	});

	test('leaves the upload relay secret optional outside upload mode', async () => {
		stubMinimalEnv({FLUXER_MEDIA_PROXY_MODE: 'mp'});
		vi.stubEnv('FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64', '');

		const config = await loadConfig();

		expect(config.services.media_proxy.upload_relay.secret_base64).toBe('');
	});

	test('rejects a VAPID public key that is not a 65-byte uncompressed point', async () => {
		const {privateKey} = generateVapidPair();
		stubMinimalEnv({
			FLUXER_VAPID_PUBLIC_KEY: Buffer.alloc(64, 4).toString('base64url'),
			FLUXER_VAPID_PRIVATE_KEY: privateKey,
		});
		await expect(loadConfig()).rejects.toThrow(
			'FLUXER_VAPID_PUBLIC_KEY must be the base64url 65-byte uncompressed P-256 point',
		);
	});

	test('rejects a VAPID private key that is not a 32-byte scalar', async () => {
		const {publicKey} = generateVapidPair();
		stubMinimalEnv({
			FLUXER_VAPID_PUBLIC_KEY: publicKey,
			FLUXER_VAPID_PRIVATE_KEY: Buffer.alloc(31, 9).toString('base64url'),
		});
		await expect(loadConfig()).rejects.toThrow('FLUXER_VAPID_PRIVATE_KEY must be the base64url 32-byte P-256 scalar');
	});

	test('rejects a well formed VAPID scalar that does not derive the public point', async () => {
		const {publicKey} = generateVapidPair();
		const other = generateVapidPair();
		stubMinimalEnv({
			FLUXER_VAPID_PUBLIC_KEY: publicKey,
			FLUXER_VAPID_PRIVATE_KEY: other.privateKey,
		});
		await expect(loadConfig()).rejects.toThrow('FLUXER_VAPID_PRIVATE_KEY does not match FLUXER_VAPID_PUBLIC_KEY');
	});

	test('accepts a generated VAPID pair', async () => {
		const pair = generateVapidPair();
		stubMinimalEnv({
			FLUXER_VAPID_PUBLIC_KEY: pair.publicKey,
			FLUXER_VAPID_PRIVATE_KEY: pair.privateKey,
		});

		const config = await loadConfig();

		expect(config.auth.vapid.public_key).toBe(pair.publicKey);
		expect(config.auth.vapid.private_key).toBe(pair.privateKey);
	});

	test('requires a complete environment', async () => {
		vi.stubEnv('FLUXER_ENV', 'test');
		await expect(loadConfig()).rejects.toThrow();
	});
});
