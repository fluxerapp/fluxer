// SPDX-License-Identifier: AGPL-3.0-or-later

import {buildNamedFluxerEnvOverrides, setNestedValue} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {describe, expect, test} from 'vitest';

describe('setNestedValue', () => {
	test('sets a top-level key', () => {
		const target: Record<string, unknown> = {};
		setNestedValue(target, ['port'], 8080);
		expect(target).toEqual({port: 8080});
	});
	test('sets a nested key', () => {
		const target: Record<string, unknown> = {};
		setNestedValue(target, ['database', 'host'], 'localhost');
		expect(target).toEqual({database: {host: 'localhost'}});
	});
	test('sets a deeply nested key', () => {
		const target: Record<string, unknown> = {};
		setNestedValue(target, ['a', 'b', 'c'], 'deep');
		expect(target).toEqual({a: {b: {c: 'deep'}}});
	});
	test('does nothing for empty keys', () => {
		const target: Record<string, unknown> = {existing: true};
		setNestedValue(target, [], 'value');
		expect(target).toEqual({existing: true});
	});
	test('overwrites non-object intermediate values', () => {
		const target: Record<string, unknown> = {a: 'string'};
		setNestedValue(target, ['a', 'b'], 'nested');
		expect(target).toEqual({a: {b: 'nested'}});
	});
	test('creates arrays for numeric path keys', () => {
		const target: Record<string, unknown> = {};
		setNestedValue(target, ['auth', 'bluesky', 'keys', 0, 'kid'], 'key-1');
		setNestedValue(target, ['auth', 'bluesky', 'keys', 0, 'private_key_path'], '/etc/fluxer/keys/key.pem');
		expect(target).toEqual({
			auth: {
				bluesky: {
					keys: [{kid: 'key-1', private_key_path: '/etc/fluxer/keys/key.pem'}],
				},
			},
		});
	});
});

describe('buildNamedFluxerEnvOverrides', () => {
	test('builds canonical split env overrides', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_BASE_DOMAIN: 'canonical.example',
			FLUXER_API_ENDPOINT: 'https://canonical.example/api',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'https://a.example, https://b.example',
			FLUXER_S3_FORCE_PATH_STYLE: 'true',
			FLUXER_AUTH_BLUESKY_KEYS: '[{"kid":"key-1","private_key_path":"/etc/fluxer/keys/bluesky.pem"}]',
			FLUXER_STRIPE_PRICE_MONTHLY_USD: 'price_monthly_usd',
		});

		expect(overrides).toMatchObject({
			domain: {base_domain: 'canonical.example'},
			endpoint_overrides: {api: 'https://canonical.example/api'},
			auth: {
				passkeys: {additional_allowed_origins: ['https://a.example', 'https://b.example']},
				bluesky: {keys: [{kid: 'key-1', private_key_path: '/etc/fluxer/keys/bluesky.pem'}]},
			},
			s3: {force_path_style: true},
			integrations: {stripe: {prices: {monthly_usd: 'price_monthly_usd'}}},
		});
	});

	test.each(['', ' ', '\t\n'])('treats %j as unset for every value type', (blank) => {
		expect(
			buildNamedFluxerEnvOverrides({
				FLUXER_BASE_DOMAIN: blank,
				FLUXER_API_PORT: blank,
				FLUXER_S3_FORCE_PATH_STYLE: blank,
				FLUXER_API_IP_BAN_EXEMPT_IPS: blank,
				FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: blank,
				FLUXER_LIVEKIT_DEFAULT_REGION: blank,
				FLUXER_AUTH_BLUESKY_KEYS: blank,
				FLUXER_EMAIL_FROM_NAME: blank,
			}),
		).toEqual({});
	});

	test('a blank canonical name falls through to its alias', () => {
		expect(
			buildNamedFluxerEnvOverrides({
				FLUXER_NATS_URL: '',
				FLUXER_NATS_CORE_URL: 'nats://alias',
				FLUXER_IPINFO_API_KEY: ' ',
				FLUXER_RISK_IPINFO_API_KEY: 'alias-key',
			}),
		).toMatchObject({
			services: {nats: {core_url: 'nats://alias'}},
			integrations: {ipinfo: {api_key: 'alias-key'}},
		});
	});

	test('a blank alias is unset too', () => {
		expect(buildNamedFluxerEnvOverrides({FLUXER_NATS_URL: '', FLUXER_NATS_CORE_URL: ' '})).toEqual({});
	});

	test('none clears the storage change feed skip list', () => {
		expect(buildNamedFluxerEnvOverrides({FLUXER_API_STORAGE_CHANGE_FEED_SKIP_BUCKETS: ' None '})).toEqual({
			services: {api: {storage_change_feed: {skip_buckets: []}}},
		});
	});

	test('rejects a non-integer value for an integer override', () => {
		expect(() => buildNamedFluxerEnvOverrides({FLUXER_API_PORT: '80a'})).toThrow(
			'FLUXER_API_PORT must be an integer, got "80a"',
		);
	});

	test('the canonical name wins over its alias regardless of declaration order', () => {
		expect(
			buildNamedFluxerEnvOverrides({
				FLUXER_MEDIA_PROXY_ENDPOINT: 'http://alias',
				FLUXER_INTERNAL_MEDIA_PROXY_ENDPOINT: 'http://canonical',
				FLUXER_NATS_CORE_URL: 'nats://alias',
				FLUXER_NATS_URL: 'nats://canonical',
			}),
		).toMatchObject({
			internal: {media_proxy: 'http://canonical'},
			services: {nats: {core_url: 'nats://canonical'}},
		});
	});

	test('an alias alone still applies', () => {
		expect(
			buildNamedFluxerEnvOverrides({
				FLUXER_MEDIA_PROXY_ENDPOINT: 'http://alias',
				FLUXER_NATS_CORE_URL: 'nats://alias',
			}),
		).toMatchObject({
			internal: {media_proxy: 'http://alias'},
			services: {nats: {core_url: 'nats://alias'}},
		});
	});

	test('rejects malformed JSON for a JSON-shaped override', () => {
		expect(() => buildNamedFluxerEnvOverrides({FLUXER_LIVEKIT_DEFAULT_REGION: '{bad'})).toThrow(
			'FLUXER_LIVEKIT_DEFAULT_REGION must be valid JSON',
		);
	});

	test('parses the legacy Stripe price map onto integrations.stripe.legacy_prices', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_STRIPE_LEGACY_PRICES:
				'{"monthly_brl":["price_old_monthly_brl","price_older_monthly_brl"],"yearly_brl":["price_old_yearly_brl"],"monthly_try":["price_archived_monthly_try"],"gift_1_month_brl":["price_old_gift_1_month_brl"],"gift_1_year_brl":["price_old_gift_1_year_brl"]}',
		});

		expect(overrides).toMatchObject({
			integrations: {
				stripe: {
					legacy_prices: {
						monthly_brl: ['price_old_monthly_brl', 'price_older_monthly_brl'],
						yearly_brl: ['price_old_yearly_brl'],
						monthly_try: ['price_archived_monthly_try'],
						gift_1_month_brl: ['price_old_gift_1_month_brl'],
						gift_1_year_brl: ['price_old_gift_1_year_brl'],
					},
				},
			},
		});
	});

	test('the legacy Stripe price map does not disturb the live price map', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_STRIPE_PRICES: '{"monthly_brl":"price_new_monthly_brl"}',
			FLUXER_STRIPE_LEGACY_PRICES: '{"monthly_brl":["price_old_monthly_brl"]}',
		});

		const stripe = (overrides.integrations as {stripe: Record<string, unknown>}).stripe;
		expect(stripe.prices).toEqual({monthly_brl: 'price_new_monthly_brl'});
		expect(stripe.legacy_prices).toEqual({monthly_brl: ['price_old_monthly_brl']});
	});

	test('rejects malformed JSON for the legacy Stripe price map', () => {
		expect(() => buildNamedFluxerEnvOverrides({FLUXER_STRIPE_LEGACY_PRICES: '{"monthly_brl":['})).toThrow(
			'FLUXER_STRIPE_LEGACY_PRICES must be valid JSON',
		);
	});

	test('an individual Stripe price env var wins over the blob, and the blob keys it does not name survive', () => {
		// The deploy runbook flips one slot at a time with FLUXER_STRIPE_PRICE_* while the blob stays
		// pinned to the previous release. The individual variable is declared after the blob, so it is
		// merged into the blob rather than replaced by it.
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_STRIPE_PRICES:
				'{"monthly_brl":"price_blob_monthly_brl","yearly_brl":"price_blob_yearly_brl","monthly_usd":"price_blob_monthly_usd"}',
			FLUXER_STRIPE_PRICE_MONTHLY_BRL: 'price_individual_monthly_brl',
		});

		expect((overrides.integrations as {stripe: {prices: unknown}}).stripe.prices).toEqual({
			monthly_brl: 'price_individual_monthly_brl',
			yearly_brl: 'price_blob_yearly_brl',
			monthly_usd: 'price_blob_monthly_usd',
		});
	});

	test('several individual Stripe price env vars merge into the blob together', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_STRIPE_PRICES: '{"monthly_brl":"price_blob_monthly_brl","yearly_brl":"price_blob_yearly_brl"}',
			FLUXER_STRIPE_PRICE_MONTHLY_BRL: 'price_individual_monthly_brl',
			FLUXER_STRIPE_PRICE_YEARLY_BRL: 'price_individual_yearly_brl',
			FLUXER_STRIPE_PRICE_MONTHLY_EUR: 'price_individual_monthly_eur',
		});

		expect((overrides.integrations as {stripe: {prices: unknown}}).stripe.prices).toEqual({
			monthly_brl: 'price_individual_monthly_brl',
			yearly_brl: 'price_individual_yearly_brl',
			monthly_eur: 'price_individual_monthly_eur',
		});
	});

	test('the Stripe price blob applies on its own when no individual price var is set', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_STRIPE_PRICES: '{"monthly_brl":"price_blob_monthly_brl","yearly_brl":"price_blob_yearly_brl"}',
		});

		expect((overrides.integrations as {stripe: {prices: unknown}}).stripe.prices).toEqual({
			monthly_brl: 'price_blob_monthly_brl',
			yearly_brl: 'price_blob_yearly_brl',
		});
	});
});
