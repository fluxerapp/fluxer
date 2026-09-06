// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildNamedFluxerEnvOverrides,
	parseEnvValue,
	setNestedValue,
} from '@fluxer/config/src/config_loader/EnvironmentOverrides';
import {describe, expect, test} from 'vitest';

describe('parseEnvValue', () => {
	test('parses boolean true', () => {
		expect(parseEnvValue('true')).toBe(true);
		expect(parseEnvValue(' true ')).toBe(true);
	});
	test('parses boolean false', () => {
		expect(parseEnvValue('false')).toBe(false);
		expect(parseEnvValue(' false ')).toBe(false);
	});
	test('parses integers', () => {
		expect(parseEnvValue('42')).toBe(42);
		expect(parseEnvValue('-7')).toBe(-7);
		expect(parseEnvValue('0')).toBe(0);
	});
	test('parses floats', () => {
		expect(parseEnvValue('3.14')).toBe(3.14);
		expect(parseEnvValue('-0.5')).toBe(-0.5);
	});
	test('parses JSON objects', () => {
		expect(parseEnvValue('{"key": "value"}')).toEqual({key: 'value'});
	});
	test('parses JSON arrays', () => {
		expect(parseEnvValue('[1, 2, 3]')).toEqual([1, 2, 3]);
	});
	test('rejects invalid JSON-like values', () => {
		expect(() => parseEnvValue('{not json}')).toThrow('must be valid JSON');
	});
	test('returns raw string for plain strings', () => {
		expect(parseEnvValue('hello')).toBe('hello');
		expect(parseEnvValue('localhost')).toBe('localhost');
	});
});

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
	test('builds canonical split env overrides and preserves empty strings', () => {
		const overrides = buildNamedFluxerEnvOverrides({
			FLUXER_BASE_DOMAIN: 'canonical.example',
			FLUXER_API_ENDPOINT: 'https://canonical.example/api',
			FLUXER_PASSKEY_ADDITIONAL_ALLOWED_ORIGINS: 'https://a.example, https://b.example',
			FLUXER_S3_FORCE_PATH_STYLE: 'true',
			FLUXER_AUTH_BLUESKY_KEYS: '[{"kid":"key-1","private_key_path":"/etc/fluxer/keys/bluesky.pem"}]',
			FLUXER_ADMIN_BASE_PATH: '',
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
			services: {
				admin: {base_path: ''},
			},
			integrations: {stripe: {prices: {monthly_usd: 'price_monthly_usd'}}},
		});
	});

	test('maps the internal scheme and KV provider names', () => {
		expect(buildNamedFluxerEnvOverrides({FLUXER_INTERNAL_SCHEME: 'https', FLUXER_KV_PROVIDER: 'redis'})).toMatchObject({
			domain: {internal_scheme: 'https'},
			internal: {kv_provider: 'redis'},
		});
	});

	test('rejects a non-integer value for an integer override', () => {
		expect(() => buildNamedFluxerEnvOverrides({FLUXER_API_PORT: '80a'})).toThrow(
			'FLUXER_API_PORT must be an integer, got "80a"',
		);
	});

	test('leaves the default in place for a blank integer override', () => {
		expect(buildNamedFluxerEnvOverrides({FLUXER_API_PORT: ''})).toEqual({});
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
});
