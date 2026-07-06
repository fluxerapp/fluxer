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
	test('returns raw string for invalid JSON-like values', () => {
		expect(parseEnvValue('{not json}')).toBe('{not json}');
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
			FLUXER_SSO_AUTO_REDIRECT: 'true',
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
			instance: {sso: {auto_redirect: true}},
			integrations: {stripe: {prices: {monthly_usd: 'price_monthly_usd'}}},
		});
	});
});
