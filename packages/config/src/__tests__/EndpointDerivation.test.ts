// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	buildUrl,
	canonicalizeDomain,
	type DerivedEndpoints,
	type DomainConfig,
	deriveDomain,
	deriveEndpointsFromDomain,
	normalizePublicEndpoint,
	parsePublicOrigin,
} from '@fluxer/config/src/EndpointDerivation';
import {describe, expect, test} from 'vitest';

describe('buildUrl', () => {
	test.each([
		['http', 'example.com', 80, '/path', 'http://example.com/path'],
		['https', 'example.com', 443, '/path', 'https://example.com/path'],
		['ws', 'example.com', 80, '/gateway', 'ws://example.com/gateway'],
		['wss', 'example.com', 443, '/gateway', 'wss://example.com/gateway'],
		['http', 'localhost', 8088, '/api', 'http://localhost:8088/api'],
		['https', 'example.com', 8443, '/api', 'https://example.com:8443/api'],
		['https', 'example.com', undefined, '/api', 'https://example.com/api'],
		['https', 'example.com', 443, undefined, 'https://example.com'],
		['https', 'example.com', 443, '', 'https://example.com'],
		['https', 'example.com', 443, '/', 'https://example.com/'],
	] as const)('%s %s:%s%s becomes %s', (scheme, domain, port, path, expected) => {
		expect(buildUrl(scheme, domain, port, path)).toBe(expected);
	});
});

describe('deriveDomain', () => {
	const baseConfig: DomainConfig = {
		base_domain: 'fluxer.dev',
		public_scheme: 'https',
		internal_scheme: 'http',
	};
	test.each([
		'api',
		'api_client',
		'app',
		'gateway',
		'media',
		'static_cdn',
		'admin',
		'docs',
		'marketing',
		'invite',
		'gift',
	] as const)('uses the base domain for %s without an override', (endpoint) => {
		expect(deriveDomain(endpoint, baseConfig)).toBe('fluxer.dev');
	});
	test.each([
		['static_cdn', {static_cdn_domain: 'cdn.fluxer.dev'}, 'cdn.fluxer.dev'],
		['invite', {invite_domain: 'fluxer.gg'}, 'fluxer.gg'],
		['gift', {gift_domain: 'fluxer.gift'}, 'fluxer.gift'],
	] as const)('uses the custom %s domain', (endpoint, overrides, expected) => {
		expect(deriveDomain(endpoint, {...baseConfig, ...overrides})).toBe(expected);
	});
});

interface EndpointScenario {
	name: string;
	config: DomainConfig;
	expected: DerivedEndpoints;
}

const endpointScenarios: Array<EndpointScenario> = [
	{
		name: 'development',
		config: {
			base_domain: 'localhost',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 8088,
			internal_port: 8088,
		},
		expected: {
			api: 'http://localhost:8088/api',
			api_client: 'http://localhost:8088/api',
			app: 'http://localhost:8088',
			gateway: 'ws://localhost:8088/gateway',
			media: 'http://localhost:8088/media',
			static_cdn: 'http://localhost:8088',
			admin: 'http://localhost:8088/admin',
			docs: 'https://fluxer.dev',
			marketing: 'http://localhost:8088/marketing',
			invite: 'http://localhost:8088/invite',
			gift: 'http://localhost:8088/gift',
		},
	},
	{
		name: 'production',
		config: {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			internal_port: 8080,
		},
		expected: {
			api: 'https://fluxer.app/api',
			api_client: 'https://fluxer.app/api',
			app: 'https://fluxer.app',
			gateway: 'wss://fluxer.app/gateway',
			media: 'https://fluxer.app/media',
			static_cdn: 'https://fluxer.app',
			admin: 'https://fluxer.app/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://fluxer.app/marketing',
			invite: 'https://fluxer.app/invite',
			gift: 'https://fluxer.app/gift',
		},
	},
	{
		name: 'staging',
		config: {
			base_domain: 'staging.fluxer.dev',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 8443,
			internal_port: 8080,
		},
		expected: {
			api: 'https://staging.fluxer.dev:8443/api',
			api_client: 'https://staging.fluxer.dev:8443/api',
			app: 'https://staging.fluxer.dev:8443',
			gateway: 'wss://staging.fluxer.dev:8443/gateway',
			media: 'https://staging.fluxer.dev:8443/media',
			static_cdn: 'https://staging.fluxer.dev:8443',
			admin: 'https://staging.fluxer.dev:8443/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://staging.fluxer.dev:8443/marketing',
			invite: 'https://staging.fluxer.dev:8443/invite',
			gift: 'https://staging.fluxer.dev:8443/gift',
		},
	},
	{
		name: 'custom CDN',
		config: {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			static_cdn_domain: 'cdn.fluxer.app',
		},
		expected: {
			api: 'https://fluxer.app/api',
			api_client: 'https://fluxer.app/api',
			app: 'https://fluxer.app',
			gateway: 'wss://fluxer.app/gateway',
			media: 'https://fluxer.app/media',
			static_cdn: 'https://cdn.fluxer.app',
			admin: 'https://fluxer.app/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://fluxer.app/marketing',
			invite: 'https://fluxer.app/invite',
			gift: 'https://fluxer.app/gift',
		},
	},
	{
		name: 'custom invite and gift domains',
		config: {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			invite_domain: 'fluxer.gg',
			gift_domain: 'fluxer.gift',
		},
		expected: {
			api: 'https://fluxer.app/api',
			api_client: 'https://fluxer.app/api',
			app: 'https://fluxer.app',
			gateway: 'wss://fluxer.app/gateway',
			media: 'https://fluxer.app/media',
			static_cdn: 'https://fluxer.app',
			admin: 'https://fluxer.app/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://fluxer.app/marketing',
			invite: 'https://fluxer.gg/invite',
			gift: 'https://fluxer.gift/gift',
		},
	},
	{
		name: 'canary',
		config: {
			base_domain: 'canary.fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			static_cdn_domain: 'cdn-canary.fluxer.app',
		},
		expected: {
			api: 'https://canary.fluxer.app/api',
			api_client: 'https://canary.fluxer.app/api',
			app: 'https://canary.fluxer.app',
			gateway: 'wss://canary.fluxer.app/gateway',
			media: 'https://canary.fluxer.app/media',
			static_cdn: 'https://cdn-canary.fluxer.app',
			admin: 'https://canary.fluxer.app/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://canary.fluxer.app/marketing',
			invite: 'https://canary.fluxer.app/invite',
			gift: 'https://canary.fluxer.app/gift',
		},
	},
	{
		name: 'standard HTTP port',
		config: {
			base_domain: 'example.com',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 80,
		},
		expected: {
			api: 'http://example.com/api',
			api_client: 'http://example.com/api',
			app: 'http://example.com',
			gateway: 'ws://example.com/gateway',
			media: 'http://example.com/media',
			static_cdn: 'http://example.com',
			admin: 'http://example.com/admin',
			docs: 'https://fluxer.dev',
			marketing: 'http://example.com/marketing',
			invite: 'http://example.com/invite',
			gift: 'http://example.com/gift',
		},
	},
	{
		name: 'unspecified port',
		config: {
			base_domain: 'example.com',
			public_scheme: 'https',
			internal_scheme: 'http',
		},
		expected: {
			api: 'https://example.com/api',
			api_client: 'https://example.com/api',
			app: 'https://example.com',
			gateway: 'wss://example.com/gateway',
			media: 'https://example.com/media',
			static_cdn: 'https://example.com',
			admin: 'https://example.com/admin',
			docs: 'https://fluxer.dev',
			marketing: 'https://example.com/marketing',
			invite: 'https://example.com/invite',
			gift: 'https://example.com/gift',
		},
	},
	{
		name: 'IPv4',
		config: {
			base_domain: '127.0.0.1',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 8088,
		},
		expected: {
			api: 'http://127.0.0.1:8088/api',
			api_client: 'http://127.0.0.1:8088/api',
			app: 'http://127.0.0.1:8088',
			gateway: 'ws://127.0.0.1:8088/gateway',
			media: 'http://127.0.0.1:8088/media',
			static_cdn: 'http://127.0.0.1:8088',
			admin: 'http://127.0.0.1:8088/admin',
			docs: 'https://fluxer.dev',
			marketing: 'http://127.0.0.1:8088/marketing',
			invite: 'http://127.0.0.1:8088/invite',
			gift: 'http://127.0.0.1:8088/gift',
		},
	},
	{
		name: 'external CDN uses HTTPS without the public port',
		config: {
			base_domain: 'localhost',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 8088,
			static_cdn_domain: 'cdn.example.com',
		},
		expected: {
			api: 'http://localhost:8088/api',
			api_client: 'http://localhost:8088/api',
			app: 'http://localhost:8088',
			gateway: 'ws://localhost:8088/gateway',
			media: 'http://localhost:8088/media',
			static_cdn: 'https://cdn.example.com',
			admin: 'http://localhost:8088/admin',
			docs: 'https://fluxer.dev',
			marketing: 'http://localhost:8088/marketing',
			invite: 'http://localhost:8088/invite',
			gift: 'http://localhost:8088/gift',
		},
	},
];

describe('deriveEndpointsFromDomain', () => {
	test.each(endpointScenarios)('$name', ({config, expected}) => {
		expect(deriveEndpointsFromDomain(config)).toEqual(expected);
	});
});

describe('normalizePublicEndpoint', () => {
	test('leaves a default https install untouched', () => {
		expect(normalizePublicEndpoint('https://fluxer.dev', 'fluxer.dev', 443)).toBe('https://fluxer.dev');
		expect(normalizePublicEndpoint('https://fluxer.dev/media', 'fluxer.dev', 443)).toBe('https://fluxer.dev/media');
		expect(normalizePublicEndpoint('wss://fluxer.dev/gateway', 'fluxer.dev', 443)).toBe('wss://fluxer.dev/gateway');
	});
	test('leaves a default http install untouched', () => {
		expect(normalizePublicEndpoint('http://fluxer.dev', 'fluxer.dev', 80)).toBe('http://fluxer.dev');
		expect(normalizePublicEndpoint('http://fluxer.dev/media', 'fluxer.dev', 80)).toBe('http://fluxer.dev/media');
		expect(normalizePublicEndpoint('ws://fluxer.dev/gateway', 'fluxer.dev', 80)).toBe('ws://fluxer.dev/gateway');
	});
	test('inserts a non-standard port', () => {
		expect(normalizePublicEndpoint('https://fluxer.dev', 'fluxer.dev', 8443)).toBe('https://fluxer.dev:8443');
		expect(normalizePublicEndpoint('https://fluxer.dev/media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:8443/media',
		);
		expect(normalizePublicEndpoint('wss://fluxer.dev/gateway', 'fluxer.dev', 8443)).toBe(
			'wss://fluxer.dev:8443/gateway',
		);
	});
	test('judges standard ports against the url scheme, not the public scheme', () => {
		expect(normalizePublicEndpoint('http://fluxer.dev/media', 'fluxer.dev', 443)).toBe('http://fluxer.dev:443/media');
		expect(normalizePublicEndpoint('https://fluxer.dev/media', 'fluxer.dev', 80)).toBe('https://fluxer.dev:80/media');
	});
	test('leaves a foreign host untouched', () => {
		expect(normalizePublicEndpoint('https://cdn.example.net/media', 'fluxer.dev', 8443)).toBe(
			'https://cdn.example.net/media',
		);
		expect(normalizePublicEndpoint('https://sub.fluxer.dev', 'fluxer.dev', 8443)).toBe('https://sub.fluxer.dev');
	});
	test('leaves an already ported url untouched', () => {
		expect(normalizePublicEndpoint('https://fluxer.dev:8443/media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:8443/media',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev:9000/media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:9000/media',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev:443/media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:443/media',
		);
	});
	test('is idempotent', () => {
		const once = normalizePublicEndpoint('https://fluxer.dev/media', 'fluxer.dev', 8443);
		expect(normalizePublicEndpoint(once, 'fluxer.dev', 8443)).toBe(once);
	});
	test('preserves path, query, fragment, trailing slash, and case', () => {
		expect(normalizePublicEndpoint('https://fluxer.dev/Media/', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:8443/Media/',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev/media?a=B#Frag', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:8443/media?a=B#Frag',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev?a=B', 'fluxer.dev', 8443)).toBe('https://fluxer.dev:8443?a=B');
		expect(normalizePublicEndpoint('https://fluxer.dev#Frag', 'fluxer.dev', 8443)).toBe('https://fluxer.dev:8443#Frag');
		expect(normalizePublicEndpoint('https://user:pw@fluxer.dev/media', 'fluxer.dev', 8443)).toBe(
			'https://user:pw@fluxer.dev:8443/media',
		);
	});
	test('matches the host case-insensitively and ignores a trailing dot', () => {
		expect(normalizePublicEndpoint('https://FLUXER.dev/media', 'fluxer.dev', 8443)).toBe(
			'https://FLUXER.dev:8443/media',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev./media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev.:8443/media',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev/media', 'FLUXER.dev.', 8443)).toBe(
			'https://fluxer.dev:8443/media',
		);
	});
	test('leaves unparseable and non-http values untouched', () => {
		expect(normalizePublicEndpoint('not a url', 'fluxer.dev', 8443)).toBe('not a url');
		expect(normalizePublicEndpoint('', 'fluxer.dev', 8443)).toBe('');
		expect(normalizePublicEndpoint('android:apk-key-hash:abc', 'fluxer.dev', 8443)).toBe('android:apk-key-hash:abc');
		expect(normalizePublicEndpoint('https://fluxer.dev:/media', 'fluxer.dev', 8443)).toBe('https://fluxer.dev:/media');
	});
	test('leaves malformed authorities untouched', () => {
		expect(normalizePublicEndpoint('https:fluxer.dev/media', 'fluxer.dev', 8443)).toBe('https:fluxer.dev/media');
		expect(normalizePublicEndpoint('https:/fluxer.dev/media', 'fluxer.dev', 8443)).toBe('https:/fluxer.dev/media');
		expect(normalizePublicEndpoint('https:////fluxer.dev/media', 'fluxer.dev', 8443)).toBe(
			'https:////fluxer.dev/media',
		);
		expect(normalizePublicEndpoint('https://fluxer.dev\\media', 'fluxer.dev', 8443)).toBe(
			'https://fluxer.dev:8443\\media',
		);
	});
	test('leaves everything untouched without a usable port or base domain', () => {
		expect(normalizePublicEndpoint('https://fluxer.dev/media', 'fluxer.dev')).toBe('https://fluxer.dev/media');
		expect(normalizePublicEndpoint('https://fluxer.dev/media', '', 8443)).toBe('https://fluxer.dev/media');
		expect(normalizePublicEndpoint('https://fluxer.dev/media', '   ', 8443)).toBe('https://fluxer.dev/media');
	});
});

describe('canonicalizeDomain', () => {
	test('lowercases, trims and drops the root dot', () => {
		expect(canonicalizeDomain('  CHAT.Example.COM.  ')).toBe('chat.example.com');
	});
	test('leaves an empty value empty', () => {
		expect(canonicalizeDomain('   ')).toBe('');
	});
});

describe('parsePublicOrigin', () => {
	test('reads scheme, host and a non-standard port', () => {
		expect(parsePublicOrigin('https://chat.example.com:8443')).toEqual({
			public_scheme: 'https',
			base_domain: 'chat.example.com',
			public_port: 8443,
		});
		expect(parsePublicOrigin('http://chat.example.com:19080')).toEqual({
			public_scheme: 'http',
			base_domain: 'chat.example.com',
			public_port: 19080,
		});
	});
	test('fills in the standard port for a portless origin', () => {
		expect(parsePublicOrigin('https://chat.example.com')).toEqual({
			public_scheme: 'https',
			base_domain: 'chat.example.com',
			public_port: 443,
		});
		expect(parsePublicOrigin('http://chat.example.com')).toEqual({
			public_scheme: 'http',
			base_domain: 'chat.example.com',
			public_port: 80,
		});
	});
	test('normalizes an explicitly written standard port to the portless form', () => {
		const origin = parsePublicOrigin('https://chat.example.com:443');
		assert.ok(origin);
		expect(origin).toEqual({public_scheme: 'https', base_domain: 'chat.example.com', public_port: 443});
		expect(buildUrl(origin.public_scheme, origin.base_domain, origin.public_port)).toBe('https://chat.example.com');
		expect(parsePublicOrigin('http://chat.example.com:80')?.public_port).toBe(80);
	});
	test('canonicalizes the host', () => {
		expect(parsePublicOrigin('  https://CHAT.Example.com.:8443  ')).toEqual({
			public_scheme: 'https',
			base_domain: 'chat.example.com',
			public_port: 8443,
		});
	});
	test('keeps an IPv6 literal bracketed', () => {
		expect(parsePublicOrigin('http://[::1]:19080')).toEqual({
			public_scheme: 'http',
			base_domain: '[::1]',
			public_port: 19080,
		});
	});
	test('accepts a bare trailing slash', () => {
		expect(parsePublicOrigin('https://chat.example.com:8443/')?.public_port).toBe(8443);
	});
	test('rejects anything that is not a bare origin', () => {
		expect(parsePublicOrigin('')).toBeNull();
		expect(parsePublicOrigin('   ')).toBeNull();
		expect(parsePublicOrigin('not a url')).toBeNull();
		expect(parsePublicOrigin('chat.example.com:8443')).toBeNull();
		expect(parsePublicOrigin('wss://chat.example.com')).toBeNull();
		expect(parsePublicOrigin('https://chat.example.com/media')).toBeNull();
		expect(parsePublicOrigin('https://chat.example.com?a=1')).toBeNull();
		expect(parsePublicOrigin('https://chat.example.com#top')).toBeNull();
		expect(parsePublicOrigin('https://user:pw@chat.example.com')).toBeNull();
	});
});

describe('endpoints derived from a public origin', () => {
	test('an origin with a non-standard port ports every derived endpoint', () => {
		const origin = parsePublicOrigin('https://chat.example.com:29080');
		assert.ok(origin);
		const endpoints = deriveEndpointsFromDomain({
			...origin,
			internal_scheme: 'http',
		});
		expect(endpoints.api_client).toBe('https://chat.example.com:29080/api');
		expect(endpoints.app).toBe('https://chat.example.com:29080');
		expect(endpoints.gateway).toBe('wss://chat.example.com:29080/gateway');
		expect(endpoints.admin).toBe('https://chat.example.com:29080/admin');
	});
	test('an origin written with an explicit :443 derives portless endpoints', () => {
		const origin = parsePublicOrigin('https://chat.example.com:443');
		assert.ok(origin);
		const endpoints = deriveEndpointsFromDomain({
			...origin,
			internal_scheme: 'http',
		});
		expect(endpoints.admin).toBe('https://chat.example.com/admin');
		expect(endpoints.app).toBe('https://chat.example.com');
		expect(endpoints.gateway).toBe('wss://chat.example.com/gateway');
	});
});
