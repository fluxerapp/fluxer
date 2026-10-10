// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {
	buildUrl,
	type DerivedEndpoints,
	type DomainConfig,
	deriveEndpointsFromDomain,
	parsePublicOrigin,
} from '@fluxer/config/src/EndpointDerivation';
import {describe, expect, test} from 'vitest';

interface EndpointScenario {
	name: string;
	config: DomainConfig;
	expected: DerivedEndpoints;
}

const endpointScenarios: Array<EndpointScenario> = [
	{
		name: 'external CDN uses HTTPS without the public port',
		config: {
			base_domain: 'localhost',
			public_scheme: 'http',
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
		const endpoints = deriveEndpointsFromDomain(origin);
		expect(endpoints.api_client).toBe('https://chat.example.com:29080/api');
		expect(endpoints.app).toBe('https://chat.example.com:29080');
		expect(endpoints.gateway).toBe('wss://chat.example.com:29080/gateway');
		expect(endpoints.admin).toBe('https://chat.example.com:29080/admin');
	});
	test('an origin written with an explicit :443 derives portless endpoints', () => {
		const origin = parsePublicOrigin('https://chat.example.com:443');
		assert.ok(origin);
		const endpoints = deriveEndpointsFromDomain(origin);
		expect(endpoints.admin).toBe('https://chat.example.com/admin');
		expect(endpoints.app).toBe('https://chat.example.com');
		expect(endpoints.gateway).toBe('wss://chat.example.com/gateway');
	});
});
