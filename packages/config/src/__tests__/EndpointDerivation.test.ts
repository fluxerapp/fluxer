// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildUrl,
	type DomainConfig,
	deriveDomain,
	deriveEndpointsFromDomain,
	normalizePublicEndpoint,
} from '@fluxer/config/src/EndpointDerivation';
import {describe, expect, test} from 'vitest';

describe('buildUrl', () => {
	test('omits standard HTTP port (80)', () => {
		expect(buildUrl('http', 'example.com', 80, '/path')).toBe('http://example.com/path');
	});
	test('omits standard HTTPS port (443)', () => {
		expect(buildUrl('https', 'example.com', 443, '/path')).toBe('https://example.com/path');
	});
	test('omits standard WebSocket port (80)', () => {
		expect(buildUrl('ws', 'example.com', 80, '/gateway')).toBe('ws://example.com/gateway');
	});
	test('omits standard secure WebSocket port (443)', () => {
		expect(buildUrl('wss', 'example.com', 443, '/gateway')).toBe('wss://example.com/gateway');
	});
	test('includes non-standard port', () => {
		expect(buildUrl('http', 'localhost', 8088, '/api')).toBe('http://localhost:8088/api');
	});
	test('includes non-standard HTTPS port', () => {
		expect(buildUrl('https', 'example.com', 8443, '/api')).toBe('https://example.com:8443/api');
	});
	test('handles missing port', () => {
		expect(buildUrl('https', 'example.com', undefined, '/api')).toBe('https://example.com/api');
	});
	test('handles missing path', () => {
		expect(buildUrl('https', 'example.com', 443)).toBe('https://example.com');
	});
	test('handles empty path', () => {
		expect(buildUrl('https', 'example.com', 443, '')).toBe('https://example.com');
	});
	test('handles root path', () => {
		expect(buildUrl('https', 'example.com', 443, '/')).toBe('https://example.com/');
	});
});

describe('deriveDomain', () => {
	const baseConfig: DomainConfig = {
		base_domain: 'fluxer.dev',
		public_scheme: 'https',
		internal_scheme: 'http',
	};
	test('uses base domain for api endpoint', () => {
		expect(deriveDomain('api', baseConfig)).toBe('fluxer.dev');
	});
	test('uses base domain for app endpoint', () => {
		expect(deriveDomain('app', baseConfig)).toBe('fluxer.dev');
	});
	test('uses base domain for gateway endpoint', () => {
		expect(deriveDomain('gateway', baseConfig)).toBe('fluxer.dev');
	});
	test('uses base domain for media endpoint', () => {
		expect(deriveDomain('media', baseConfig)).toBe('fluxer.dev');
	});
	test('uses custom static CDN domain when specified', () => {
		const config = {...baseConfig, static_cdn_domain: 'cdn.fluxer.dev'};
		expect(deriveDomain('static_cdn', config)).toBe('cdn.fluxer.dev');
	});
	test('uses base domain for static CDN when custom domain not specified', () => {
		expect(deriveDomain('static_cdn', baseConfig)).toBe('fluxer.dev');
	});
	test('uses custom invite domain when specified', () => {
		const config = {...baseConfig, invite_domain: 'fluxer.gg'};
		expect(deriveDomain('invite', config)).toBe('fluxer.gg');
	});
	test('uses base domain for invite when custom domain not specified', () => {
		expect(deriveDomain('invite', baseConfig)).toBe('fluxer.dev');
	});
	test('uses custom gift domain when specified', () => {
		const config = {...baseConfig, gift_domain: 'fluxer.gift'};
		expect(deriveDomain('gift', config)).toBe('fluxer.gift');
	});
	test('uses base domain for gift when custom domain not specified', () => {
		expect(deriveDomain('gift', baseConfig)).toBe('fluxer.dev');
	});
});

describe('deriveEndpointsFromDomain', () => {
	describe('development environment (localhost)', () => {
		const devConfig: DomainConfig = {
			base_domain: 'localhost',
			public_scheme: 'http',
			internal_scheme: 'http',
			public_port: 8088,
			internal_port: 8088,
		};
		const endpoints = deriveEndpointsFromDomain(devConfig);
		test('derives api endpoint with port', () => {
			expect(endpoints.api).toBe('http://localhost:8088/api');
		});
		test('derives api client endpoint with port', () => {
			expect(endpoints.api_client).toBe('http://localhost:8088/api');
		});
		test('derives app endpoint with port', () => {
			expect(endpoints.app).toBe('http://localhost:8088');
		});
		test('derives gateway endpoint with ws scheme', () => {
			expect(endpoints.gateway).toBe('ws://localhost:8088/gateway');
		});
		test('derives media endpoint with port', () => {
			expect(endpoints.media).toBe('http://localhost:8088/media');
		});
		test('derives static CDN endpoint via public origin', () => {
			expect(endpoints.static_cdn).toBe('http://localhost:8088');
		});
		test('derives admin endpoint with port', () => {
			expect(endpoints.admin).toBe('http://localhost:8088/admin');
		});
		test('derives marketing endpoint with port', () => {
			expect(endpoints.marketing).toBe('http://localhost:8088/marketing');
		});
		test('derives invite endpoint with port', () => {
			expect(endpoints.invite).toBe('http://localhost:8088/invite');
		});
		test('derives gift endpoint with port', () => {
			expect(endpoints.gift).toBe('http://localhost:8088/gift');
		});
	});
	describe('production environment (standard HTTPS port)', () => {
		const prodConfig: DomainConfig = {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			internal_port: 8080,
		};
		const endpoints = deriveEndpointsFromDomain(prodConfig);
		test('derives api endpoint without port', () => {
			expect(endpoints.api).toBe('https://fluxer.app/api');
		});
		test('derives api client endpoint without port', () => {
			expect(endpoints.api_client).toBe('https://fluxer.app/api');
		});
		test('derives app endpoint without port', () => {
			expect(endpoints.app).toBe('https://fluxer.app');
		});
		test('derives gateway endpoint with wss scheme without port', () => {
			expect(endpoints.gateway).toBe('wss://fluxer.app/gateway');
		});
		test('derives media endpoint without port', () => {
			expect(endpoints.media).toBe('https://fluxer.app/media');
		});
		test('derives static CDN endpoint without port', () => {
			expect(endpoints.static_cdn).toBe('https://fluxer.app');
		});
		test('derives admin endpoint without port', () => {
			expect(endpoints.admin).toBe('https://fluxer.app/admin');
		});
		test('derives marketing endpoint without port', () => {
			expect(endpoints.marketing).toBe('https://fluxer.app/marketing');
		});
		test('derives invite endpoint without port', () => {
			expect(endpoints.invite).toBe('https://fluxer.app/invite');
		});
		test('derives gift endpoint without port', () => {
			expect(endpoints.gift).toBe('https://fluxer.app/gift');
		});
	});
	describe('staging environment (custom port)', () => {
		const stagingConfig: DomainConfig = {
			base_domain: 'staging.fluxer.dev',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 8443,
			internal_port: 8080,
		};
		const endpoints = deriveEndpointsFromDomain(stagingConfig);
		test('derives api endpoint with custom port', () => {
			expect(endpoints.api).toBe('https://staging.fluxer.dev:8443/api');
		});
		test('derives api client endpoint with custom port', () => {
			expect(endpoints.api_client).toBe('https://staging.fluxer.dev:8443/api');
		});
		test('derives app endpoint with custom port', () => {
			expect(endpoints.app).toBe('https://staging.fluxer.dev:8443');
		});
		test('derives gateway endpoint with wss and custom port', () => {
			expect(endpoints.gateway).toBe('wss://staging.fluxer.dev:8443/gateway');
		});
	});
	describe('custom CDN domain', () => {
		const staticCdnConfig: DomainConfig = {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			static_cdn_domain: 'cdn.fluxer.app',
		};
		const endpoints = deriveEndpointsFromDomain(staticCdnConfig);
		test('uses custom CDN domain', () => {
			expect(endpoints.static_cdn).toBe('https://cdn.fluxer.app');
		});
		test('other endpoints use base domain', () => {
			expect(endpoints.api).toBe('https://fluxer.app/api');
			expect(endpoints.app).toBe('https://fluxer.app');
		});
	});
	describe('custom invite and gift domains', () => {
		const customConfig: DomainConfig = {
			base_domain: 'fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			invite_domain: 'fluxer.gg',
			gift_domain: 'fluxer.gift',
		};
		const endpoints = deriveEndpointsFromDomain(customConfig);
		test('uses custom invite domain', () => {
			expect(endpoints.invite).toBe('https://fluxer.gg/invite');
		});
		test('uses custom gift domain', () => {
			expect(endpoints.gift).toBe('https://fluxer.gift/gift');
		});
		test('other endpoints use base domain', () => {
			expect(endpoints.api).toBe('https://fluxer.app/api');
			expect(endpoints.app).toBe('https://fluxer.app');
		});
	});
	describe('WebSocket scheme derivation', () => {
		test('derives ws from http', () => {
			const config: DomainConfig = {
				base_domain: 'localhost',
				public_scheme: 'http',
				internal_scheme: 'http',
				public_port: 8088,
			};
			const endpoints = deriveEndpointsFromDomain(config);
			expect(endpoints.gateway).toBe('ws://localhost:8088/gateway');
		});
		test('derives wss from https', () => {
			const config: DomainConfig = {
				base_domain: 'fluxer.app',
				public_scheme: 'https',
				internal_scheme: 'http',
				public_port: 443,
			};
			const endpoints = deriveEndpointsFromDomain(config);
			expect(endpoints.gateway).toBe('wss://fluxer.app/gateway');
		});
	});
	describe('canary environment', () => {
		const canaryConfig: DomainConfig = {
			base_domain: 'canary.fluxer.app',
			public_scheme: 'https',
			internal_scheme: 'http',
			public_port: 443,
			static_cdn_domain: 'cdn-canary.fluxer.app',
		};
		const endpoints = deriveEndpointsFromDomain(canaryConfig);
		test('derives api endpoint for canary', () => {
			expect(endpoints.api).toBe('https://canary.fluxer.app/api');
		});
		test('derives app endpoint for canary', () => {
			expect(endpoints.app).toBe('https://canary.fluxer.app');
		});
		test('derives gateway endpoint for canary', () => {
			expect(endpoints.gateway).toBe('wss://canary.fluxer.app/gateway');
		});
		test('uses custom CDN domain for canary', () => {
			expect(endpoints.static_cdn).toBe('https://cdn-canary.fluxer.app');
		});
	});
	describe('edge cases', () => {
		test('handles standard HTTP port (80)', () => {
			const config: DomainConfig = {
				base_domain: 'example.com',
				public_scheme: 'http',
				internal_scheme: 'http',
				public_port: 80,
			};
			const endpoints = deriveEndpointsFromDomain(config);
			expect(endpoints.api).toBe('http://example.com/api');
			expect(endpoints.gateway).toBe('ws://example.com/gateway');
		});
		test('handles ports when undefined', () => {
			const config: DomainConfig = {
				base_domain: 'example.com',
				public_scheme: 'https',
				internal_scheme: 'http',
			};
			const endpoints = deriveEndpointsFromDomain(config);
			expect(endpoints.api).toBe('https://example.com/api');
			expect(endpoints.app).toBe('https://example.com');
		});
		test('handles IPv4 addresses', () => {
			const config: DomainConfig = {
				base_domain: '127.0.0.1',
				public_scheme: 'http',
				internal_scheme: 'http',
				public_port: 8088,
			};
			const endpoints = deriveEndpointsFromDomain(config);
			expect(endpoints.api).toBe('http://127.0.0.1:8088/api');
		});
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
