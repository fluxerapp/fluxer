// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import type {TrustedCallerConfig} from '@app/api/config/APIConfig';
import {RateLimitMiddleware, type RouteRateLimitConfig} from '@app/api/middleware/RateLimitMiddleware';
import {DonationRateLimitConfigs} from '@app/api/rate_limit_configs/DonationRateLimitConfig';
import {OAuthRateLimitConfigs} from '@app/api/rate_limit_configs/OAuthRateLimitConfig';
import type {HonoEnv} from '@app/api/types/HonoEnv';
import type {
	BucketConfig,
	IRateLimitService,
	RateLimitConfig,
	RateLimitResult,
} from '@pkgs/rate_limit/src/IRateLimitService';
import {Hono} from 'hono';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

function createAllowedResult(limit: number): RateLimitResult {
	return {
		allowed: true,
		limit,
		remaining: limit - 1,
		resetTime: new Date(Date.now() + 10000),
		resetAfterDecimal: 10,
	};
}

class RecordingRateLimitService implements IRateLimitService {
	readonly globalIdentifiers: Array<string> = [];
	readonly buckets: Array<string> = [];

	async checkLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return createAllowedResult(config.maxAttempts);
	}

	async peekLimit(config: RateLimitConfig): Promise<RateLimitResult> {
		return createAllowedResult(config.maxAttempts);
	}

	async checkBucketLimit(bucket: string, config: BucketConfig): Promise<RateLimitResult> {
		this.buckets.push(bucket);
		return createAllowedResult(config.limit);
	}

	async checkGlobalLimit(identifier: string, limit: number): Promise<RateLimitResult> {
		this.globalIdentifiers.push(identifier);
		return createAllowedResult(limit);
	}

	async resetLimit(_identifier: string): Promise<void> {}

	async clearLimitsByIdentifierPrefix(_identifierPrefix: string): Promise<number> {
		return 0;
	}
}

interface Harness {
	app: Hono<HonoEnv>;
	service: RecordingRateLimitService;
}

const CALLER_IP = '192.0.2.50';
const FORWARDED_IP = '203.0.113.77';
const OTHER_FORWARDED_IP = '203.0.113.78';
const BUGS_KEY = 'bugs-key-0123456789abcdefghijklmnopqrstuv';
const DONATION_KEY = 'donation-key-0123456789abcdefghijklmnopq';

const TRUSTED_CALLERS: Array<TrustedCallerConfig> = [
	{name: 'bugs', key: BUGS_KEY, buckets: ['oauth:token', 'oauth:revoke']},
	{
		name: 'donation',
		key: DONATION_KEY,
		buckets: ['donation:request_link', 'donation:manage', 'donation:checkout'],
	},
];

function buildTrustedHarness(routeConfig: RouteRateLimitConfig): Harness {
	const service = new RecordingRateLimitService();
	const app = new Hono<HonoEnv>({strict: true});
	app.use('*', async (ctx, next) => {
		ctx.set('rateLimitService', service);
		await next();
	});
	app.post('/route', RateLimitMiddleware(routeConfig), (ctx) => ctx.text('ok'));
	return {app, service};
}

async function callTrustedRoute(harness: Harness, headers: Record<string, string>): Promise<Response> {
	return await harness.app.request('http://localhost/route', {
		method: 'POST',
		headers: {
			'x-forwarded-for': CALLER_IP,
			'x-fluxer-test-enable-rate-limits': 'true',
			...headers,
		},
	});
}

describe('RateLimitMiddleware trusted callers', () => {
	let previousTrustedCallers: Array<TrustedCallerConfig>;

	beforeEach(() => {
		previousTrustedCallers = Config.internal.trustedCallers;
		Config.internal.trustedCallers = TRUSTED_CALLERS;
	});

	afterEach(() => {
		Config.internal.trustedCallers = previousTrustedCallers;
	});

	test('keys oauth:token on the address the bugs caller forwards, one bucket per address', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		const first = await callTrustedRoute(harness, {
			'x-fluxer-internal-key': BUGS_KEY,
			'x-fluxer-client-ip': FORWARDED_IP,
		});
		const second = await callTrustedRoute(harness, {
			'x-fluxer-internal-key': BUGS_KEY,
			'x-fluxer-client-ip': OTHER_FORWARDED_IP,
		});

		expect(first.status).toBe(200);
		expect(second.status).toBe(200);
		expect(harness.service.globalIdentifiers).toEqual([`ip:${FORWARDED_IP}`, `ip:${OTHER_FORWARDED_IP}`]);
		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:oauth:token`, `ip:${OTHER_FORWARDED_IP}:oauth:token`]);
	});

	test('keys oauth:revoke on the address the bugs caller forwards', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_REVOKE);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:oauth:revoke`]);
	});

	test('ignores the bugs key on a donation route', async () => {
		const harness = buildTrustedHarness(DonationRateLimitConfigs.DONATION_MANAGE);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-donor-ip': FORWARDED_IP});

		expect(harness.service.globalIdentifiers).toEqual([`ip:${CALLER_IP}`, `ip:${CALLER_IP}`]);
		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:donation:manage`, `ip:${CALLER_IP}:donation:manage`]);
	});

	test('ignores the donation key on oauth:token', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': DONATION_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:token`]);
	});

	test('accepts the donation key on its routes with the new and the old address header', async () => {
		for (const routeConfig of Object.values(DonationRateLimitConfigs)) {
			const harness = buildTrustedHarness(routeConfig);

			await callTrustedRoute(harness, {'x-fluxer-internal-key': DONATION_KEY, 'x-fluxer-client-ip': FORWARDED_IP});
			await callTrustedRoute(harness, {
				'x-fluxer-internal-key': DONATION_KEY,
				'x-fluxer-donor-ip': OTHER_FORWARDED_IP,
			});

			expect(harness.service.buckets).toEqual([
				`ip:${FORWARDED_IP}:${routeConfig.bucket}`,
				`ip:${OTHER_FORWARDED_IP}:${routeConfig.bucket}`,
			]);
		}
	});

	test('prefers the new address header when both are sent', async () => {
		const harness = buildTrustedHarness(DonationRateLimitConfigs.DONATION_MANAGE);

		await callTrustedRoute(harness, {
			'x-fluxer-internal-key': DONATION_KEY,
			'x-fluxer-client-ip': FORWARDED_IP,
			'x-fluxer-donor-ip': OTHER_FORWARDED_IP,
		});

		expect(harness.service.buckets).toEqual([`ip:${FORWARDED_IP}:donation:manage`]);
	});

	test('ignores a wrong key of the same or a different length, and a missing key', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);
		const sameLengthWrongKey = `${BUGS_KEY.slice(0, -1)}${BUGS_KEY.endsWith('v') ? 'w' : 'v'}`;

		await callTrustedRoute(harness, {'x-fluxer-internal-key': sameLengthWrongKey, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-internal-key': `${BUGS_KEY}x`, 'x-fluxer-client-ip': FORWARDED_IP});
		await callTrustedRoute(harness, {'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([
			`ip:${CALLER_IP}:oauth:token`,
			`ip:${CALLER_IP}:oauth:token`,
			`ip:${CALLER_IP}:oauth:token`,
		]);
	});

	test('falls back to the caller when a trusted key sends an unparsable address', async () => {
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_TOKEN);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': 'not-an-ip'});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:token`]);
	});

	test('ignores a trusted key on a route that does not opt in, even when the bucket is listed', async () => {
		Config.internal.trustedCallers = [{name: 'wide', key: BUGS_KEY, buckets: ['oauth:introspect']}];
		const harness = buildTrustedHarness(OAuthRateLimitConfigs.OAUTH_INTROSPECT);

		await callTrustedRoute(harness, {'x-fluxer-internal-key': BUGS_KEY, 'x-fluxer-client-ip': FORWARDED_IP});

		expect(harness.service.buckets).toEqual([`ip:${CALLER_IP}:oauth:introspect`]);
	});
});
