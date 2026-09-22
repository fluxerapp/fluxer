// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoRequestAuditEvent} from '@pkgs/geoip/src/IpInfoService';
import {
	createPostgresIpInfoCache,
	createPostgresIpInfoRequestAuditLogger,
	IPINFO_CACHE_TTL_SECONDS,
	IPINFO_REQUEST_AUDIT_TTL_SECONDS,
} from '@pkgs/geoip/src/PostgresIpInfoKv';
import type {IPostgresClient} from '@pkgs/postgres/src/Client';
import {describe, expect, it} from 'vitest';

function recordingClient(writes: Array<Array<unknown>>): IPostgresClient {
	return {
		async query(_text: string, values?: Array<unknown>) {
			writes.push(values ?? []);
			return {rows: [], rowCount: 1};
		},
		kvTable() {
			return 'kv';
		},
	} as never;
}

function expectExpiresIn(values: Array<unknown> | undefined, ttlSeconds: number): void {
	const expiresAt = values?.[4];
	expect(expiresAt).toBeInstanceOf(Date);
	const remainingSeconds = ((expiresAt as Date).getTime() - Date.now()) / 1000;
	expect(remainingSeconds).toBeGreaterThan(ttlSeconds - 10);
	expect(remainingSeconds).toBeLessThanOrEqual(ttlSeconds);
}

const EVENT: IpInfoRequestAuditEvent = {
	requestedAt: new Date('2026-09-21T12:00:00.000Z'),
	ip: '192.0.2.1',
	cacheKey: 'ip:192.0.2.1',
	source: 'test',
	reason: null,
	outcome: 'http_success',
	httpStatus: 200,
	available: true,
	riskNote: 'none',
	latencyMs: 12,
	requestUrl: 'https://ipinfo.test/192.0.2.1',
	responseIp: '192.0.2.1',
	countryCode: 'SE',
	asnNumber: 64500,
	isAnonymous: false,
	isTor: false,
	isVpn: false,
	isProxy: false,
	isResidentialProxy: false,
};

describe('Postgres ipinfo KV expiry', () => {
	it('expires request audit rows after 90 days', async () => {
		const writes: Array<Array<unknown>> = [];
		await createPostgresIpInfoRequestAuditLogger({client: recordingClient(writes)}).record(EVENT);
		expect(writes).toHaveLength(1);
		expect(writes[0]?.[0]).toBe('ipinfo_requests_by_hour');
		expectExpiresIn(writes[0], IPINFO_REQUEST_AUDIT_TTL_SECONDS);
	});

	it('falls back to the 14-day cache default', async () => {
		const writes: Array<Array<unknown>> = [];
		const cache = createPostgresIpInfoCache({client: recordingClient(writes)});
		await cache.set('fallback', {ok: true});
		await cache.set('zero', {ok: true}, 0);
		await cache.set('short', {ok: true}, 60);
		expect(writes.map((values) => values[0])).toEqual(['ipinfo_cache', 'ipinfo_cache', 'ipinfo_cache']);
		expectExpiresIn(writes[0], IPINFO_CACHE_TTL_SECONDS);
		expectExpiresIn(writes[1], IPINFO_CACHE_TTL_SECONDS);
		expectExpiresIn(writes[2], 60);
	});
});
