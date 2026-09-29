// SPDX-License-Identifier: AGPL-3.0-or-later

import {server} from '@app/api/test/msw/server';
import type {
	CachedIpInfoFailure,
	IpInfoCache,
	IpInfoRequestAuditEvent,
	IpInfoRequestAuditLogger,
} from '@pkgs/geoip/src/IpInfoService';
import {createIpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {delay, HttpResponse, http} from 'msw';
import {describe, expect, it} from 'vitest';

interface RecordedSet {
	key: string;
	value: unknown;
	ttlSeconds: number | undefined;
}

interface RecordingCache {
	cache: IpInfoCache;
	sets: Array<RecordedSet>;
}

function createRecordingCache(): RecordingCache {
	const store = new Map<string, unknown>();
	const sets: Array<RecordedSet> = [];
	return {
		sets,
		cache: {
			async get<T>(key: string): Promise<T | null> {
				return (store.get(key) as T | undefined) ?? null;
			},
			async set<T>(key: string, value: T, ttlSeconds?: number): Promise<void> {
				store.set(key, value);
				sets.push({key, value, ttlSeconds});
			},
		},
	};
}

function createRecordingAuditLogger(): {logger: IpInfoRequestAuditLogger; events: Array<IpInfoRequestAuditEvent>} {
	const events: Array<IpInfoRequestAuditEvent> = [];
	return {
		events,
		logger: {
			async record(event: IpInfoRequestAuditEvent): Promise<void> {
				events.push(event);
			},
		},
	};
}

function useLookupHandler(handler: () => Response | Promise<Response>): {count: () => number} {
	let calls = 0;
	server.use(
		http.get('https://api.ipinfo.io/lookup/:ip', async () => {
			calls += 1;
			return await handler();
		}),
	);
	return {count: () => calls};
}

function successPayload(ip: string, anonymous: Record<string, boolean> = {}): Response {
	return HttpResponse.json({
		ip,
		geo: {country_code: 'US', country: 'United States'},
		as: {asn: 'AS64500', name: 'Test ISP'},
		anonymous,
	});
}

describe('IpInfoService caching', () => {
	it('negative-caches an HTTP error and serves the second lookup without a request', async () => {
		const requests = useLookupHandler(() => new HttpResponse(null, {status: 500}));
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		const first = await service.lookup('203.0.113.1');
		const second = await service.lookup('203.0.113.1');

		expect(first.available).toBe(false);
		expect(second.available).toBe(false);
		expect(requests.count()).toBe(1);
		expect(sets).toHaveLength(1);
		expect(sets[0]?.ttlSeconds).toBe(300);
	});

	it('negative-caches a request failure for a short window', async () => {
		useLookupHandler(async () => {
			await delay(5000);
			return successPayload('203.0.113.2');
		});
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		const result = await service.lookup('203.0.113.2');

		expect(result.available).toBe(false);
		expect(sets[0]?.ttlSeconds).toBe(60);
		expect((sets[0]?.value as CachedIpInfoFailure)?.failureOutcome).toBe('request_failed');
	});

	it('negative-caches a schema mismatch', async () => {
		useLookupHandler(() => HttpResponse.json({}));
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		const result = await service.lookup('203.0.113.3');

		expect(result.available).toBe(false);
		expect(sets[0]?.ttlSeconds).toBe(600);
		expect((sets[0]?.value as CachedIpInfoFailure)?.failureOutcome).toBe('schema_mismatch');
		expect((sets[0]?.value as CachedIpInfoFailure)?.failureHttpStatus).toBe(200);
	});

	it('negative-caches a quota rejection for longer', async () => {
		useLookupHandler(() => new HttpResponse(null, {status: 429}));
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		await service.lookup('203.0.113.4');

		expect(sets[0]?.ttlSeconds).toBe(900);
	});

	it('returns a cached failure as a clean unavailable result', async () => {
		useLookupHandler(() => new HttpResponse(null, {status: 500}));
		const {cache} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		await service.lookup('203.0.113.6');
		const cached = await service.lookup('203.0.113.6');

		expect(cached).not.toHaveProperty('cachedFailure');
		expect(cached).not.toHaveProperty('failureOutcome');
		expect(cached).not.toHaveProperty('failureHttpStatus');
		expect(cached).not.toHaveProperty('cachedAtMs');
		expect(cached.ip).toBe('203.0.113.6');
		expect(cached.note).toBe('IPInfo HTTP 500');
	});

	it('writes a cached failure that older readers can still consume', async () => {
		useLookupHandler(() => new HttpResponse(null, {status: 500}));
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		await service.lookup('203.0.113.7');

		const entry = sets[0]?.value as CachedIpInfoFailure;
		expect(entry.cachedFailure).toBe(true);
		expect(entry.failureOutcome).toBe('http_error');
		expect(entry.failureHttpStatus).toBe(500);
		expect(typeof entry.cachedAtMs).toBe('number');
		const legacyView = {...entry, ip: '203.0.113.7'};
		expect(legacyView.available).toBe(false);
		expect(legacyView.geo.countryCode).toBeNull();
		expect(legacyView.asn.number).toBeNull();
		expect(legacyView.mobile.name).toBeNull();
		expect(legacyView.anonymous.isAnonymous).toBe(false);
		expect(legacyView.flags.isMobile).toBe(false);
	});

	it('keeps the existing success TTL selection', async () => {
		useLookupHandler(() => successPayload('203.0.113.8'));
		const plain = createRecordingCache();
		await createIpInfoService({apiKey: 'token', cache: plain.cache}).lookup('203.0.113.8');

		useLookupHandler(() => successPayload('203.0.113.9', {is_vpn: true}));
		const anonymous = createRecordingCache();
		await createIpInfoService({apiKey: 'token', cache: anonymous.cache}).lookup('203.0.113.9');

		expect(plain.sets[0]?.ttlSeconds).toBe(14 * 24 * 60 * 60);
		expect(anonymous.sets[0]?.ttlSeconds).toBe(7 * 24 * 60 * 60);
	});

	it('coalesces concurrent lookups across a failure', async () => {
		const requests = useLookupHandler(() => new HttpResponse(null, {status: 500}));
		const {cache, sets} = createRecordingCache();
		const service = createIpInfoService({apiKey: 'token', cache});

		const [first, second] = await Promise.all([service.lookup('203.0.113.10'), service.lookup('203.0.113.10')]);

		expect(requests.count()).toBe(1);
		expect(sets).toHaveLength(1);
		expect(first.available).toBe(false);
		expect(second.available).toBe(false);
	});

	it('coalesces concurrent lookups from different sources into one audited request', async () => {
		const requests = useLookupHandler(() => successPayload('203.0.113.13'));
		const {cache} = createRecordingCache();
		const {logger, events} = createRecordingAuditLogger();
		const service = createIpInfoService({apiKey: 'token', cache, auditLogger: logger});

		const results = await Promise.all([
			service.lookup('203.0.113.13', {source: 'admin.ip_ban', reason: 'ban'}),
			service.lookup('203.0.113.13', {source: 'test.b'}),
			service.lookup('203.0.113.13', {source: 'test.c'}),
		]);

		expect(requests.count()).toBe(1);
		expect(results.every((result) => result.available)).toBe(true);
		expect(events).toHaveLength(1);
		expect(events[0]?.source).toBe('admin.ip_ban');
		expect(events[0]?.outcome).toBe('http_success');
		expect(events[0]?.note).toBe('IPInfo: IP is not anonymous');
	});
});
