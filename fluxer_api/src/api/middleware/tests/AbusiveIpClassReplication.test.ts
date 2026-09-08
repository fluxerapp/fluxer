// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoLookupResult} from '@pkgs/geoip/src/IpInfoService';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {ApiTestHarness} from '../../test/ApiTestHarness';
import {createApiTestHarness} from '../../test/ApiTestHarness';
import type {MockKVProvider} from '../../test/mocks/MockKVProvider';
import {
	drainAbuseAutoBanTasksForTests,
	drainAbuseIpClassLookupsForTests,
	hashAuthToken,
	recordAbuseSignal,
	resetAbuseTrackingForTests,
	setAbuseIpClassTtlsForTests,
	startAbuseReplicationSubscriber,
	stopAbuseReplicationSubscriber,
} from '../AbusiveIpAutoBanner';
import {ipBanCache} from '../IpBanMiddleware';
import {setInjectedIpInfoService} from '../ServiceMiddleware';

function ipInfoResult(ip: string, overrides: Partial<IpInfoLookupResult> = {}): IpInfoLookupResult {
	return {
		ip,
		available: true,
		riskNote: 'test',
		geo: {
			countryCode: 'US',
			countryName: 'United States',
			continent: 'North America',
			continentCode: 'NA',
			region: null,
			regionCode: null,
			city: null,
			postalCode: null,
			timezone: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
		},
		asn: {
			asn: 'AS64500',
			number: 64500,
			name: 'Test ISP',
			domain: null,
			type: null,
		},
		mobile: {
			name: null,
			mcc: null,
			mnc: null,
		},
		anonymous: {
			isAnonymous: false,
			providerName: null,
			isVpn: false,
			isProxy: false,
			isResidentialProxy: false,
			isTor: false,
			isRelay: false,
			percentDaysSeen: null,
		},
		flags: {
			isAnycast: false,
			isHosting: false,
			isMobile: false,
			isSatellite: false,
		},
		...overrides,
	};
}

function publishPeerIpClasses(harness: ApiTestHarness, entries: Array<[string, string, unknown]>): void {
	const kvProvider = harness.kvProvider as MockKVProvider;
	kvProvider
		.getSubscription()
		.simulateMessage('abuse_tracker:ipclass', JSON.stringify({sender: 'other-pod', entries, ts: Date.now()}));
}

function claimCallCount(harness: ApiTestHarness, banKey: string): number {
	const kvProvider = harness.kvProvider as MockKVProvider;
	return kvProvider.setnxSpy.mock.calls.filter(([key]) => key === `abuse:ipclass:claim:${banKey}`).length;
}

function recordTokenSignals(ip: string, prefix: string, from: number, to: number): void {
	for (let i = from; i < to; i += 1) {
		recordAbuseSignal(ip, 'auth_failure:session', {tokenHash: hashAuthToken(`${prefix}-${i}`)});
	}
}

async function drainAbuseWork(): Promise<void> {
	await drainAbuseIpClassLookupsForTests();
	await drainAbuseAutoBanTasksForTests();
}

describe('AbusiveIpAutoBanner IP class replication', () => {
	let harness: ApiTestHarness;
	let lookupCount = 0;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		resetAbuseTrackingForTests();
		ipBanCache.resetCaches();
		lookupCount = 0;
		setInjectedIpInfoService({
			async lookup(ip: string) {
				lookupCount += 1;
				return ipInfoResult(ip);
			},
		});
		await stopAbuseReplicationSubscriber();
		await startAbuseReplicationSubscriber(harness.kvProvider);
	});
	afterAll(async () => {
		await stopAbuseReplicationSubscriber();
		setInjectedIpInfoService(undefined);
		await harness.shutdown();
	});
	it('adopts a datacenter class from a peer without paying for its own lookup', async () => {
		const ip = '8.8.8.8';
		recordTokenSignals(ip, 'peer-datacenter', 0, 3);
		publishPeerIpClasses(harness, [[ip, ip, 'datacenter']]);
		recordTokenSignals(ip, 'peer-datacenter', 3, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(0);
		expect(claimCallCount(harness, ip)).toBe(0);
		expect(ipBanCache.isBanned(ip)).toBe(true);
	});
	it('never adopts a mobile class from a peer and leaves the IP bannable', async () => {
		const ip = '8.8.4.4';
		recordTokenSignals(ip, 'peer-mobile', 0, 3);
		publishPeerIpClasses(harness, [[ip, ip, 'mobile']]);
		recordTokenSignals(ip, 'peer-mobile', 3, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(0);
		expect(ipBanCache.isBanned(ip)).toBe(false);
		publishPeerIpClasses(harness, [[ip, ip, 'datacenter']]);
		await drainAbuseWork();
		expect(ipBanCache.isBanned(ip)).toBe(true);
	});
	it('ignores an unrecognised class from a peer and classifies the IP itself', async () => {
		const ip = '4.4.4.4';
		recordTokenSignals(ip, 'peer-invalid', 0, 3);
		publishPeerIpClasses(harness, [
			[ip, ip, 'datacentre'],
			[ip, ip, 42],
		]);
		recordTokenSignals(ip, 'peer-invalid', 3, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		expect(ipBanCache.isBanned(ip)).toBe(true);
	});
	it('ignores an unknown class from a peer and classifies the IP itself', async () => {
		const ip = '9.9.9.9';
		recordTokenSignals(ip, 'peer-unknown', 0, 3);
		publishPeerIpClasses(harness, [[ip, ip, 'unknown']]);
		recordTokenSignals(ip, 'peer-unknown', 3, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		expect(ipBanCache.isBanned(ip)).toBe(true);
	});
	it('ignores a peer class for an IP it is not tracking', async () => {
		const ip = '208.67.222.222';
		publishPeerIpClasses(harness, [[ip, ip, 'datacenter']]);
		recordTokenSignals(ip, 'peer-untracked', 0, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		expect(claimCallCount(harness, ip)).toBe(1);
	});
	it('accepts a peer mobile class after its own lookup failed and keeps the IP unbanned', async () => {
		const ip = '199.85.126.10';
		setInjectedIpInfoService({
			async lookup(lookupIp: string) {
				lookupCount += 1;
				return ipInfoResult(lookupIp, {available: false});
			},
		});
		recordAbuseSignal(ip, 'http_429', {weight: 25});
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		publishPeerIpClasses(harness, [[ip, ip, 'mobile']]);
		recordTokenSignals(ip, 'negative-then-mobile', 0, 10);
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		expect(ipBanCache.isBanned(ip)).toBe(false);
	});
	it('resumes classifying once a peer class hint expires', async () => {
		const ip = '77.88.8.8';
		setAbuseIpClassTtlsForTests({hintMs: 50});
		recordAbuseSignal(ip, 'http_429', {weight: 3});
		publishPeerIpClasses(harness, [[ip, ip, 'datacenter']]);
		recordAbuseSignal(ip, 'http_429', {weight: 25});
		await drainAbuseWork();
		expect(lookupCount).toBe(0);
		await new Promise((resolve) => setTimeout(resolve, 80));
		recordAbuseSignal(ip, 'http_429', {weight: 25});
		await drainAbuseWork();
		expect(lookupCount).toBe(1);
		expect(claimCallCount(harness, ip)).toBe(1);
		expect(ipBanCache.isBanned(ip)).toBe(false);
	});
});
