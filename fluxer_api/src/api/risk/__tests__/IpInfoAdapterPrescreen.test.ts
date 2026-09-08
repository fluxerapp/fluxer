// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoLookupContext, IpInfoLookupResult, IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {afterEach, beforeEach, describe, expect, it} from 'vitest';
import {createCurrentBehaviorTestAccountPolicyEvaluator} from '../../test/AccountPolicyTestEvaluator';
import {setInjectedAccountPolicyEvaluator} from '../AccountPolicyService';
import {createIpInfoChecker, unavailableIpInfoAnonymousResult} from '../adapters/IpInfoAdapter';

function ipInfoResult(overrides: Partial<IpInfoLookupResult> = {}): IpInfoLookupResult {
	return {
		ip: '198.51.100.1',
		available: true,
		riskNote: 'live lookup',
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
			type: 'isp',
		},
		mobile: {
			name: null,
			mcc: null,
			mnc: null,
		},
		anonymous: {
			isAnonymous: true,
			providerName: 'Example VPN',
			isVpn: true,
			isProxy: false,
			isResidentialProxy: false,
			isTor: false,
			isRelay: false,
			percentDaysSeen: 42,
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

function countingIpInfoService(result: IpInfoLookupResult): {
	service: IpInfoService;
	calls: () => number;
	contexts: () => Array<IpInfoLookupContext | undefined>;
} {
	const contexts: Array<IpInfoLookupContext | undefined> = [];
	return {
		service: {
			lookup: async (_ip, context) => {
				contexts.push(context);
				return result;
			},
		},
		calls: () => contexts.length,
		contexts: () => contexts,
	};
}

beforeEach(() => {
	setInjectedAccountPolicyEvaluator(createCurrentBehaviorTestAccountPolicyEvaluator());
});

afterEach(() => {
	setInjectedAccountPolicyEvaluator(undefined);
});

describe('createIpInfoChecker pre-screen', () => {
	it('returns an unavailable result with zero lookups when the pre-screen skips', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const checkIpInfo = createIpInfoChecker({
			ipInfoService: counting.service,
			prescreen: async () => 'skip',
		});
		await expect(checkIpInfo('198.51.100.1')).resolves.toEqual(
			unavailableIpInfoAnonymousResult('198.51.100.1', 'IPInfo skipped (local pre-screen)'),
		);
		expect(counting.calls()).toBe(0);
	});

	it('calls IPInfo exactly once and preserves the mapping when the pre-screen consults', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const checkIpInfo = createIpInfoChecker({
			ipInfoService: counting.service,
			prescreen: async () => 'consult',
		});
		await expect(checkIpInfo('198.51.100.1')).resolves.toEqual({
			ip: '198.51.100.1',
			available: true,
			isAnonymous: true,
			providerName: 'Example VPN',
			isVpn: true,
			isProxy: false,
			isResidentialProxy: false,
			isTor: false,
			isRelay: false,
			isHosting: false,
			isMobile: false,
			asnType: 'isp',
			asnOrg: 'Test ISP',
			connectionType: 'residential',
			percentDaysSeen: 42,
			riskNote: 'live lookup',
		});
		expect(counting.calls()).toBe(1);
		expect(counting.contexts()).toEqual([{source: 'risk.ipinfo_checker', reason: 'registration_risk'}]);
	});

	it('is identical to a consulting pre-screen when no pre-screen is wired', async () => {
		const withoutPrescreen = countingIpInfoService(ipInfoResult());
		const withPrescreen = countingIpInfoService(ipInfoResult());
		const baseline = await createIpInfoChecker({ipInfoService: withoutPrescreen.service})('198.51.100.1');
		const consulted = await createIpInfoChecker({
			ipInfoService: withPrescreen.service,
			prescreen: async () => 'consult',
		})('198.51.100.1');
		expect(baseline).toEqual(consulted);
		expect(withoutPrescreen.calls()).toBe(1);
		expect(withPrescreen.calls()).toBe(1);
	});

	it('never synthesizes a clean attestation for a skipped lookup', () => {
		const skipped = unavailableIpInfoAnonymousResult('198.51.100.1', 'IPInfo skipped (local pre-screen)');
		expect(skipped.available).toBe(false);
		expect(skipped.isAnonymous).toBe(false);
		expect(skipped.isVpn).toBe(false);
		expect(skipped.isProxy).toBe(false);
		expect(skipped.isResidentialProxy).toBe(false);
		expect(skipped.isTor).toBe(false);
		expect(skipped.isRelay).toBe(false);
		expect(skipped.isHosting).toBe(false);
		expect(skipped.isMobile).toBe(false);
		expect(skipped.asnType).toBeNull();
		expect(skipped.asnOrg).toBeNull();
		expect(skipped.connectionType).toBe('unknown');
		expect(skipped.percentDaysSeen).toBeNull();
	});
});
