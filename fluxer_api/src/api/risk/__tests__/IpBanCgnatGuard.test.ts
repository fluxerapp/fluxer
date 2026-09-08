// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IpInfoLookupResult} from '@pkgs/geoip/src/IpInfoService';
import {describe, expect, it} from 'vitest';
import {
	isHighCgnatBlastRadiusRisk,
	isHighSharedAccessBlastRadiusRisk,
	isSingleIpBanCandidate,
} from '../IpBanCgnatGuard';

function ipInfoResult(overrides: Partial<IpInfoLookupResult> = {}): IpInfoLookupResult {
	return {
		ip: '198.51.100.1',
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

describe('IpBanCgnatGuard', () => {
	it('only treats single IP ban entries as CGNAT guard candidates', () => {
		expect(isSingleIpBanCandidate('198.51.100.10')).toBe(true);
		expect(isSingleIpBanCandidate('198.51.100.0/24')).toBe(false);
	});
	it('flags mobile carrier IPs as high blast-radius risk', () => {
		expect(
			isHighCgnatBlastRadiusRisk(
				ipInfoResult({
					mobile: {name: 'Example Mobile', mcc: '001', mnc: '01'},
					flags: {isAnycast: false, isHosting: false, isMobile: true, isSatellite: false},
				}),
			),
		).toBe(true);
	});
	it('does not exempt hosting or anonymous infrastructure', () => {
		expect(
			isHighCgnatBlastRadiusRisk(
				ipInfoResult({
					flags: {isAnycast: false, isHosting: true, isMobile: true, isSatellite: false},
				}),
			),
		).toBe(false);
		expect(
			isHighCgnatBlastRadiusRisk(
				ipInfoResult({
					anonymous: {
						isAnonymous: true,
						providerName: 'Example VPN',
						isVpn: true,
						isProxy: false,
						isResidentialProxy: false,
						isTor: false,
						isRelay: false,
						percentDaysSeen: null,
					},
					flags: {isAnycast: false, isHosting: false, isMobile: true, isSatellite: false},
				}),
			),
		).toBe(false);
	});
	it('flags satellite, anycast and education networks as high blast-radius risk', () => {
		expect(
			isHighSharedAccessBlastRadiusRisk(
				ipInfoResult({
					flags: {isAnycast: false, isHosting: false, isMobile: false, isSatellite: true},
				}),
			),
		).toBe(true);
		expect(
			isHighSharedAccessBlastRadiusRisk(
				ipInfoResult({
					flags: {isAnycast: true, isHosting: false, isMobile: false, isSatellite: false},
				}),
			),
		).toBe(true);
		expect(
			isHighSharedAccessBlastRadiusRisk(
				ipInfoResult({asn: {asn: 'AS64500', number: 64500, name: 'Test University', domain: null, type: 'education'}}),
			),
		).toBe(true);
	});
	it('does not flag ordinary residential networks as shared-access risk', () => {
		expect(isHighSharedAccessBlastRadiusRisk(ipInfoResult())).toBe(false);
	});
	it('does not treat shared-access networks as CGNAT risk', () => {
		expect(
			isHighCgnatBlastRadiusRisk(
				ipInfoResult({
					flags: {isAnycast: false, isHosting: false, isMobile: false, isSatellite: true},
				}),
			),
		).toBe(false);
	});
	it('does not exempt hosting or anonymous shared-access infrastructure', () => {
		expect(
			isHighSharedAccessBlastRadiusRisk(
				ipInfoResult({
					flags: {isAnycast: true, isHosting: true, isMobile: false, isSatellite: false},
				}),
			),
		).toBe(false);
		expect(
			isHighSharedAccessBlastRadiusRisk(
				ipInfoResult({
					anonymous: {
						isAnonymous: true,
						providerName: 'Example VPN',
						isVpn: true,
						isProxy: false,
						isResidentialProxy: false,
						isTor: false,
						isRelay: false,
						percentDaysSeen: null,
					},
					flags: {isAnycast: false, isHosting: false, isMobile: false, isSatellite: true},
				}),
			),
		).toBe(false);
	});
});
