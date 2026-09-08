// SPDX-License-Identifier: AGPL-3.0-or-later

import type {GeoipAsnResult, GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import type {IpInfoLookupResult, IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import {describe, expect, it} from 'vitest';
import {createGeoIpAsnAdapter, createGeoIpCityAdapter} from '../adapters/GeoIpAdapters';

function throwingIpInfoService(): IpInfoService {
	return {
		lookup: async () => {
			throw new Error('ipinfo must not be consulted');
		},
	};
}

function countingIpInfoService(result: IpInfoLookupResult): {service: IpInfoService; calls: () => number} {
	let calls = 0;
	return {
		service: {
			lookup: async () => {
				calls += 1;
				return result;
			},
		},
		calls: () => calls,
	};
}

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
			region: 'California',
			regionCode: 'CA',
			city: 'San Jose',
			postalCode: null,
			timezone: 'America/Los_Angeles',
			latitude: 37.3,
			longitude: -121.9,
			accuracyRadiusKm: 20,
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

function geoipResult(overrides: Partial<GeoipResult> = {}): GeoipResult {
	return {
		countryCode: 'SE',
		normalizedIp: '198.51.100.7',
		city: 'Stockholm',
		region: 'Stockholm County',
		regionCode: 'AB',
		countryName: 'Sweden',
		latitude: 59.33,
		longitude: 18.06,
		accuracyRadiusKm: 5,
		timeZone: 'Europe/Stockholm',
		...overrides,
	};
}

function geoipAsnResult(overrides: Partial<GeoipAsnResult> = {}): GeoipAsnResult {
	return {
		normalizedIp: '198.51.100.7',
		asn: 64510,
		asnOrg: 'Example Broadband ISP',
		available: true,
		...overrides,
	};
}

describe('createGeoIpCityAdapter', () => {
	it('answers from the local city database without touching IPInfo', async () => {
		const lookupGeoIpCity = createGeoIpCityAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalCity: async () => geoipResult(),
		});
		await expect(lookupGeoIpCity({ip: '198.51.100.7'})).resolves.toEqual({
			ip: '198.51.100.7',
			available: true,
			found: true,
			countryIso: 'SE',
			country: 'Sweden',
			region: 'Stockholm County',
			city: 'Stockholm',
			latitude: 59.33,
			longitude: 18.06,
			accuracyRadiusKm: 5,
			timeZone: 'Europe/Stockholm',
		});
	});

	it('normalizes the IP before handing it to the local city database', async () => {
		const seen: Array<string> = [];
		const lookupGeoIpCity = createGeoIpCityAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalCity: async (ip) => {
				seen.push(ip);
				return geoipResult();
			},
		});
		await lookupGeoIpCity({ip: '[2001:0db8:0000::0001]'});
		expect(seen).toEqual(['2001:db8::1']);
	});

	it('coalesces missing optional local fields to null', async () => {
		const lookupGeoIpCity = createGeoIpCityAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalCity: async () => ({
				countryCode: 'SE',
				normalizedIp: '198.51.100.7',
				city: null,
				region: null,
				countryName: null,
			}),
		});
		await expect(lookupGeoIpCity({ip: '198.51.100.7'})).resolves.toEqual({
			ip: '198.51.100.7',
			available: true,
			found: true,
			countryIso: 'SE',
			country: null,
			region: null,
			city: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
			timeZone: null,
		});
	});

	it('falls back to IPInfo when the local city database has no country', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const lookupGeoIpCity = createGeoIpCityAdapter({
			ipInfoService: counting.service,
			lookupLocalCity: async () => geoipResult({countryCode: null}),
		});
		await expect(lookupGeoIpCity({ip: '198.51.100.1'})).resolves.toEqual({
			ip: '198.51.100.1',
			available: true,
			found: true,
			countryIso: 'US',
			country: 'United States',
			region: 'California',
			city: 'San Jose',
			latitude: 37.3,
			longitude: -121.9,
			accuracyRadiusKm: 20,
			timeZone: 'America/Los_Angeles',
		});
		expect(counting.calls()).toBe(1);
	});

	it('falls back to IPInfo when no local city lookup is wired', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const lookupGeoIpCity = createGeoIpCityAdapter({ipInfoService: counting.service});
		const result = await lookupGeoIpCity({ip: '198.51.100.1'});
		expect(result.countryIso).toBe('US');
		expect(counting.calls()).toBe(1);
	});

	it('returns an available not-found result for an unparseable IP without any lookup', async () => {
		const lookupGeoIpCity = createGeoIpCityAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalCity: async () => {
				throw new Error('local city must not be consulted');
			},
		});
		await expect(lookupGeoIpCity({ip: 'not-an-ip'})).resolves.toEqual({
			ip: 'not-an-ip',
			available: true,
			found: false,
			countryIso: null,
			country: null,
			region: null,
			city: null,
			latitude: null,
			longitude: null,
			accuracyRadiusKm: null,
			timeZone: null,
		});
	});
});

describe('createGeoIpAsnAdapter', () => {
	it('answers from the local ASN database without touching IPInfo', async () => {
		const lookupGeoIpAsn = createGeoIpAsnAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalAsn: async () => geoipAsnResult(),
		});
		await expect(lookupGeoIpAsn({ip: '198.51.100.7'})).resolves.toEqual({
			ip: '198.51.100.7',
			available: true,
			found: true,
			asn: 64510,
			asnOrg: 'Example Broadband ISP',
		});
	});

	it('normalizes the IP before handing it to the local ASN database', async () => {
		const seen: Array<string> = [];
		const lookupGeoIpAsn = createGeoIpAsnAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalAsn: async (ip) => {
				seen.push(ip);
				return geoipAsnResult();
			},
		});
		await lookupGeoIpAsn({ip: '[2001:0db8:0000::0001]'});
		expect(seen).toEqual(['2001:db8::1']);
	});

	it('falls back to IPInfo when the local ASN database has no ASN', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const lookupGeoIpAsn = createGeoIpAsnAdapter({
			ipInfoService: counting.service,
			lookupLocalAsn: async () => geoipAsnResult({asn: null, asnOrg: null}),
		});
		await expect(lookupGeoIpAsn({ip: '198.51.100.1'})).resolves.toEqual({
			ip: '198.51.100.1',
			available: true,
			found: true,
			asn: 64500,
			asnOrg: 'Test ISP',
		});
		expect(counting.calls()).toBe(1);
	});

	it('falls back to IPInfo when the local ASN database is unavailable', async () => {
		const counting = countingIpInfoService(ipInfoResult());
		const lookupGeoIpAsn = createGeoIpAsnAdapter({
			ipInfoService: counting.service,
			lookupLocalAsn: async () => geoipAsnResult({normalizedIp: null, asn: null, asnOrg: null, available: false}),
		});
		const result = await lookupGeoIpAsn({ip: '198.51.100.1'});
		expect(result.asn).toBe(64500);
		expect(counting.calls()).toBe(1);
	});

	it('returns an available not-found result for an unparseable IP without any lookup', async () => {
		const lookupGeoIpAsn = createGeoIpAsnAdapter({
			ipInfoService: throwingIpInfoService(),
			lookupLocalAsn: async () => {
				throw new Error('local ASN must not be consulted');
			},
		});
		await expect(lookupGeoIpAsn({ip: 'not-an-ip'})).resolves.toEqual({
			ip: 'not-an-ip',
			available: true,
			found: false,
			asn: null,
			asnOrg: null,
		});
	});
});
