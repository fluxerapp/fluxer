// SPDX-License-Identifier: AGPL-3.0-or-later

import {parseIpAddress} from '@fluxer/ip_utils/src/IpAddress';
import type {GeoipAsnResult, GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import type {IpInfoService} from '@pkgs/geoip/src/IpInfoService';
import type {GeoIpAsnResult, GeoIpCityResult} from '../RiskTypes';

interface GeoIpCityContext {
	ipInfoService: IpInfoService;
	lookupLocalCity?: (ip: string) => Promise<GeoipResult>;
}

interface GeoIpAsnContext {
	ipInfoService: IpInfoService;
	lookupLocalAsn?: (ip: string) => Promise<GeoipAsnResult>;
}

export function createGeoIpCityAdapter(ctx: GeoIpCityContext) {
	return async function lookupGeoIpCity(args: {ip: string}): Promise<GeoIpCityResult> {
		const ip = args.ip;
		const parsed = parseIpAddress(ip);
		if (!parsed) {
			return notFound(ip, true);
		}
		const local = ctx.lookupLocalCity ? await ctx.lookupLocalCity(parsed.normalized) : null;
		if (local && local.countryCode !== null) {
			return {
				ip,
				available: true,
				found: true,
				countryIso: local.countryCode,
				country: local.countryName,
				region: local.region,
				city: local.city,
				latitude: local.latitude ?? null,
				longitude: local.longitude ?? null,
				accuracyRadiusKm: local.accuracyRadiusKm ?? null,
				timeZone: local.timeZone ?? null,
			};
		}
		const info = await ctx.ipInfoService.lookup(parsed.normalized, {
			source: 'risk.geoip_city',
			reason: 'registration_risk',
		});
		if (!info.available) {
			return {
				ip,
				available: false,
				found: false,
				countryIso: null,
				country: null,
				region: null,
				city: null,
				latitude: null,
				longitude: null,
				accuracyRadiusKm: null,
				timeZone: null,
			};
		}
		const geo = info.geo;
		const found = geo.countryCode !== null || geo.region !== null || geo.city !== null || geo.latitude !== null;
		return {
			ip,
			available: true,
			found,
			countryIso: geo.countryCode,
			country: geo.countryName,
			region: geo.region,
			city: geo.city,
			latitude: geo.latitude,
			longitude: geo.longitude,
			accuracyRadiusKm: geo.accuracyRadiusKm,
			timeZone: geo.timezone,
		};
	};
}

export function createGeoIpAsnAdapter(ctx: GeoIpAsnContext) {
	return async function lookupGeoIpAsn(args: {ip: string}): Promise<GeoIpAsnResult> {
		const ip = args.ip;
		const parsed = parseIpAddress(ip);
		if (!parsed) {
			return {ip, available: true, found: false, asn: null, asnOrg: null};
		}
		const local = ctx.lookupLocalAsn ? await ctx.lookupLocalAsn(parsed.normalized) : null;
		if (local && local.asn !== null) {
			return {ip, available: true, found: true, asn: local.asn, asnOrg: local.asnOrg};
		}
		const info = await ctx.ipInfoService.lookup(parsed.normalized, {
			source: 'risk.geoip_asn',
			reason: 'registration_risk',
		});
		if (!info.available) {
			return {ip, available: false, found: false, asn: null, asnOrg: null};
		}
		const {number, name} = info.asn;
		const found = number !== null || name !== null;
		return {ip, available: true, found, asn: number, asnOrg: name};
	};
}

function notFound(ip: string, available: boolean): GeoIpCityResult {
	return {
		ip,
		available,
		found: false,
		countryIso: null,
		country: null,
		region: null,
		city: null,
		latitude: null,
		longitude: null,
		accuracyRadiusKm: null,
		timeZone: null,
	};
}
