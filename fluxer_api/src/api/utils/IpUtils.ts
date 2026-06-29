// SPDX-License-Identifier: AGPL-3.0-or-later

import dns from 'node:dns';
import {extractClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import type {GeoipResult} from '@pkgs/geoip/src/GeoipLookup';
import {formatGeoipLocation, lookupGeoipByIp} from '@pkgs/geoip/src/GeoipLookup';
import {seconds} from 'itty-time';
import {Config} from '../Config';

const REVERSE_DNS_CACHE_TTL_SECONDS = seconds('1 day');
const REVERSE_DNS_CACHE_PREFIX = 'reverse-dns:';

interface GetIpAddressReverseOptions {
	timeoutMs?: number;
	cacheTtlSeconds?: number;
}

export function resolveRequestClientIp(req: Request): string | null {
	return extractClientIp(req, {
		trustClientIpHeader: Config.proxy.trust_client_ip_header,
		clientIpHeaderName: Config.proxy.client_ip_header,
	});
}

export async function lookupGeoip(req: Request): Promise<GeoipResult>;
export async function lookupGeoip(ip: string): Promise<GeoipResult>;
export async function lookupGeoip(input: string | Request): Promise<GeoipResult> {
	const ip = typeof input === 'string' ? input : resolveRequestClientIp(input);
	if (!ip) {
		return {countryCode: null, normalizedIp: null, city: null, region: null, countryName: null};
	}
	return lookupGeoipByIp(ip, Config.geoip.maxmindDbPath);
}

export async function getIpAddressReverse(
	ip: string,
	cacheService?: ICacheService,
	options: GetIpAddressReverseOptions = {},
): Promise<string | null> {
	const cacheKey = `${REVERSE_DNS_CACHE_PREFIX}${ip}`;
	if (cacheService) {
		const cached = await cacheService.get<string | null>(cacheKey);
		if (cached !== null) return cached === '' ? null : cached;
	}
	let result: string | null = null;
	try {
		const reversePromise: Promise<Array<string>> = dns.promises.reverse(ip);
		let hostnames: Array<string>;
		if (options.timeoutMs !== undefined) {
			const timeoutMs = options.timeoutMs;
			const timeoutPromise = new Promise<Array<string>>((_, reject) => {
				setTimeout(() => reject(new Error(`reverse DNS timeout after ${timeoutMs}ms`)), timeoutMs);
			});
			hostnames = await Promise.race([reversePromise, timeoutPromise]);
		} else {
			hostnames = await reversePromise;
		}
		result = hostnames[0] ?? null;
	} catch {
		result = null;
	}
	if (cacheService) {
		await cacheService.set(cacheKey, result ?? '', options.cacheTtlSeconds ?? REVERSE_DNS_CACHE_TTL_SECONDS);
	}
	return result;
}

export async function getLocationLabelFromIp(ip: string): Promise<string | null> {
	const result = await lookupGeoipByIp(ip, Config.geoip.maxmindDbPath);
	return formatGeoipLocation(result);
}
