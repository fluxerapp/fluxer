// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRegionDisplayName} from '@fluxer/geo_utils/src/RegionFormatting';
import {getSameIpDecisionKey, isValidIp, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';
import maxmind, {type CityResponse, type Reader} from 'maxmind';

export const UNKNOWN_LOCATION = 'Unknown Location';

export interface GeoipResult {
	countryCode: string | null;
	normalizedIp: string | null;
	city: string | null;
	region: string | null;
	regionCode?: string | null;
	countryName: string | null;
	latitude?: number | null;
	longitude?: number | null;
}

type CacheEntry = {
	result: GeoipResult;
	expiresAt: number;
};

const CACHE_TTL_MS = 10 * 60 * 1000;
const CACHE_MAX_ENTRIES = 10_000;
const geoipCache = new Map<string, CacheEntry>();

let maxmindReader: Reader<CityResponse> | null = null;
let maxmindReaderPromise: Promise<Reader<CityResponse>> | null = null;

function buildFallbackResult(normalizedIp: string): GeoipResult {
	return {
		countryCode: null,
		normalizedIp: normalizedIp || null,
		city: null,
		region: null,
		regionCode: null,
		countryName: null,
		latitude: null,
		longitude: null,
	};
}

async function ensureReader(dbPath: string): Promise<Reader<CityResponse>> {
	if (maxmindReader) return maxmindReader;
	if (!maxmindReaderPromise) {
		maxmindReaderPromise = maxmind
			.open<CityResponse>(dbPath)
			.then((reader) => {
				maxmindReader = reader;
				return reader;
			})
			.catch((error) => {
				maxmindReaderPromise = null;
				throw error;
			});
	}
	return maxmindReaderPromise;
}

function stateLabel(record?: CityResponse): string | null {
	const subdivision = record?.subdivisions?.[0];
	if (!subdivision) return null;
	return subdivision.names?.en || subdivision.iso_code || null;
}

function stateCode(record?: CityResponse): string | null {
	const subdivision = record?.subdivisions?.[0];
	if (!subdivision?.iso_code) return null;
	return subdivision.iso_code.toUpperCase();
}

function countryDisplayName(code: string, locale = 'en'): string | null {
	if (!isAsciiUpperAlpha2(code)) return null;
	return getRegionDisplayName(code, {locale}) ?? null;
}

function isAsciiUpperAlpha2(value: string): boolean {
	return (
		value.length === 2 &&
		value.charCodeAt(0) >= 65 &&
		value.charCodeAt(0) <= 90 &&
		value.charCodeAt(1) >= 65 &&
		value.charCodeAt(1) <= 90
	);
}

function getCachedGeoipResult(cacheKey: string, normalizedIp: string): GeoipResult | null {
	const cached = geoipCache.get(cacheKey);
	if (!cached) {
		return null;
	}
	if (Date.now() >= cached.expiresAt) {
		geoipCache.delete(cacheKey);
		return null;
	}
	geoipCache.delete(cacheKey);
	geoipCache.set(cacheKey, cached);
	return {...cached.result, normalizedIp};
}

function setCachedGeoipResult(cacheKey: string, result: GeoipResult): void {
	geoipCache.delete(cacheKey);
	if (geoipCache.size >= CACHE_MAX_ENTRIES) {
		const oldestKey = geoipCache.keys().next().value;
		if (oldestKey === undefined) {
			throw new Error('GeoIP cache reached capacity without an entry to evict');
		}
		geoipCache.delete(oldestKey);
	}
	geoipCache.set(cacheKey, {result, expiresAt: Date.now() + CACHE_TTL_MS});
}

async function lookupMaxmind(clean: string, dbPath: string): Promise<GeoipResult> {
	try {
		const reader = await ensureReader(dbPath);
		const record = reader.get(clean);
		if (!record) return buildFallbackResult(clean);
		const isoCode = record.country?.iso_code;
		const countryCode = isoCode ? isoCode.toUpperCase() : null;
		return {
			countryCode,
			normalizedIp: clean,
			city: record.city?.names?.en ?? null,
			region: stateLabel(record),
			regionCode: stateCode(record),
			countryName: record.country?.names?.en ?? (countryCode ? countryDisplayName(countryCode) : null) ?? null,
			latitude: record.location?.latitude ?? null,
			longitude: record.location?.longitude ?? null,
		};
	} catch {
		return buildFallbackResult(clean);
	}
}

async function resolveGeoip(clean: string, dbPath: string): Promise<GeoipResult> {
	const cacheKey = getSameIpDecisionKey(clean) ?? clean;
	const cached = getCachedGeoipResult(cacheKey, clean);
	if (cached) {
		return cached;
	}
	const result = await lookupMaxmind(clean, dbPath);
	setCachedGeoipResult(cacheKey, result);
	return result;
}

export async function lookupGeoipByIp(ip: string, dbPath: string | undefined): Promise<GeoipResult> {
	if (!dbPath) {
		return buildFallbackResult(ip);
	}
	const clean = normalizeIpString(ip);
	if (!isValidIp(clean)) {
		return buildFallbackResult(clean);
	}
	return resolveGeoip(clean, dbPath);
}

export function formatGeoipLocation(result: GeoipResult): string | null {
	const parts: Array<string> = [];
	if (result.city) parts.push(result.city);
	if (result.region) parts.push(result.region);
	const countryLabel = result.countryName ?? result.countryCode;
	if (countryLabel) parts.push(countryLabel);
	return parts.length > 0 ? parts.join(', ') : null;
}
