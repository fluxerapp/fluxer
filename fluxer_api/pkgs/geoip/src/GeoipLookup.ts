// SPDX-License-Identifier: AGPL-3.0-or-later

import {getRegionDisplayName} from '@fluxer/geo_utils/src/RegionFormatting';
import {getSameIpDecisionKey, isValidIp, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';
import maxmind, {type AsnResponse, type CityResponse, type Reader} from 'maxmind';

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
	accuracyRadiusKm?: number | null;
	timeZone?: string | null;
}

export interface GeoipAsnResult {
	normalizedIp: string | null;
	asn: number | null;
	asnOrg: string | null;
	available: boolean;
}

type CacheEntry = {
	result: GeoipResult;
	expiresAt: number;
};

type AsnCacheEntry = {
	result: GeoipAsnResult;
	expiresAt: number;
};

const CACHE_TTL_MS = 10 * 60 * 1000;
const CACHE_MAX_ENTRIES = 10_000;
const geoipCache = new Map<string, CacheEntry>();
const asnCache = new Map<string, AsnCacheEntry>();

let maxmindReader: Reader<CityResponse> | null = null;
let maxmindReaderPromise: Promise<Reader<CityResponse>> | null = null;
let maxmindAsnReader: Reader<AsnResponse> | null = null;
let maxmindAsnReaderPromise: Promise<Reader<AsnResponse>> | null = null;
let maxmindAsnUnavailable = false;

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
		accuracyRadiusKm: null,
		timeZone: null,
	};
}

function buildAsnFallbackResult(normalizedIp: string | null): GeoipAsnResult {
	return {
		normalizedIp: normalizedIp || null,
		asn: null,
		asnOrg: null,
		available: false,
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

async function ensureAsnReader(dbPath: string): Promise<Reader<AsnResponse>> {
	if (maxmindAsnReader) return maxmindAsnReader;
	if (!maxmindAsnReaderPromise) {
		maxmindAsnReaderPromise = maxmind
			.open<AsnResponse>(dbPath, {watchForUpdates: true, watchForUpdatesNonPersistent: true})
			.then((reader) => {
				maxmindAsnReader = reader;
				return reader;
			})
			.catch((error) => {
				maxmindAsnReaderPromise = null;
				maxmindAsnUnavailable = true;
				throw error;
			});
	}
	return maxmindAsnReaderPromise;
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

function getCachedAsnResult(cacheKey: string, normalizedIp: string): GeoipAsnResult | null {
	const cached = asnCache.get(cacheKey);
	if (!cached) {
		return null;
	}
	if (Date.now() >= cached.expiresAt) {
		asnCache.delete(cacheKey);
		return null;
	}
	asnCache.delete(cacheKey);
	asnCache.set(cacheKey, cached);
	return {...cached.result, normalizedIp};
}

function setCachedAsnResult(cacheKey: string, result: GeoipAsnResult): void {
	asnCache.delete(cacheKey);
	if (asnCache.size >= CACHE_MAX_ENTRIES) {
		const oldestKey = asnCache.keys().next().value;
		if (oldestKey !== undefined) {
			asnCache.delete(oldestKey);
		}
	}
	asnCache.set(cacheKey, {result, expiresAt: Date.now() + CACHE_TTL_MS});
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
			accuracyRadiusKm: record.location?.accuracy_radius ?? null,
			timeZone: record.location?.time_zone ?? null,
		};
	} catch {
		return buildFallbackResult(clean);
	}
}

async function lookupMaxmindAsn(clean: string, dbPath: string): Promise<GeoipAsnResult> {
	try {
		const reader = await ensureAsnReader(dbPath);
		const record = reader.get(clean);
		if (!record) {
			return {normalizedIp: clean, asn: null, asnOrg: null, available: true};
		}
		return {
			normalizedIp: clean,
			asn: record.autonomous_system_number ?? null,
			asnOrg: record.autonomous_system_organization ?? null,
			available: true,
		};
	} catch {
		return buildAsnFallbackResult(clean);
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

async function resolveAsn(clean: string, dbPath: string): Promise<GeoipAsnResult> {
	const cacheKey = getSameIpDecisionKey(clean) ?? clean;
	const cached = getCachedAsnResult(cacheKey, clean);
	if (cached) {
		return cached;
	}
	const result = await lookupMaxmindAsn(clean, dbPath);
	setCachedAsnResult(cacheKey, result);
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

export async function lookupAsnByIp(ip: string, asnDbPath: string | undefined): Promise<GeoipAsnResult> {
	if (!asnDbPath || maxmindAsnUnavailable) {
		return buildAsnFallbackResult(null);
	}
	const clean = normalizeIpString(ip);
	if (!isValidIp(clean)) {
		return buildAsnFallbackResult(clean);
	}
	return resolveAsn(clean, asnDbPath);
}

export function resetGeoipReadersForTesting(): void {
	maxmindReader = null;
	maxmindReaderPromise = null;
	maxmindAsnReader = null;
	maxmindAsnReaderPromise = null;
	maxmindAsnUnavailable = false;
	geoipCache.clear();
	asnCache.clear();
}

export function formatGeoipLocation(result: GeoipResult): string | null {
	const parts: Array<string> = [];
	if (result.city) parts.push(result.city);
	if (result.region) parts.push(result.region);
	const countryLabel = result.countryName ?? result.countryCode;
	if (countryLabel) parts.push(countryLabel);
	return parts.length > 0 ? parts.join(', ') : null;
}
