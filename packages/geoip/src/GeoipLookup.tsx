/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {getRegionDisplayName} from '@fluxer/geo_utils/src/RegionFormatting';
import {isValidIp, normalizeIpString} from '@fluxer/ip_utils/src/IpAddress';
import maxmind, {type CityResponse, type Reader} from 'maxmind';

export const UNKNOWN_LOCATION = 'Unknown Location';

export interface GeoipResult {
	countryCode: string | null;
	normalizedIp: string | null;
	city: string | null;
	region: string | null;
	countryName: string | null;
}

type CacheEntry = {
	result: GeoipResult;
	expiresAt: number;
};

const CACHE_TTL_MS = 10 * 60 * 1000;

const geoipCache = new Map<string, CacheEntry>();

let maxmindReader: Reader<CityResponse> | null = null;
let maxmindReaderPromise: Promise<Reader<CityResponse>> | null = null;

function buildFallbackResult(normalizedIp: string): GeoipResult {
	return {
		countryCode: null,
		normalizedIp: normalizedIp || null,
		city: null,
		region: null,
		countryName: null,
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
			countryName: record.country?.names?.en ?? (countryCode ? countryDisplayName(countryCode) : null) ?? null,
		};
	} catch {
		return buildFallbackResult(clean);
	}
}

async function resolveGeoip(clean: string, dbPath: string): Promise<GeoipResult> {
	const now = Date.now();
	const cached = geoipCache.get(clean);
	if (cached && now < cached.expiresAt) {
		return cached.result;
	}

	const result = await lookupMaxmind(clean, dbPath);
	geoipCache.set(clean, {result, expiresAt: now + CACHE_TTL_MS});
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
