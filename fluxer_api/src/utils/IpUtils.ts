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

import dns from 'node:dns';
import {isIPv4, isIPv6} from 'node:net';
import maxmind, {type CityResponse, type Reader} from 'maxmind';
import {Config} from '~/Config';
import type {ICacheService} from '~/infrastructure/ICacheService';
import {Logger} from '~/Logger';

const CACHE_TTL_MS = 10 * 60 * 1000;
const REVERSE_DNS_CACHE_TTL_SECONDS = 24 * 60 * 60;
const REVERSE_DNS_CACHE_PREFIX = 'reverse-dns:';

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

const geoipCache = new Map<string, CacheEntry>();

let maxmindReader: Reader<CityResponse> | null = null;
let maxmindReaderPromise: Promise<Reader<CityResponse>> | null = null;

export function __clearIpCache(): void {
	geoipCache.clear();
}

export function __resetMaxmindReader(): void {
	maxmindReader = null;
	maxmindReaderPromise = null;
}

export function extractClientIp(req: Request): string | null {
	const xff = (req.headers.get('X-Forwarded-For') ?? '').trim();
	if (!xff) return null;
	const [first] = xff.split(',');
	if (!first) return null;
	return normalizeIpString(first);
}

export function requireClientIp(req: Request): string {
	const ip = extractClientIp(req);
	if (!ip) {
		throw new Error('X-Forwarded-For header is required for this endpoint');
	}
	return ip;
}

export async function lookupGeoip(req: Request): Promise<GeoipResult>;
export async function lookupGeoip(ip: string): Promise<GeoipResult>;
export async function lookupGeoip(input: string | Request): Promise<GeoipResult> {
	const ip = typeof input === 'string' ? input : extractClientIp(input);
	if (!ip) {
		return buildFallbackResult('');
	}
	return lookupGeoipFromString(ip);
}

function buildFallbackResult(clean: string): GeoipResult {
	return {
		countryCode: null,
		normalizedIp: clean || null,
		city: null,
		region: null,
		countryName: null,
	};
}

async function ensureMaxmindReader(): Promise<Reader<CityResponse>> {
	if (maxmindReader) return maxmindReader;

	if (!maxmindReaderPromise) {
		const dbPath = Config.geoip.maxmindDbPath;
		if (!dbPath) {
			throw new Error('Missing MaxMind DB path');
		}

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

async function lookupMaxmind(clean: string): Promise<GeoipResult> {
	const dbPath = Config.geoip.maxmindDbPath;
	if (!dbPath) {
		return buildFallbackResult(clean);
	}

	try {
		const reader = await ensureMaxmindReader();
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
	} catch (error) {
		const message = (error as Error).message ?? 'unknown';
		Logger.warn({error, maxmind_db_path: dbPath, message}, 'MaxMind lookup failed');
		return buildFallbackResult(clean);
	}
}

async function resolveGeoip(clean: string): Promise<GeoipResult> {
	const now = Date.now();
	const cached = geoipCache.get(clean);
	if (cached && now < cached.expiresAt) {
		return cached.result;
	}

	const result = await lookupMaxmind(clean);
	geoipCache.set(clean, {result, expiresAt: now + CACHE_TTL_MS});
	return result;
}

async function lookupGeoipFromString(value: string): Promise<GeoipResult> {
	const clean = normalizeIpString(value);
	if (!isIPv4(clean) && !isIPv6(clean)) {
		return buildFallbackResult(clean);
	}

	return resolveGeoip(clean);
}

function countryDisplayName(code: string, locale = 'en'): string | null {
	const upper = code.toUpperCase();
	if (!isAsciiUpperAlpha2(upper)) return null;
	const dn = new Intl.DisplayNames([locale], {type: 'region', fallback: 'none'});
	return dn.of(upper) ?? null;
}

export function formatGeoipLocation(result: GeoipResult): string | null {
	const parts: Array<string> = [];
	if (result.city) parts.push(result.city);
	if (result.region) parts.push(result.region);
	const countryLabel = result.countryName ?? result.countryCode;
	if (countryLabel) parts.push(countryLabel);
	return parts.length > 0 ? parts.join(', ') : null;
}

function stripBrackets(value: string): string {
	return value.startsWith('[') && value.endsWith(']') ? value.slice(1, -1) : value;
}

export function normalizeIpString(value: string): string {
	const trimmed = value.trim();
	const stripped = stripBrackets(trimmed);
	const zoneIndex = stripped.indexOf('%');
	return zoneIndex === -1 ? stripped : stripped.slice(0, zoneIndex);
}

export async function getIpAddressReverse(ip: string, cacheService?: ICacheService): Promise<string | null> {
	const cacheKey = `${REVERSE_DNS_CACHE_PREFIX}${ip}`;
	if (cacheService) {
		const cached = await cacheService.get<string | null>(cacheKey);
		if (cached !== null) return cached === '' ? null : cached;
	}

	let result: string | null = null;
	try {
		const hostnames = await dns.promises.reverse(ip);
		result = hostnames[0] ?? null;
	} catch {
		result = null;
	}

	if (cacheService) {
		await cacheService.set(cacheKey, result ?? '', REVERSE_DNS_CACHE_TTL_SECONDS);
	}

	return result;
}

export async function getLocationLabelFromIp(ip: string): Promise<string | null> {
	const result = await lookupGeoip(ip);
	return formatGeoipLocation(result);
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
