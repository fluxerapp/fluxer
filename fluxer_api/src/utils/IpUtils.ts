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

export const DEFAULT_CC = 'US';
export const FETCH_TIMEOUT_MS = 1500;
export const UNKNOWN_LOCATION = 'Unknown Location';
const REVERSE_DNS_CACHE_TTL_SECONDS = 24 * 60 * 60;
const REVERSE_DNS_CACHE_PREFIX = 'reverse-dns:';

export interface GeoipResult {
	countryCode: string;
	normalizedIp: string | null;
	reason: string | null;
	city: string | null;
	region: string | null;
	countryName: string | null;
}

interface CacheVal {
	result: GeoipResult;
	exp: number;
}

const cache = new Map<string, CacheVal>();

let maxmindReader: Reader<CityResponse> | null = null;
let maxmindReaderPromise: Promise<Reader<CityResponse>> | null = null;

export function __clearIpCache(): void {
	cache.clear();
}

export function extractClientIp(req: Request): string | null {
	const xff = (req.headers.get('X-Forwarded-For') ?? '').trim();
	if (!xff) {
		return null;
	}
	const first = xff.split(',')[0]?.trim() ?? '';
	return normalizeIpString(first);
}

export function requireClientIp(req: Request): string {
	const ip = extractClientIp(req);
	if (!ip) {
		throw new Error('X-Forwarded-For header is required for this endpoint');
	}
	return ip;
}

function buildFallbackResult(clean: string, reason: string | null): GeoipResult {
	return {
		countryCode: DEFAULT_CC,
		normalizedIp: clean,
		reason,
		city: null,
		region: null,
		countryName: null,
	};
}

async function getMaxmindReader(): Promise<Reader<CityResponse>> {
	if (maxmindReader) return maxmindReader;

	if (!maxmindReaderPromise) {
		const dbPath = Config.geoip.maxmindDbPath;
		if (!dbPath) {
			return Promise.reject(new Error('Missing MaxMind DB path'));
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

function getSubdivisionLabel(record?: CityResponse): string | null {
	const subdivision = record?.subdivisions?.[0];
	if (!subdivision) return null;
	return subdivision.iso_code || subdivision.names?.en || null;
}

async function lookupMaxmind(clean: string): Promise<GeoipResult> {
	const dbPath = Config.geoip.maxmindDbPath;
	if (!dbPath) {
		return buildFallbackResult(clean, 'maxmind_db_missing');
	}

	try {
		const reader = await getMaxmindReader();
		const record = reader.get(clean);
		if (!record) {
			return buildFallbackResult(clean, 'maxmind_not_found');
		}

		const countryCode = (record.country?.iso_code ?? DEFAULT_CC).toUpperCase();
		return {
			countryCode,
			normalizedIp: clean,
			reason: null,
			city: record.city?.names?.en ?? null,
			region: getSubdivisionLabel(record),
			countryName: record.country?.names?.en ?? countryDisplayName(countryCode) ?? null,
		};
	} catch (error) {
		const message = (error as Error)?.message ?? 'unknown';
		Logger.warn({error, maxmind_db_path: dbPath}, 'MaxMind lookup failed');
		return buildFallbackResult(clean, `maxmind_error:${message}`);
	}
}

async function lookupIpinfo(clean: string): Promise<GeoipResult> {
	const host = Config.geoip.host;
	if (!host) {
		return buildFallbackResult(clean, 'geoip_host_missing');
	}

	const url = `http://${host}/lookup?ip=${encodeURIComponent(clean)}`;
	const controller = new AbortController();
	const timer = setTimeout(() => controller.abort(), FETCH_TIMEOUT_MS);

	try {
		const res = await globalThis.fetch!(url, {
			signal: controller.signal,
			headers: {Accept: 'text/plain'},
		});
		if (!res.ok) {
			return buildFallbackResult(clean, `non_ok:${res.status}`);
		}

		const text = (await res.text()).trim().toUpperCase();
		const countryCode = isAsciiUpperAlpha2(text) ? text : DEFAULT_CC;
		const reason = isAsciiUpperAlpha2(text) ? null : 'invalid_response';
		return {
			countryCode,
			normalizedIp: clean,
			reason,
			city: null,
			region: null,
			countryName: countryDisplayName(countryCode),
		};
	} catch (error) {
		const message = (error as Error)?.message ?? 'unknown';
		return buildFallbackResult(clean, `error:${message}`);
	} finally {
		clearTimeout(timer);
	}
}

export async function getCountryCodeDetailed(ip: string): Promise<GeoipResult> {
	const clean = normalizeIpString(ip);
	if (!isIPv4(clean) && !isIPv6(clean)) {
		return buildFallbackResult(clean, 'invalid_ip');
	}

	const cached = cache.get(clean);
	const now = Date.now();
	if (cached && now < cached.exp) {
		return cached.result;
	}

	const provider = Config.geoip.provider;
	const result = provider === 'maxmind' ? await lookupMaxmind(clean) : await lookupIpinfo(clean);

	cache.set(clean, {result, exp: now + CACHE_TTL_MS});
	return result;
}

export async function getCountryCode(ip: string): Promise<string> {
	const result = await getCountryCodeDetailed(ip);
	return result.countryCode;
}

export async function getCountryCodeFromReq(req: Request): Promise<string> {
	const ip = extractClientIp(req);
	if (!ip) return DEFAULT_CC;
	return await getCountryCode(ip);
}

function countryDisplayName(code: string, locale = 'en'): string | null {
	const c = code.toUpperCase();
	if (!isAsciiUpperAlpha2(c)) return null;
	const dn = new Intl.DisplayNames([locale], {type: 'region', fallback: 'none'});
	return dn.of(c) ?? null;
}

export function formatGeoipLocation(result: GeoipResult): string {
	const parts: Array<string> = [];
	if (result.city) parts.push(result.city);
	if (result.region) parts.push(result.region);
	const countryLabel = result.countryName ?? result.countryCode;
	if (countryLabel) parts.push(countryLabel);
	return parts.length > 0 ? parts.join(', ') : UNKNOWN_LOCATION;
}

function stripBrackets(s: string): string {
	return s.startsWith('[') && s.endsWith(']') ? s.slice(1, -1) : s;
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
		if (cached !== null) {
			return cached === '' ? null : cached;
		}
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

function isAsciiUpperAlpha2(s: string): boolean {
	if (s.length !== 2) return false;
	const a = s.charCodeAt(0);
	const b = s.charCodeAt(1);
	return a >= 65 && a <= 90 && b >= 65 && b <= 90;
}
