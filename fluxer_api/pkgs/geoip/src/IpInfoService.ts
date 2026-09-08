// SPDX-License-Identifier: AGPL-3.0-or-later

import {getSameIpDecisionKey} from '@fluxer/ip_utils/src/IpAddress';
import {z} from 'zod';

const IPINFO_BASE_URL = 'https://api.ipinfo.io/lookup';
const FETCH_TIMEOUT_MS = 3000;
const CACHE_KEY_PREFIX = 'ipinfo:max:';
const ISO_DATE_REGEX = /^\d{4}-\d{2}-\d{2}$/u;
const POSITIVE_CACHE_TTL_SECONDS = 7 * 24 * 60 * 60;
const NEGATIVE_CACHE_TTL_SECONDS = 14 * 24 * 60 * 60;
const FAILURE_TTL_REQUEST_FAILED_SECONDS = 60;
const FAILURE_TTL_HTTP_ERROR_SECONDS = 300;
const FAILURE_TTL_QUOTA_SECONDS = 900;
const FAILURE_TTL_SCHEMA_MISMATCH_SECONDS = 600;
const FAILURE_TTL_BACKGROUND_CAP_SECONDS = 120;

export interface IpInfoGeoBlock {
	countryCode: string | null;
	countryName: string | null;
	continent: string | null;
	continentCode: string | null;
	region: string | null;
	regionCode: string | null;
	city: string | null;
	postalCode: string | null;
	timezone: string | null;
	latitude: number | null;
	longitude: number | null;
	accuracyRadiusKm: number | null;
}

export interface IpInfoAsnBlock {
	asn: string | null;
	number: number | null;
	name: string | null;
	domain: string | null;
	type: string | null;
}

export interface IpInfoMobileBlock {
	name: string | null;
	mcc: string | null;
	mnc: string | null;
}

export interface IpInfoAnonymousBlock {
	isAnonymous: boolean;
	providerName: string | null;
	isVpn: boolean;
	isProxy: boolean;
	isResidentialProxy: boolean;
	isTor: boolean;
	isRelay: boolean;
	percentDaysSeen: number | null;
}

export interface IpInfoFlags {
	isAnycast: boolean;
	isHosting: boolean;
	isMobile: boolean;
	isSatellite: boolean;
}

export interface IpInfoLookupResult {
	ip: string;
	available: boolean;
	riskNote: string;
	geo: IpInfoGeoBlock;
	asn: IpInfoAsnBlock;
	mobile: IpInfoMobileBlock;
	anonymous: IpInfoAnonymousBlock;
	flags: IpInfoFlags;
}

export interface IpInfoCache {
	get<T>(key: string): Promise<T | null>;
	set<T>(key: string, value: T, ttlSeconds?: number): Promise<void>;
}

export type IpInfoLookupPriority = 'critical' | 'standard' | 'background';

export interface IpInfoLookupBudget {
	tryConsume(priority: IpInfoLookupPriority): Promise<boolean>;
}

export interface CachedIpInfoFailure extends IpInfoLookupResult {
	cachedFailure: true;
	failureOutcome: 'http_error' | 'request_failed' | 'schema_mismatch';
	failureHttpStatus: number | null;
	cachedAtMs: number;
}

export function resolveIpInfoLookupPriority(source: string | undefined): IpInfoLookupPriority {
	if (source === 'admin.ip_ban' || source === 'admin.scheduled_deletion_suspicious_ip') return 'critical';
	if (source === 'AbusiveIpAutoBanner') return 'background';
	return 'standard';
}

export function isCachedIpInfoFailure(value: unknown): value is CachedIpInfoFailure {
	return typeof value === 'object' && value !== null && (value as {available?: unknown}).available === false;
}

function failureCacheTtlSeconds(
	outcome: CachedIpInfoFailure['failureOutcome'],
	httpStatus: number | null,
	priority: IpInfoLookupPriority,
): number {
	let ttl = FAILURE_TTL_HTTP_ERROR_SECONDS;
	if (outcome === 'request_failed') ttl = FAILURE_TTL_REQUEST_FAILED_SECONDS;
	else if (outcome === 'schema_mismatch') ttl = FAILURE_TTL_SCHEMA_MISMATCH_SECONDS;
	else if (httpStatus === 402 || httpStatus === 403 || httpStatus === 429) ttl = FAILURE_TTL_QUOTA_SECONDS;
	return priority === 'background' ? Math.min(ttl, FAILURE_TTL_BACKGROUND_CAP_SECONDS) : ttl;
}

export interface IpInfoLookupContext {
	source?: string;
	reason?: string;
	metadata?: Record<string, string | number | boolean | null>;
}

export interface IpInfoRequestAuditEvent {
	requestedAt: Date;
	ip: string;
	cacheKey: string;
	source: string;
	reason: string | null;
	metadata?: Record<string, string | number | boolean | null>;
	outcome: 'http_success' | 'http_error' | 'request_failed' | 'schema_mismatch' | 'budget_shed';
	httpStatus: number | null;
	available: boolean;
	riskNote: string;
	latencyMs: number;
	requestUrl: string;
	responseIp: string | null;
	countryCode: string | null;
	asnNumber: number | null;
	isAnonymous: boolean;
	isTor: boolean;
	isVpn: boolean;
	isProxy: boolean;
	isResidentialProxy: boolean;
}

export interface IpInfoRequestAuditLogger {
	record(event: IpInfoRequestAuditEvent): Promise<void>;
}

interface IpInfoServiceContext {
	apiKey: string;
	cache: IpInfoCache;
	auditLogger?: IpInfoRequestAuditLogger;
	budget?: IpInfoLookupBudget;
}

export interface IpInfoService {
	lookup(ip: string, context?: IpInfoLookupContext): Promise<IpInfoLookupResult>;
}

const IpInfoDateSchema = z.string().regex(ISO_DATE_REGEX);
const RawIpInfoGeoSchema = z.object({
	city: z.string().optional(),
	region: z.string().optional(),
	region_code: z.string().optional(),
	country: z.string().optional(),
	country_code: z.string().optional(),
	continent: z.string().optional(),
	continent_code: z.string().optional(),
	latitude: z.number().optional(),
	longitude: z.number().optional(),
	timezone: z.string().optional(),
	postal_code: z.string().optional(),
	dma_code: z.string().optional(),
	geoname_id: z.string().optional(),
	radius: z.number().int().optional(),
	last_changed: IpInfoDateSchema.optional(),
});
const RawIpInfoAsSchema = z.object({
	asn: z.string().optional(),
	name: z.string().optional(),
	domain: z.string().optional(),
	type: z.string().optional(),
	last_changed: IpInfoDateSchema.optional(),
});
const RawIpInfoMobileSchema = z.object({
	name: z.string().optional(),
	mcc: z.string().optional(),
	mnc: z.string().optional(),
});
const RawIpInfoAnonymousSchema = z.object({
	name: z.string().optional(),
	last_seen: IpInfoDateSchema.optional(),
	percent_days_seen: z.number().int().optional(),
	is_proxy: z.boolean().optional(),
	is_relay: z.boolean().optional(),
	is_tor: z.boolean().optional(),
	is_vpn: z.boolean().optional(),
	is_res_proxy: z.boolean().optional(),
});
const RawIpInfoResponseSchema = z.object({
	ip: z.string(),
	hostname: z.string().optional(),
	geo: RawIpInfoGeoSchema,
	as: RawIpInfoAsSchema,
	mobile: RawIpInfoMobileSchema.optional(),
	anonymous: RawIpInfoAnonymousSchema,
	is_anonymous: z.boolean().optional(),
	is_anycast: z.boolean().optional(),
	is_hosting: z.boolean().optional(),
	is_mobile: z.boolean().optional(),
	is_satellite: z.boolean().optional(),
});

type RawIpInfoResponse = z.infer<typeof RawIpInfoResponseSchema>;

export function createIpInfoService(ctx: IpInfoServiceContext): IpInfoService {
	const inflight: Map<string, Promise<IpInfoLookupResult>> = new Map();
	return {
		async lookup(ip: string, context?: IpInfoLookupContext): Promise<IpInfoLookupResult> {
			const cacheKey = `${CACHE_KEY_PREFIX}${getSameIpDecisionKey(ip) ?? ip}`;
			const priority = resolveIpInfoLookupPriority(context?.source);
			const cached = await ctx.cache.get<IpInfoLookupResult>(cacheKey);
			if (cached !== null) {
				if (isCachedIpInfoFailure(cached)) {
					return unavailable(ip, cached.riskNote);
				}
				return {...cached, ip};
			}
			const existing = inflight.get(cacheKey);
			if (existing) {
				const result = await existing;
				return {...result, ip};
			}
			const requestedAt = new Date();
			const startedAt = Date.now();
			const requestUrl = `${IPINFO_BASE_URL}/${encodeURIComponent(ip)}`;
			const fetchUrl = `${requestUrl}?token=${encodeURIComponent(ctx.apiKey)}`;
			const finalize = async (params: {
				result: IpInfoLookupResult;
				outcome: IpInfoRequestAuditEvent['outcome'];
				httpStatus: number | null;
			}): Promise<IpInfoLookupResult> => {
				await ctx.auditLogger
					?.record({
						requestedAt,
						ip,
						cacheKey,
						source: context?.source ?? 'unknown',
						reason: context?.reason ?? null,
						metadata: context?.metadata,
						outcome: params.outcome,
						httpStatus: params.httpStatus,
						available: params.result.available,
						riskNote: params.result.riskNote,
						latencyMs: Date.now() - startedAt,
						requestUrl,
						responseIp: params.result.available ? params.result.ip : null,
						countryCode: params.result.geo.countryCode,
						asnNumber: params.result.asn.number,
						isAnonymous: params.result.anonymous.isAnonymous,
						isTor: params.result.anonymous.isTor,
						isVpn: params.result.anonymous.isVpn,
						isProxy: params.result.anonymous.isProxy,
						isResidentialProxy: params.result.anonymous.isResidentialProxy,
					})
					.catch(() => {});
				return params.result;
			};
			const performLookup = async (): Promise<IpInfoLookupResult> => {
				if (ctx.budget && !(await ctx.budget.tryConsume(priority))) {
					return finalize({
						result: unavailable(ip, `IPInfo lookup shed (budget exhausted, priority: ${priority})`),
						outcome: 'budget_shed',
						httpStatus: null,
					});
				}
				const finalizeFailure = async (params: {
					result: IpInfoLookupResult;
					outcome: CachedIpInfoFailure['failureOutcome'];
					httpStatus: number | null;
				}): Promise<IpInfoLookupResult> => {
					const entry: CachedIpInfoFailure = {
						...params.result,
						cachedFailure: true,
						failureOutcome: params.outcome,
						failureHttpStatus: params.httpStatus,
						cachedAtMs: Date.now(),
					};
					await ctx.cache
						.set(cacheKey, entry, failureCacheTtlSeconds(params.outcome, params.httpStatus, priority))
						.catch(() => {});
					return finalize(params);
				};
				let payload: unknown;
				try {
					const res = await fetch(fetchUrl, {
						signal: AbortSignal.timeout(FETCH_TIMEOUT_MS),
						headers: {Accept: 'application/json'},
					});
					if (!res.ok) {
						return finalizeFailure({
							result: unavailable(ip, `IPInfo HTTP ${res.status}`),
							outcome: 'http_error',
							httpStatus: res.status,
						});
					}
					payload = await res.json();
				} catch (err) {
					const detail = err instanceof Error ? err.message : String(err);
					return finalizeFailure({
						result: unavailable(ip, `IPInfo request failed: ${detail}`),
						outcome: 'request_failed',
						httpStatus: null,
					});
				}
				const parsedResponse = RawIpInfoResponseSchema.safeParse(payload);
				if (!parsedResponse.success) {
					return finalizeFailure({
						result: unavailable(ip, formatSchemaMismatch(parsedResponse.error)),
						outcome: 'schema_mismatch',
						httpStatus: 200,
					});
				}
				const result = parseIpInfoResponse(parsedResponse.data);
				const ttl = result.anonymous.isAnonymous ? POSITIVE_CACHE_TTL_SECONDS : NEGATIVE_CACHE_TTL_SECONDS;
				await ctx.cache.set(cacheKey, result, ttl).catch(() => {});
				return finalize({
					result,
					outcome: 'http_success',
					httpStatus: 200,
				});
			};
			const promise: Promise<IpInfoLookupResult> = performLookup().finally(() => {
				if (inflight.get(cacheKey) === promise) {
					inflight.delete(cacheKey);
				}
			});
			inflight.set(cacheKey, promise);
			return promise;
		},
	};
}

export function createUnavailableIpInfoService(reason = 'IPInfo not configured'): IpInfoService {
	return {
		async lookup(ip: string): Promise<IpInfoLookupResult> {
			return unavailable(ip, reason);
		},
	};
}

function unavailable(ip: string, reason: string): IpInfoLookupResult {
	return {
		ip,
		available: false,
		riskNote: reason,
		geo: emptyGeo(),
		asn: emptyAsn(),
		mobile: emptyMobile(),
		anonymous: emptyAnonymous(),
		flags: emptyFlags(),
	};
}

function emptyGeo(): IpInfoGeoBlock {
	return {
		countryCode: null,
		countryName: null,
		continent: null,
		continentCode: null,
		region: null,
		regionCode: null,
		city: null,
		postalCode: null,
		timezone: null,
		latitude: null,
		longitude: null,
		accuracyRadiusKm: null,
	};
}

function emptyAsn(): IpInfoAsnBlock {
	return {asn: null, number: null, name: null, domain: null, type: null};
}

function emptyMobile(): IpInfoMobileBlock {
	return {name: null, mcc: null, mnc: null};
}

function emptyAnonymous(): IpInfoAnonymousBlock {
	return {
		isAnonymous: false,
		providerName: null,
		isVpn: false,
		isProxy: false,
		isResidentialProxy: false,
		isTor: false,
		isRelay: false,
		percentDaysSeen: null,
	};
}

function emptyFlags(): IpInfoFlags {
	return {isAnycast: false, isHosting: false, isMobile: false, isSatellite: false};
}

function parseIpInfoResponse(raw: RawIpInfoResponse): IpInfoLookupResult {
	const geo = raw.geo;
	const anon = raw.anonymous;
	const isAnonymous =
		raw.is_anonymous === true ||
		anon.is_res_proxy === true ||
		anon.is_vpn === true ||
		anon.is_proxy === true ||
		anon.is_tor === true ||
		anon.is_relay === true;
	return {
		ip: raw.ip,
		available: true,
		riskNote: buildRiskNote(isAnonymous, anon),
		geo: {
			countryCode: normalizeCountryCode(geo.country_code),
			countryName: geo.country ?? null,
			continent: geo?.continent ?? null,
			continentCode: normalizeContinentCode(geo.continent_code),
			region: geo.region ?? null,
			regionCode: normalizeRegionCode(geo.region_code),
			city: geo.city ?? null,
			postalCode: geo.postal_code ?? null,
			timezone: geo.timezone ?? null,
			latitude: normalizeCoordinate(geo.latitude),
			longitude: normalizeCoordinate(geo.longitude),
			accuracyRadiusKm: typeof geo?.radius === 'number' && Number.isFinite(geo.radius) ? geo.radius : null,
		},
		asn: parseAsnBlock(raw.as),
		mobile: {
			name: raw.mobile?.name ?? null,
			mcc: raw.mobile?.mcc ?? null,
			mnc: raw.mobile?.mnc ?? null,
		},
		anonymous: {
			isAnonymous,
			providerName: anon?.name ?? null,
			isVpn: anon?.is_vpn === true,
			isProxy: anon?.is_proxy === true,
			isResidentialProxy: anon?.is_res_proxy === true,
			isTor: anon?.is_tor === true,
			isRelay: anon?.is_relay === true,
			percentDaysSeen: typeof anon?.percent_days_seen === 'number' ? anon.percent_days_seen : null,
		},
		flags: {
			isAnycast: raw.is_anycast === true,
			isHosting: raw.is_hosting === true,
			isMobile: raw.is_mobile === true,
			isSatellite: raw.is_satellite === true,
		},
	};
}

function formatSchemaMismatch(error: z.ZodError): string {
	const issue = error.issues[0];
	if (!issue) {
		return 'IPInfo response schema mismatch';
	}
	const path = issue.path.length > 0 ? issue.path.join('.') : '<root>';
	return `IPInfo response schema mismatch at ${path}: ${issue.message}`;
}

function parseAsnBlock(as: RawIpInfoResponse['as']): IpInfoAsnBlock {
	const raw = as?.asn ?? null;
	const numeric = raw ? Number(raw.replace(/^AS/i, '')) : Number.NaN;
	return {
		asn: raw,
		number: Number.isFinite(numeric) ? numeric : null,
		name: as?.name ?? null,
		domain: as?.domain ?? null,
		type: as?.type ?? null,
	};
}

function buildRiskNote(isAnonymous: boolean, anon: RawIpInfoResponse['anonymous']): string {
	if (!isAnonymous) {
		return 'IPInfo: IP is not anonymous';
	}
	if (!anon) {
		return 'IPInfo: anonymous IP';
	}
	const flags: Array<string> = [];
	if (anon.is_res_proxy) flags.push('residential proxy');
	if (anon.is_vpn) flags.push('VPN');
	if (anon.is_proxy) flags.push('proxy');
	if (anon.is_tor) flags.push('Tor');
	if (anon.is_relay) flags.push('relay');
	const provider = anon.name ? ` (provider: ${anon.name})` : '';
	const seen = anon.percent_days_seen != null ? `, seen ${anon.percent_days_seen}% of days` : '';
	return `IPInfo: anonymous IP${provider} — ${flags.join(', ')}${seen}`;
}

function normalizeCountryCode(value: string | undefined): string | null {
	if (!value) {
		return null;
	}
	const normalized = value.trim().toUpperCase();
	return /^[A-Z]{2}$/u.test(normalized) ? normalized : null;
}

function normalizeContinentCode(value: string | undefined): string | null {
	if (!value) {
		return null;
	}
	const normalized = value.trim().toUpperCase();
	return /^[A-Z]{2}$/u.test(normalized) ? normalized : null;
}

function normalizeRegionCode(value: string | undefined): string | null {
	if (!value) {
		return null;
	}
	const normalized = value.trim().toUpperCase();
	return normalized.length > 0 ? normalized : null;
}

function normalizeCoordinate(value: number | undefined): number | null {
	return typeof value === 'number' && Number.isFinite(value) ? value : null;
}
