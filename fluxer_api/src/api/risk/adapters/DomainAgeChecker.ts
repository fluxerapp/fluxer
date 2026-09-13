// SPDX-License-Identifier: AGPL-3.0-or-later

import {isMajorEmailProvider} from '@app/api/risk/MajorEmailProviders';
import type {DomainAgeResult} from '@app/api/risk/RiskTypes';
import {EXTERNAL_RESPONSE_LIMITS} from '@app/api/utils/ExternalResponseLimits';
import * as FetchUtils from '@app/api/utils/FetchUtils';
import {isJsonRecord, parseJsonRecord} from '@app/api/utils/JsonBoundaryUtils';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';

const RDAP_TIMEOUT_MS = 4000;
const CACHE_KEY_PREFIX = 'risk:domain_age:';
const CACHE_TTL_SUCCESS_SECONDS = 30 * 24 * 60 * 60;
const CACHE_TTL_FAILURE_SECONDS = 15 * 60;

interface CachedDomainAge {
	domain: string;
	available: boolean;
	creationDate: string | null;
	failureNote: string | null;
}

interface RdapRegistrationEvent {
	eventAction: string;
	eventDate: string;
}

function readRdapRegistrationEvent(responseText: string): RdapRegistrationEvent | null {
	const data = parseJsonRecord(responseText);
	const events = data?.events;
	if (!Array.isArray(events)) {
		return null;
	}
	for (const event of events) {
		if (!isJsonRecord(event)) continue;
		if (event.eventAction === 'registration' && typeof event.eventDate === 'string') {
			return {
				eventAction: event.eventAction,
				eventDate: event.eventDate,
			};
		}
	}
	return null;
}

interface DomainAgeCheckerOptions {
	cacheService?: ICacheService;
}

export function createDomainAgeChecker(opts: DomainAgeCheckerOptions = {}) {
	const cache = opts.cacheService;
	async function readCache(domain: string): Promise<DomainAgeResult | null> {
		if (!cache) return null;
		try {
			const cached = await cache.get<CachedDomainAge>(cacheKey(domain));
			return cached ? hydrate(cached) : null;
		} catch {
			return null;
		}
	}
	async function writeCache(entry: CachedDomainAge): Promise<void> {
		if (!cache) return;
		const ttl = entry.available && entry.creationDate ? CACHE_TTL_SUCCESS_SECONDS : CACHE_TTL_FAILURE_SECONDS;
		try {
			await cache.set(cacheKey(entry.domain), entry, ttl);
		} catch {}
	}
	return async function checkDomainAge(args: {domain: string}): Promise<DomainAgeResult> {
		const domain = args.domain.toLowerCase().trim();
		if (isMajorEmailProvider(domain)) {
			return {
				domain,
				available: true,
				creationDate: null,
				ageDays: null,
				isNewlyRegistered: false,
				riskNote: 'well-known major provider — RDAP lookup skipped',
			};
		}
		const cached = await readCache(domain);
		if (cached) return cached;
		const entry = await fetchFromRdap(domain);
		await writeCache(entry);
		return hydrate(entry);
	};
}

async function fetchFromRdap(domain: string): Promise<CachedDomainAge> {
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), RDAP_TIMEOUT_MS);
	try {
		const response = await fetch(`https://rdap.org/domain/${encodeURIComponent(domain)}`, {
			signal: controller.signal,
			headers: {Accept: 'application/rdap+json'},
		});
		if (!response.ok) {
			FetchUtils.discardResponseBody(response.body, response.status);
			return {
				domain,
				available: false,
				creationDate: null,
				failureNote: `RDAP lookup failed: HTTP ${response.status}`,
			};
		}
		const responseText = await FetchUtils.streamToStringWithLimit(response.body, {
			maxBytes: EXTERNAL_RESPONSE_LIMITS.rdapBytes,
			headers: response.headers,
			url: response.url,
			description: 'RDAP response',
		});
		const registrationEvent = readRdapRegistrationEvent(responseText);
		if (!registrationEvent?.eventDate) {
			return {
				domain,
				available: true,
				creationDate: null,
				failureNote: 'RDAP response did not include a registration date',
			};
		}
		return {
			domain,
			available: true,
			creationDate: new Date(registrationEvent.eventDate).toISOString(),
			failureNote: null,
		};
	} catch (err) {
		const message = err instanceof Error ? err.message : String(err);
		return {
			domain,
			available: false,
			creationDate: null,
			failureNote: `RDAP lookup error: ${message.slice(0, 100)}`,
		};
	} finally {
		clearTimeout(timeout);
	}
}

function hydrate(entry: CachedDomainAge): DomainAgeResult {
	if (!entry.creationDate) {
		return {
			domain: entry.domain,
			available: entry.available,
			creationDate: null,
			ageDays: null,
			isNewlyRegistered: false,
			riskNote: entry.failureNote ?? 'RDAP response did not include a registration date',
		};
	}
	const createdAt = new Date(entry.creationDate);
	const ageDays = Math.floor((Date.now() - createdAt.getTime()) / (1000 * 60 * 60 * 24));
	const isNewlyRegistered = ageDays < 30;
	let riskNote = `domain registered ${ageDays} days ago`;
	if (ageDays < 7) {
		riskNote = `domain registered only ${ageDays} days ago — very fresh, high abuse risk`;
	} else if (ageDays < 30) {
		riskNote = `domain registered ${ageDays} days ago — newly registered, elevated abuse risk`;
	} else if (ageDays < 90) {
		riskNote = `domain registered ${ageDays} days ago — relatively new`;
	}
	return {
		domain: entry.domain,
		available: entry.available,
		creationDate: entry.creationDate,
		ageDays,
		isNewlyRegistered,
		riskNote,
	};
}

function cacheKey(domain: string): string {
	return `${CACHE_KEY_PREFIX}${domain}`;
}
