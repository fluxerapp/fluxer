// SPDX-License-Identifier: AGPL-3.0-or-later

import {Resolver} from 'node:dns/promises';
import {ms} from 'itty-time';
import {Config} from '../Config';
import {Logger} from '../Logger';
import type {IEmailDnsValidationService} from './IEmailDnsValidationService';

interface DomainValidationCacheEntry {
	valid: boolean;
	expiresAtMs: number;
}

interface IDnsResolver {
	resolveMx(domain: string): Promise<
		Array<{
			exchange: string;
			priority: number;
		}>
	>;
	resolve4(domain: string): Promise<Array<string>>;
	resolve6(domain: string): Promise<Array<string>>;
}

interface EmailDnsValidationServiceOptions {
	resolver?: IDnsResolver;
	enforceInTestMode?: boolean;
	positiveTtlMs?: number;
	negativeTtlMs?: number;
	lookupTimeoutMs?: number;
	maxCachedDomains?: number;
	isEmailEnabled?: () => Promise<boolean>;
}

type DnsResolutionResult = 'valid' | 'invalid' | 'fallback' | 'transient_error';
type DomainVerdict = 'valid' | 'invalid' | 'unverified';

const DOMAIN_NOT_FOUND_CODES = new Set(['ENOTFOUND', 'ENONAME', 'EAI_NONAME', 'NXDOMAIN']);
const DOMAIN_NO_RECORD_CODES = new Set(['ENODATA', 'ENOENT', 'NODATA']);
const DNS_LOOKUP_TIMEOUT_MS = 2000;
const DNS_LOOKUP_TRIES = 1;
const MAX_CACHED_DOMAINS = 10000;

async function isInstanceEmailEnabled(): Promise<boolean> {
	const {getInstanceConfigRepository} = await import('../middleware/ServiceSingletons');
	return getInstanceConfigRepository().isEmailEnabled();
}

function createLookupTimeoutError(): NodeJS.ErrnoException {
	const error: NodeJS.ErrnoException = new Error('Email DNS lookup timed out');
	error.code = 'ETIMEOUT';
	return error;
}

export class EmailDnsValidationService implements IEmailDnsValidationService {
	private readonly resolver: IDnsResolver;
	private readonly enforceInTestMode: boolean;
	private readonly positiveTtlMs: number;
	private readonly negativeTtlMs: number;
	private readonly lookupTimeoutMs: number;
	private readonly maxCachedDomains: number;
	private readonly isEmailEnabled: () => Promise<boolean>;
	private readonly domainCache = new Map<string, DomainValidationCacheEntry>();

	constructor(options: EmailDnsValidationServiceOptions = {}) {
		this.lookupTimeoutMs = options.lookupTimeoutMs ?? DNS_LOOKUP_TIMEOUT_MS;
		this.resolver =
			options.resolver ??
			new Resolver({timeout: this.lookupTimeoutMs, tries: DNS_LOOKUP_TRIES, maxTimeout: this.lookupTimeoutMs});
		this.enforceInTestMode = options.enforceInTestMode ?? false;
		this.positiveTtlMs = options.positiveTtlMs ?? ms('30 minutes');
		this.negativeTtlMs = options.negativeTtlMs ?? ms('5 minutes');
		this.maxCachedDomains = options.maxCachedDomains ?? MAX_CACHED_DOMAINS;
		this.isEmailEnabled = options.isEmailEnabled ?? isInstanceEmailEnabled;
	}

	async hasValidDnsRecords(email: string): Promise<boolean> {
		if (!this.enforceInTestMode && Config.dev.testModeEnabled) {
			return true;
		}
		const domain = this.extractDomain(email);
		if (!domain) {
			return false;
		}
		if (!(await this.isEmailEnabled())) {
			return true;
		}
		const cached = this.getCachedDomainResult(domain);
		if (cached !== null) {
			return cached;
		}
		const verdict = await this.resolveDomain(domain);
		if (verdict === 'invalid') {
			Logger.warn({domain}, 'Email domain publishes no mail exchange or address records, rejecting the address');
		}
		this.setCachedDomainResult(domain, verdict);
		return verdict !== 'invalid';
	}

	private extractDomain(email: string): string | null {
		const atIndex = email.lastIndexOf('@');
		if (atIndex <= 0 || atIndex === email.length - 1) {
			return null;
		}
		return email.slice(atIndex + 1).toLowerCase();
	}

	private getCachedDomainResult(domain: string): boolean | null {
		const cached = this.domainCache.get(domain);
		if (!cached) {
			return null;
		}
		if (Date.now() >= cached.expiresAtMs) {
			this.domainCache.delete(domain);
			return null;
		}
		this.domainCache.delete(domain);
		this.domainCache.set(domain, cached);
		return cached.valid;
	}

	private setCachedDomainResult(domain: string, verdict: DomainVerdict): void {
		const ttlMs = verdict === 'valid' ? this.positiveTtlMs : this.negativeTtlMs;
		if (this.domainCache.size >= this.maxCachedDomains && !this.domainCache.has(domain)) {
			const oldestDomain = this.domainCache.keys().next().value;
			if (oldestDomain !== undefined) {
				this.domainCache.delete(oldestDomain);
			}
		}
		this.domainCache.set(domain, {
			valid: verdict !== 'invalid',
			expiresAtMs: Date.now() + ttlMs,
		});
	}

	private async resolveDomain(domain: string): Promise<DomainVerdict> {
		const mxResult = await this.resolveMx(domain);
		if (mxResult === 'valid') {
			return 'valid';
		}
		if (mxResult === 'invalid') {
			return 'invalid';
		}
		if (mxResult === 'transient_error') {
			return 'unverified';
		}
		const addressResult = await this.resolveAddressRecords(domain);
		if (addressResult === 'valid') {
			return 'valid';
		}
		if (addressResult === 'invalid') {
			return 'invalid';
		}
		return 'unverified';
	}

	private async resolveMx(domain: string): Promise<DnsResolutionResult> {
		try {
			const records = await this.withLookupDeadline(this.resolver.resolveMx(domain));
			if (records.length > 0) {
				return 'valid';
			}
			return 'fallback';
		} catch (error) {
			return this.classifyResolverError(error, domain, true);
		}
	}

	private async resolveAddressRecords(domain: string): Promise<DnsResolutionResult> {
		const [ipv4Result, ipv6Result] = await Promise.allSettled([
			this.withLookupDeadline(this.resolver.resolve4(domain)),
			this.withLookupDeadline(this.resolver.resolve6(domain)),
		]);
		if (ipv4Result.status === 'fulfilled' && ipv4Result.value.length > 0) {
			return 'valid';
		}
		if (ipv6Result.status === 'fulfilled' && ipv6Result.value.length > 0) {
			return 'valid';
		}
		const ipv4Classification =
			ipv4Result.status === 'rejected' ? this.classifyResolverError(ipv4Result.reason, domain, false) : 'invalid';
		const ipv6Classification =
			ipv6Result.status === 'rejected' ? this.classifyResolverError(ipv6Result.reason, domain, false) : 'invalid';
		if (ipv4Classification === 'transient_error' || ipv6Classification === 'transient_error') {
			return 'transient_error';
		}
		return 'invalid';
	}

	private async withLookupDeadline<T>(lookup: Promise<T>): Promise<T> {
		let timer: ReturnType<typeof setTimeout> | undefined;
		try {
			return await Promise.race([
				lookup,
				new Promise<never>((_resolve, reject) => {
					timer = setTimeout(() => reject(createLookupTimeoutError()), this.lookupTimeoutMs);
				}),
			]);
		} finally {
			clearTimeout(timer);
		}
	}

	private classifyResolverError(
		error: unknown,
		domain: string,
		allowFallbackForNoRecords: boolean,
	): DnsResolutionResult {
		const code = this.extractErrorCode(error);
		if (code && DOMAIN_NOT_FOUND_CODES.has(code)) {
			return 'invalid';
		}
		if (code && DOMAIN_NO_RECORD_CODES.has(code)) {
			return allowFallbackForNoRecords ? 'fallback' : 'invalid';
		}
		Logger.warn({domain, code, error}, 'Email DNS lookup failed with a transient error, allowing request');
		return 'transient_error';
	}

	private extractErrorCode(error: unknown): string | null {
		if (!error || typeof error !== 'object' || !('code' in error)) {
			return null;
		}
		const code = (
			error as {
				code?: unknown;
			}
		).code;
		return typeof code === 'string' ? code : null;
	}
}
