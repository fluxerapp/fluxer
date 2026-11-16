// SPDX-License-Identifier: AGPL-3.0-or-later

import {Resolver} from 'node:dns/promises';
import {Config} from '@app/api/Config';
import {Logger} from '@app/api/Logger';
import {ms} from 'itty-time';

interface DomainValidationCacheEntry {
	valid: boolean;
	mailHosts: ReadonlyArray<string>;
	mailHostAddresses?: ReadonlyArray<string>;
	expiresAtMs: number;
}

interface DomainLookup {
	verdict: DomainVerdict;
	mailHosts: ReadonlyArray<string>;
}

interface MxLookup {
	result: DnsResolutionResult;
	mailHosts: ReadonlyArray<string>;
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
	const {getInstanceConfigRepository} = await import('@app/api/middleware/ServiceSingletons');
	return getInstanceConfigRepository().isEmailEnabled();
}

function createLookupTimeoutError(): NodeJS.ErrnoException {
	const error: NodeJS.ErrnoException = new Error('Email DNS lookup timed out');
	error.code = 'ETIMEOUT';
	return error;
}

export class EmailDnsValidationService {
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
		return (await this.lookupDomain(domain)).valid;
	}

	async mailHosts(email: string): Promise<ReadonlyArray<string>> {
		if (!this.enforceInTestMode && Config.dev.testModeEnabled) {
			return [];
		}
		const domain = this.extractDomain(email);
		if (!domain || !(await this.isEmailEnabled())) {
			return [];
		}
		return (await this.lookupDomain(domain)).mailHosts;
	}

	async mailHostAddresses(email: string): Promise<ReadonlyArray<string>> {
		const hosts = await this.mailHosts(email);
		if (hosts.length === 0) {
			return [];
		}
		const domain = this.extractDomain(email);
		const cached = domain ? this.domainCache.get(domain) : undefined;
		if (cached?.mailHostAddresses) {
			return cached.mailHostAddresses;
		}
		const addresses = new Set<string>();
		for (const host of hosts) {
			const [ipv4, ipv6] = await Promise.allSettled([
				this.withLookupDeadline(this.resolver.resolve4(host)),
				this.withLookupDeadline(this.resolver.resolve6(host)),
			]);
			for (const result of [ipv4, ipv6]) {
				if (result.status === 'fulfilled') {
					for (const address of result.value) {
						addresses.add(address);
					}
				}
			}
		}
		const resolved = [...addresses];
		if (cached) {
			cached.mailHostAddresses = resolved;
		}
		return resolved;
	}

	private async lookupDomain(domain: string): Promise<{valid: boolean; mailHosts: ReadonlyArray<string>}> {
		const cached = this.getCachedDomainResult(domain);
		if (cached !== null) {
			return cached;
		}
		const lookup = await this.resolveDomain(domain);
		if (lookup.verdict === 'invalid') {
			Logger.warn({domain}, 'Email domain publishes no mail exchange or address records, rejecting the address');
		}
		this.setCachedDomainResult(domain, lookup);
		return {valid: lookup.verdict !== 'invalid', mailHosts: lookup.mailHosts};
	}

	private extractDomain(email: string): string | null {
		const atIndex = email.lastIndexOf('@');
		if (atIndex <= 0 || atIndex === email.length - 1) {
			return null;
		}
		return email.slice(atIndex + 1).toLowerCase();
	}

	private getCachedDomainResult(domain: string): {valid: boolean; mailHosts: ReadonlyArray<string>} | null {
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
		return {valid: cached.valid, mailHosts: cached.mailHosts};
	}

	private setCachedDomainResult(domain: string, {verdict, mailHosts}: DomainLookup): void {
		const ttlMs = verdict === 'valid' ? this.positiveTtlMs : this.negativeTtlMs;
		if (this.domainCache.size >= this.maxCachedDomains && !this.domainCache.has(domain)) {
			const oldestDomain = this.domainCache.keys().next().value;
			if (oldestDomain !== undefined) {
				this.domainCache.delete(oldestDomain);
			}
		}
		this.domainCache.set(domain, {
			valid: verdict !== 'invalid',
			mailHosts,
			expiresAtMs: Date.now() + ttlMs,
		});
	}

	private async resolveDomain(domain: string): Promise<DomainLookup> {
		const mx = await this.resolveMx(domain);
		if (mx.result === 'valid') {
			return {verdict: 'valid', mailHosts: mx.mailHosts};
		}
		if (mx.result === 'invalid') {
			return {verdict: 'invalid', mailHosts: []};
		}
		if (mx.result === 'transient_error') {
			return {verdict: 'unverified', mailHosts: []};
		}
		const addressResult = await this.resolveAddressRecords(domain);
		if (addressResult === 'valid') {
			return {verdict: 'valid', mailHosts: []};
		}
		if (addressResult === 'invalid') {
			return {verdict: 'invalid', mailHosts: []};
		}
		return {verdict: 'unverified', mailHosts: []};
	}

	private async resolveMx(domain: string): Promise<MxLookup> {
		try {
			const records = await this.withLookupDeadline(this.resolver.resolveMx(domain));
			if (records.length > 0) {
				const mailHosts = records
					.map((record) => record.exchange.trim().toLowerCase().replace(/\.$/, ''))
					.filter((host) => host.length > 0);
				return {result: 'valid', mailHosts};
			}
			return {result: 'fallback', mailHosts: []};
		} catch (error) {
			return {result: this.classifyResolverError(error, domain, true), mailHosts: []};
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
		let timer: NodeJS.Timeout | undefined;
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
