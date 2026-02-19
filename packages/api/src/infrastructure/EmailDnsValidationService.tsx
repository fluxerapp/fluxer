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

import {Resolver} from 'node:dns/promises';
import {Config} from '@fluxer/api/src/Config';
import type {IEmailDnsValidationService} from '@fluxer/api/src/infrastructure/IEmailDnsValidationService';
import {Logger} from '@fluxer/api/src/Logger';
import {ms} from 'itty-time';

interface DomainValidationCacheEntry {
	valid: boolean;
	expiresAtMs: number;
}

interface IDnsResolver {
	resolveMx(domain: string): Promise<Array<{exchange: string; priority: number}>>;
	resolve4(domain: string): Promise<Array<string>>;
	resolve6(domain: string): Promise<Array<string>>;
}

interface EmailDnsValidationServiceOptions {
	resolver?: IDnsResolver;
	enforceInTestMode?: boolean;
	positiveTtlMs?: number;
	negativeTtlMs?: number;
}

type DnsResolutionResult = 'valid' | 'invalid' | 'fallback' | 'transient_error';

const DOMAIN_NOT_FOUND_CODES = new Set(['ENOTFOUND', 'ENONAME', 'EAI_NONAME', 'NXDOMAIN']);
const DOMAIN_NO_RECORD_CODES = new Set(['ENODATA', 'ENOENT', 'NODATA']);

export class EmailDnsValidationService implements IEmailDnsValidationService {
	private readonly resolver: IDnsResolver;
	private readonly enforceInTestMode: boolean;
	private readonly positiveTtlMs: number;
	private readonly negativeTtlMs: number;
	private readonly domainCache = new Map<string, DomainValidationCacheEntry>();

	constructor(options: EmailDnsValidationServiceOptions = {}) {
		this.resolver = options.resolver ?? new Resolver();
		this.enforceInTestMode = options.enforceInTestMode ?? false;
		this.positiveTtlMs = options.positiveTtlMs ?? ms('30 minutes');
		this.negativeTtlMs = options.negativeTtlMs ?? ms('5 minutes');
	}

	async hasValidDnsRecords(email: string): Promise<boolean> {
		if (!this.enforceInTestMode && Config.dev.testModeEnabled) {
			return true;
		}

		const domain = this.extractDomain(email);
		if (!domain) {
			return false;
		}

		const cached = this.getCachedDomainResult(domain);
		if (cached !== null) {
			return cached;
		}

		const isValid = await this.resolveDomain(domain);
		this.setCachedDomainResult(domain, isValid);
		return isValid;
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

		return cached.valid;
	}

	private setCachedDomainResult(domain: string, isValid: boolean): void {
		const ttlMs = isValid ? this.positiveTtlMs : this.negativeTtlMs;
		this.domainCache.set(domain, {
			valid: isValid,
			expiresAtMs: Date.now() + ttlMs,
		});
	}

	private async resolveDomain(domain: string): Promise<boolean> {
		const mxResult = await this.resolveMx(domain);
		if (mxResult === 'valid') {
			return true;
		}
		if (mxResult === 'invalid') {
			return false;
		}
		if (mxResult === 'transient_error') {
			return true;
		}

		const addressResult = await this.resolveAddressRecords(domain);
		if (addressResult === 'valid') {
			return true;
		}
		if (addressResult === 'invalid') {
			return false;
		}
		return true;
	}

	private async resolveMx(domain: string): Promise<DnsResolutionResult> {
		try {
			const records = await this.resolver.resolveMx(domain);
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
			this.resolver.resolve4(domain),
			this.resolver.resolve6(domain),
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
		const code = (error as {code?: unknown}).code;
		return typeof code === 'string' ? code : null;
	}
}
