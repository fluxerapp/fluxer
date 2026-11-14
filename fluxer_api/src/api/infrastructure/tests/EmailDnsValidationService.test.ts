// SPDX-License-Identifier: AGPL-3.0-or-later

import {EmailDnsValidationService} from '@app/api/infrastructure/EmailDnsValidationService';
import {ms} from 'itty-time';
import {describe, expect, it} from 'vitest';

interface MxRecord {
	exchange: string;
	priority: number;
}

function dnsError(code: string): NodeJS.ErrnoException {
	const error: NodeJS.ErrnoException = new Error(`dns lookup failed with ${code}`);
	error.code = code;
	return error;
}

class FakeDnsResolver {
	readonly lookups: Array<string> = [];
	mxRecords: Array<MxRecord> = [{exchange: 'mx.example.com', priority: 10}];
	mxErrorCode: string | null = null;
	addressErrorCode: string | null = 'ENOTFOUND';
	addresses: Array<string> = [];
	stall = false;

	async resolveMx(domain: string): Promise<Array<MxRecord>> {
		this.lookups.push(`mx:${domain}`);
		if (this.stall) {
			return new Promise<Array<MxRecord>>(() => {});
		}
		if (this.mxErrorCode) {
			throw dnsError(this.mxErrorCode);
		}
		return this.mxRecords;
	}

	async resolve4(domain: string): Promise<Array<string>> {
		this.lookups.push(`a:${domain}`);
		if (this.addressErrorCode) {
			throw dnsError(this.addressErrorCode);
		}
		return this.addresses;
	}

	async resolve6(domain: string): Promise<Array<string>> {
		this.lookups.push(`aaaa:${domain}`);
		if (this.addressErrorCode) {
			throw dnsError(this.addressErrorCode);
		}
		return this.addresses;
	}
}

describe('EmailDnsValidationService', () => {
	it('rejects a domain that does not exist', async () => {
		const resolver = new FakeDnsResolver();
		resolver.mxErrorCode = 'ENOTFOUND';
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
		});
		expect(await service.hasValidDnsRecords('probe@asdf.asdf')).toBe(false);
		expect(resolver.lookups).toEqual(['mx:asdf.asdf']);
	});

	it('accepts a domain that publishes address records but no mail records', async () => {
		const resolver = new FakeDnsResolver();
		resolver.mxErrorCode = 'ENODATA';
		resolver.addressErrorCode = null;
		resolver.addresses = ['198.51.100.10'];
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
		});
		expect(await service.hasValidDnsRecords('probe@mail-less.test')).toBe(true);
		expect(resolver.lookups).toEqual(['mx:mail-less.test', 'a:mail-less.test', 'aaaa:mail-less.test']);
	});

	it('rejects a domain that publishes neither mail nor address records', async () => {
		const resolver = new FakeDnsResolver();
		resolver.mxErrorCode = 'ENODATA';
		resolver.addressErrorCode = 'ENODATA';
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
		});
		expect(await service.hasValidDnsRecords('probe@no-records.test')).toBe(false);
		expect(resolver.lookups).toEqual(['mx:no-records.test', 'a:no-records.test', 'aaaa:no-records.test']);
	});

	it('allows the address when the resolver fails transiently', async () => {
		const resolver = new FakeDnsResolver();
		resolver.mxErrorCode = 'ESERVFAIL';
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
		});
		expect(await service.hasValidDnsRecords('probe@asdf.asdf')).toBe(true);
	});

	it('does not cache a transient failure as a verified domain', async () => {
		const resolver = new FakeDnsResolver();
		resolver.mxErrorCode = 'ESERVFAIL';
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
			positiveTtlMs: ms('30 minutes'),
			negativeTtlMs: 0,
		});
		expect(await service.hasValidDnsRecords('probe@asdf.asdf')).toBe(true);
		expect(await service.hasValidDnsRecords('probe@asdf.asdf')).toBe(true);
		expect(resolver.lookups).toEqual(['mx:asdf.asdf', 'mx:asdf.asdf']);
	});

	it('gives up on a stalled resolver instead of hanging', async () => {
		const resolver = new FakeDnsResolver();
		resolver.stall = true;
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
			isEmailEnabled: async () => true,
			lookupTimeoutMs: 10,
		});
		const startedAtMs = Date.now();
		expect(await service.hasValidDnsRecords('probe@stalled.test')).toBe(true);
		expect(Date.now() - startedAtMs).toBeLessThan(ms('5 seconds'));
		expect(resolver.lookups).toEqual(['mx:stalled.test']);
	});
});
