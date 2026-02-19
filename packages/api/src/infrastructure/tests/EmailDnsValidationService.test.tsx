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

import {EmailDnsValidationService} from '@fluxer/api/src/infrastructure/EmailDnsValidationService';
import {describe, expect, test, vi} from 'vitest';

type ResolveMxFn = (domain: string) => Promise<Array<{exchange: string; priority: number}>>;
type ResolveAddressFn = (domain: string) => Promise<Array<string>>;

interface MockResolver {
	resolveMx: ReturnType<typeof vi.fn<ResolveMxFn>>;
	resolve4: ReturnType<typeof vi.fn<ResolveAddressFn>>;
	resolve6: ReturnType<typeof vi.fn<ResolveAddressFn>>;
}

function createDnsError(code: string): Error & {code: string} {
	const error = new Error(`DNS lookup failed: ${code}`) as Error & {code: string};
	error.code = code;
	return error;
}

function createMockResolver(): MockResolver {
	return {
		resolveMx: vi.fn<ResolveMxFn>(),
		resolve4: vi.fn<ResolveAddressFn>(),
		resolve6: vi.fn<ResolveAddressFn>(),
	};
}

describe('EmailDnsValidationService', () => {
	test('accepts email domains that have MX records', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockResolvedValue([{exchange: 'mx.example.com', priority: 10}]);
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const result = await service.hasValidDnsRecords('person@example.com');

		expect(result).toBe(true);
		expect(resolver.resolveMx).toHaveBeenCalledWith('example.com');
		expect(resolver.resolve4).not.toHaveBeenCalled();
		expect(resolver.resolve6).not.toHaveBeenCalled();
	});

	test('falls back to A/AAAA when MX is missing', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockRejectedValue(createDnsError('ENODATA'));
		resolver.resolve4.mockResolvedValue(['192.0.2.10']);
		resolver.resolve6.mockResolvedValue([]);
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const result = await service.hasValidDnsRecords('person@example.com');

		expect(result).toBe(true);
		expect(resolver.resolveMx).toHaveBeenCalledWith('example.com');
		expect(resolver.resolve4).toHaveBeenCalledWith('example.com');
		expect(resolver.resolve6).toHaveBeenCalledWith('example.com');
	});

	test('rejects domains that have no MX and no A/AAAA records', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockRejectedValue(createDnsError('ENODATA'));
		resolver.resolve4.mockRejectedValue(createDnsError('ENODATA'));
		resolver.resolve6.mockRejectedValue(createDnsError('ENODATA'));
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const result = await service.hasValidDnsRecords('person@example.com');

		expect(result).toBe(false);
	});

	test('rejects domains that do not exist', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockRejectedValue(createDnsError('ENOTFOUND'));
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const result = await service.hasValidDnsRecords('person@missing-domain.invalid');

		expect(result).toBe(false);
		expect(resolver.resolve4).not.toHaveBeenCalled();
		expect(resolver.resolve6).not.toHaveBeenCalled();
	});

	test('fails open on transient resolver errors', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockRejectedValue(createDnsError('ETIMEOUT'));
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const result = await service.hasValidDnsRecords('person@example.com');

		expect(result).toBe(true);
	});

	test('caches results by normalised domain', async () => {
		const resolver = createMockResolver();
		resolver.resolveMx.mockResolvedValue([{exchange: 'mx.example.com', priority: 10}]);
		const service = new EmailDnsValidationService({
			resolver,
			enforceInTestMode: true,
		});

		const first = await service.hasValidDnsRecords('person@Example.com');
		const second = await service.hasValidDnsRecords('someone@example.com');

		expect(first).toBe(true);
		expect(second).toBe(true);
		expect(resolver.resolveMx).toHaveBeenCalledTimes(1);
	});
});
