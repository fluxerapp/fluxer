// SPDX-License-Identifier: AGPL-3.0-or-later

import {SearchProvider, type SearchServices} from '@app/api/infrastructure/SearchProvider';
import {InMemorySearchProvider} from '@app/api/test/search/InMemorySearchProvider';
import {describe, expect, it, vi} from 'vitest';

function createServices(): SearchServices {
	const provider = new InMemorySearchProvider();
	const lifecycle = () => ({initialize: vi.fn(async () => {}), shutdown: vi.fn(async () => {})});
	return {
		message: Object.assign(provider.getMessageSearchService(), lifecycle()),
		guild: Object.assign(provider.getGuildSearchService(), lifecycle()),
		user: Object.assign(provider.getUserSearchService(), lifecycle()),
		report: Object.assign(provider.getReportSearchService(), lifecycle()),
		auditLog: Object.assign(provider.getAuditLogSearchService(), lifecycle()),
		guildMember: Object.assign(provider.getGuildMemberSearchService(), lifecycle()),
		thread: Object.assign(provider.getThreadSearchService(), {shutdown: vi.fn(async () => {})}),
	};
}

describe('SearchProvider lifecycle', () => {
	it('keeps all services available for cleanup after a partial initialization failure', async () => {
		const services = createServices();
		const failure = new Error('index creation failed');
		vi.mocked(services.guild.initialize).mockRejectedValueOnce(failure);
		const initialized = vi.fn();
		const provider = new SearchProvider(async () => services, initialized);

		await expect(provider.initialize()).rejects.toBe(failure);
		expect(provider.getMessageSearchService()).toBe(services.message);
		expect(provider.getThreadSearchService()).toBe(services.thread);
		expect(initialized).not.toHaveBeenCalled();
		await provider.shutdown();

		for (const service of Object.values(services)) expect(service.shutdown).toHaveBeenCalledOnce();
		expect(provider.getMessageSearchService()).toBeNull();
		expect(provider.getThreadSearchService()).toBeNull();
	});

	it('retains service references until shutdown succeeds so a failed shutdown can be retried', async () => {
		const services = createServices();
		const failure = new Error('adapter shutdown failed');
		vi.mocked(services.thread.shutdown).mockRejectedValueOnce(failure);
		const provider = new SearchProvider(async () => services);
		await provider.initialize();

		await expect(provider.shutdown()).rejects.toBe(failure);
		expect(provider.getThreadSearchService()).toBe(services.thread);
		await provider.shutdown();
		expect(services.thread.shutdown).toHaveBeenCalledTimes(2);
		expect(provider.getThreadSearchService()).toBeNull();
	});
});
