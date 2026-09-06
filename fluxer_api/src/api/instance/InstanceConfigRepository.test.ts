// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, describe, expect, it, vi} from 'vitest';
import {setCassandraQueryExecutorForTesting} from '../database/CassandraQueryExecution';
import type {PreparedQuery} from '../database/CassandraTypes';
import {InMemoryCassandraQueryExecutor} from '../test/InMemoryCassandraQueryExecutor';
import {MockKVProvider} from '../test/mocks/MockKVProvider';
import {
	INSTANCE_CONFIG_REFRESH_CHANNEL,
	InstanceConfigRepository,
	type InstanceRegistrationConfig,
} from './InstanceConfigRepository';

class CountingInMemoryCassandraQueryExecutor extends InMemoryCassandraQueryExecutor {
	instanceConfigSelects = 0;

	override async executeQuery<T = Record<string, unknown>>(query: PreparedQuery): Promise<Array<T>> {
		if (query.kvMeta?.action === 'select' && query.kvMeta.table.name === 'instance_configuration') {
			this.instanceConfigSelects++;
		}
		return super.executeQuery<T>(query);
	}
}

describe('InstanceConfigRepository', () => {
	const repositories: Array<InstanceConfigRepository> = [];

	afterEach(() => {
		for (const repository of repositories) {
			repository.shutdown();
		}
		repositories.length = 0;
	});

	function createRepository(kvProvider: MockKVProvider): InstanceConfigRepository {
		const repository = new InstanceConfigRepository(kvProvider);
		repositories.push(repository);
		return repository;
	}

	it('serves repeated config reads from the hydrated in-memory cache', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setRegistrationConfig({mode: 'closed'});
		executor.instanceConfigSelects = 0;

		expect(await repository.getRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);
		expect(await repository.getRegistrationConfig()).toEqual({
			mode: 'closed',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);
		expect(executor.instanceConfigSelects).toBe(0);
		expect(kvProvider.getSubscription().subscribedChannels).toContain(INSTANCE_CONFIG_REFRESH_CHANNEL);
	});

	it('refreshes a hydrated cache after another repository publishes a config update', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		expect(await reader.getRegistrationConfig()).toEqual({
			mode: 'open',
			admin_registration_urls_enabled: true,
		} satisfies InstanceRegistrationConfig);

		await writer.setRegistrationConfig({mode: 'approval'});

		await vi.waitFor(async () => {
			expect(await reader.getRegistrationConfig()).toEqual({
				mode: 'approval',
				admin_registration_urls_enabled: true,
			} satisfies InstanceRegistrationConfig);
		});
	});

	it('reuses the memoized effective bluesky config until the integrations blob changes', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const first = await repository.getEffectiveBlueskyConfig();
		expect(await repository.getEffectiveBlueskyConfig()).toBe(first);

		await repository.setInstanceIntegrationsConfig({bluesky: {client_name: 'Memoized Instance'}});

		const updated = await repository.getEffectiveBlueskyConfig();
		expect(updated).not.toBe(first);
		expect(updated.client_name).toBe('Memoized Instance');
		expect(await repository.getEffectiveBlueskyConfig()).toBe(updated);
	});

	it('recomputes the effective bluesky config after another repository publishes an integrations update', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		const before = await reader.getEffectiveBlueskyConfig();
		expect(await reader.getEffectiveBlueskyConfig()).toBe(before);

		await writer.setInstanceIntegrationsConfig({bluesky: {client_name: 'Refreshed Instance'}});

		await vi.waitFor(async () => {
			expect((await reader.getEffectiveBlueskyConfig()).client_name).toBe('Refreshed Instance');
		});
	});

	it('reports the effective captcha provider as none while the selected pair is incomplete', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setInstanceIntegrationsConfig({
			captcha: {
				provider: 'turnstile',
				hcaptcha_site_key: 'hcaptcha-site-key',
				hcaptcha_secret_key: 'hcaptcha-secret-key',
			},
		});

		await expect(repository.getEffectiveCaptchaConfig()).resolves.toMatchObject({
			enabled: false,
			provider: 'none',
		});

		await repository.setInstanceIntegrationsConfig({
			captcha: {
				turnstile_site_key: 'turnstile-site-key',
				turnstile_secret_key: 'turnstile-secret-key',
			},
		});

		await expect(repository.getEffectiveCaptchaConfig()).resolves.toMatchObject({
			enabled: true,
			provider: 'turnstile',
		});
	});

	it('uses the registration URL id as the admin-visible registration code', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const created = await repository.createRegistrationUrl({
			label: 'Support invite',
			createdByUserId: '1500000000000000000',
			expiresAt: null,
			maxUses: null,
			approvalRequired: false,
		});

		expect(created.code).toBe(created.registrationUrl.id);
		expect(created.registrationUrl).not.toHaveProperty('code_hash');
		await expect(repository.resolveRegistrationUrlCode(created.registrationUrl.id)).resolves.toMatchObject({
			id: created.registrationUrl.id,
		});
	});
});
