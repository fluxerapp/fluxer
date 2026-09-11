// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
	type VoiceNoiseSuppressionConfig,
} from '@fluxer/schema/src/domains/admin/VoiceNoiseSuppressionSchemas';
import {
	DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
	type ExperimentDeliveryConfig,
} from '@fluxer/schema/src/domains/experiment/ExperimentSchemas';
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

const VOICE_NOISE_SUPPRESSION_CONFIG_KEY = 'voice_noise_suppression_config';
const EXPERIMENT_DELIVERY_CONFIG_KEY = 'experiment_delivery_config';

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

	it('returns the default voice noise suppression config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'out-of-range values', stored: '{"rollout_basis_points":99999}'},
		{name: 'an unknown backend', stored: '{"default_backend":"magic"}'},
	])('falls back to the default voice noise suppression config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(VOICE_NOISE_SUPPRESSION_CONFIG_KEY, stored);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);
	});

	it('round-trips a stored voice noise suppression config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: VoiceNoiseSuppressionConfig = {
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 3,
			default_backend: 'rnnoise',
			enabled_backends: ['none', 'standard', 'rnnoise'],
			allow_user_override: false,
			rollout_basis_points: 2500,
			rollout_salt: 'voice-ns-v2',
			included_user_ids: ['1400000000000000001'],
			excluded_user_ids: ['1400000000000000002'],
			guild_overrides: [{guild_id: '2400000000000000001', backend: 'rnnoise'}],
			stereo_enabled: true,
			suppression_strength: 55,
		};
		await repository.setVoiceNoiseSuppressionConfig(config);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual(config);
	});

	it('fills newly added voice noise suppression fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(
			VOICE_NOISE_SUPPRESSION_CONFIG_KEY,
			JSON.stringify({enabled: true, config_version: 2, rollout_basis_points: 1000}),
		);

		await expect(repository.getVoiceNoiseSuppressionConfig()).resolves.toEqual({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 2,
			rollout_basis_points: 1000,
		});
	});

	it('returns the default experiment delivery config when the key is absent', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(DEFAULT_EXPERIMENT_DELIVERY_CONFIG);
	});

	it.each([
		{name: 'unparseable text', stored: 'not-json'},
		{name: 'a json array', stored: '[]'},
		{name: 'an out-of-range poll interval', stored: '{"poll_interval_seconds":1}'},
		{name: 'an out-of-range jitter', stored: '{"poll_jitter_percent":99}'},
		{name: 'a non-numeric poll interval', stored: '{"poll_interval_seconds":"often"}'},
	])('falls back to the default experiment delivery config for $name', async ({stored}) => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(EXPERIMENT_DELIVERY_CONFIG_KEY, stored);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(DEFAULT_EXPERIMENT_DELIVERY_CONFIG);
	});

	it('round-trips a stored experiment delivery config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		const config: ExperimentDeliveryConfig = {poll_interval_seconds: 900, poll_jitter_percent: 0};
		await repository.setExperimentDeliveryConfig(config);

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual(config);
	});

	it('fills missing experiment delivery fields from the schema defaults', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const repository = createRepository(kvProvider);

		await repository.setConfig(EXPERIMENT_DELIVERY_CONFIG_KEY, JSON.stringify({poll_interval_seconds: 3600}));

		await expect(repository.getExperimentDeliveryConfig()).resolves.toEqual({
			...DEFAULT_EXPERIMENT_DELIVERY_CONFIG,
			poll_interval_seconds: 3600,
		});
	});

	it('publishes a refresh so another repository observes the voice noise suppression config', async () => {
		const executor = new CountingInMemoryCassandraQueryExecutor();
		setCassandraQueryExecutorForTesting(executor);
		const kvProvider = new MockKVProvider();
		const reader = createRepository(kvProvider);
		const writer = createRepository(kvProvider);

		await expect(reader.getVoiceNoiseSuppressionConfig()).resolves.toEqual(DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG);

		await writer.setVoiceNoiseSuppressionConfig({
			...DEFAULT_VOICE_NOISE_SUPPRESSION_CONFIG,
			enabled: true,
			config_version: 1,
		});

		await vi.waitFor(async () => {
			expect(await reader.getVoiceNoiseSuppressionConfig()).toMatchObject({enabled: true, config_version: 1});
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
