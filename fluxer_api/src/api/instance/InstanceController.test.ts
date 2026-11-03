// SPDX-License-Identifier: AGPL-3.0-or-later

import {createAuthHarness} from '@app/api/auth/tests/AuthTestUtils';
import {Config} from '@app/api/Config';
import type {InstanceConfigRepository} from '@app/api/instance/InstanceConfigRepository';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {DEFAULT_DOMAIN_MIGRATION_CONFIG} from '@fluxer/schema/src/domains/admin/DomainMigrationSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface DiscoveryResponse {
	captcha: {provider: string};
	features: {discovery_enabled?: boolean};
	domain_migration: unknown;
}

describe('InstanceController discovery', () => {
	let harness: ApiTestHarness;
	let repository: InstanceConfigRepository;
	const previousDiscoveryEnabled = Config.discovery.enabled;

	beforeAll(async () => {
		harness = await createAuthHarness();
	});
	beforeEach(async () => {
		await harness.reset();
		repository = getInstanceConfigRepository();
	});
	afterEach(() => {
		Config.discovery.enabled = previousDiscoveryEnabled;
		repository.clearCacheForTesting();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function fetchDiscovery() {
		const result = await createBuilderWithoutAuth<DiscoveryResponse>(harness).get('/.well-known/fluxer').executeRaw();
		expect(result.response.status).toBe(200);
		return result;
	}

	it('advertises altcha by default', async () => {
		expect((await fetchDiscovery()).json.captcha).toEqual({provider: 'altcha'});
	});

	it('advertises no provider once an admin turns the captcha off', async () => {
		await repository.updateCaptchaConfig({enabled: false});

		expect((await fetchDiscovery()).json.captcha).toEqual({provider: 'none'});
	});

	it('publishes whether discovery is enabled', async () => {
		Config.discovery.enabled = false;
		expect((await fetchDiscovery()).json.features.discovery_enabled).toBe(false);

		Config.discovery.enabled = true;
		expect((await fetchDiscovery()).json.features.discovery_enabled).toBe(true);
	});

	it('publishes the domain migration kill switch and anonymous rollout without the targeting lists', async () => {
		const initial = await fetchDiscovery();
		expect(initial.json.domain_migration).toEqual({
			enabled: false,
			anonymous_rollout_basis_points: 0,
			rollout_salt: 'domain-migration-v1',
			standalone_forwarding: false,
		});

		await repository.setDomainMigrationConfig({
			...DEFAULT_DOMAIN_MIGRATION_CONFIG,
			enabled: true,
			config_version: 2,
			rollout_basis_points: 100,
			anonymous_rollout_basis_points: 1500,
			included_user_ids: ['1400000000000000001'],
			standalone_forwarding: true,
		});

		const updated = await fetchDiscovery();
		expect(updated.response.headers.get('etag')).not.toBe(initial.response.headers.get('etag'));
		expect(updated.json.domain_migration).toEqual({
			enabled: true,
			anonymous_rollout_basis_points: 1500,
			rollout_salt: 'domain-migration-v1',
			standalone_forwarding: true,
		});
	});
});
