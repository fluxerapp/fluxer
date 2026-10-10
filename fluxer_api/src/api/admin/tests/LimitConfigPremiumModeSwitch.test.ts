// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getInstanceConfigRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

describe('limit config across premium model switches on a self-hosted instance', () => {
	let harness: ApiTestHarness;
	let originalSelfHosted: boolean;
	let originalPremiumMode: ReturnType<typeof getCachedInstancePremiumMode>;

	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		originalPremiumMode = getCachedInstancePremiumMode();
		getConfig().instance.selfHosted = true;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	async function createAdmin(): Promise<TestAccount> {
		return await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
	}

	async function setPremiumMode(admin: TestAccount, premiumMode: 'mirror' | 'everyone'): Promise<void> {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {premium_mode: premiumMode}})
			.execute();
	}

	async function getLimitConfig(admin: TestAccount): Promise<LimitConfigResponse> {
		return await createBuilder<LimitConfigResponse>(harness, admin.token).get('/admin/limit-config').execute();
	}

	async function saveDefaultRule(admin: TestAccount, overrides: Record<string, number>): Promise<void> {
		const {limit_config: config} = await getLimitConfig(admin);
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: config.traitDefinitions,
					rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) =>
						rule.id === 'default' ? {...rule, limits: {...rule.limits, ...overrides}} : rule,
					),
				},
			})
			.execute();
	}

	function defaultRule(response: LimitConfigResponse) {
		const rule = response.limit_config.rules.find((candidate) => candidate.id === 'default');
		expect(rule).toBeDefined();
		return rule!;
	}

	it('gives everyone premium limits after a save in mirror mode and keeps edited values', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await saveDefaultRule(admin, {max_bio_length: 300});
		await setPremiumMode(admin, 'everyone');
		const rule = defaultRule(await getLimitConfig(admin));
		expect(rule.limits).toEqual({...DEFAULT_STOCK_LIMITS, max_bio_length: 300});
		expect(rule.modifiedFields).toEqual(['max_bio_length']);
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		expect(discovery.limits.rules.find((candidate) => candidate.id === 'default')?.overrides).toMatchObject({
			feature_custom_discriminator: 1,
			max_guilds: DEFAULT_STOCK_LIMITS.max_guilds,
		});
	});

	it('restores free limits after a save in everyone mode', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'everyone');
		await saveDefaultRule(admin, {});
		await setPremiumMode(admin, 'mirror');
		const response = await getLimitConfig(admin);
		expect(defaultRule(response).limits).toEqual(DEFAULT_RESTRICTED_LIMITS);
		expect(response.limit_config.rules.find((rule) => rule.id === 'premium')?.limits).toEqual(DEFAULT_STOCK_LIMITS);
	});

	it('stores only the edited values', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await saveDefaultRule(admin, {max_bio_length: 300});
		const {config: stored} = await getInstanceConfigRepository().readLimitConfigInputs();
		expect(stored?.rules).toEqual([
			{id: 'premium', filters: {traits: ['premium']}, limits: {}},
			{id: 'default', limits: {max_bio_length: 300}},
			{id: 'very_large_guild', filters: {guildFeatures: ['VERY_LARGE_GUILD']}, limits: {}},
		]);
	});

	it('repairs free limits stored in full before an earlier switch to everyone', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'everyone');
		await getInstanceConfigRepository().setLimitConfig({
			traitDefinitions: ['premium'],
			rules: [
				{id: 'premium', filters: {traits: ['premium']}, limits: {...DEFAULT_STOCK_LIMITS}},
				{
					id: 'default',
					limits: {...DEFAULT_RESTRICTED_LIMITS, max_bio_length: 300},
					modifiedFields: ['max_bio_length'],
				},
			],
		});
		await getLimitConfigService().refreshCache();
		const rule = defaultRule(await getLimitConfig(admin));
		expect(rule.limits).toEqual({...DEFAULT_STOCK_LIMITS, max_bio_length: 300});
		expect(rule.modifiedFields).toEqual(['max_bio_length']);
		const {config: stored} = await getInstanceConfigRepository().readLimitConfigInputs();
		expect(stored?.rules.find((candidate) => candidate.id === 'default')).toEqual({
			id: 'default',
			limits: {max_bio_length: 300},
		});
	});

	it('repairs premium limits stored in full before an earlier switch to mirror', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await getInstanceConfigRepository().setLimitConfig({
			traitDefinitions: [],
			rules: [{id: 'default', limits: {...DEFAULT_STOCK_LIMITS}}],
		});
		await getLimitConfigService().refreshCache();
		const response = await getLimitConfig(admin);
		expect(defaultRule(response).limits).toEqual(DEFAULT_RESTRICTED_LIMITS);
		expect(defaultRule(response).modifiedFields).toBeUndefined();
	});
});
