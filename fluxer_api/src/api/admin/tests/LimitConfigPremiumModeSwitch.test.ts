// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getInstanceConfigRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe, grantPremium} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {LIMIT_KEYS} from '@fluxer/constants/src/LimitConfigMetadata';
import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {DEFAULT_RESTRICTED_LIMITS, DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import {resolveLimits} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

const MODE_KEYS = LIMIT_KEYS.filter((key) => DEFAULT_STOCK_LIMITS[key] !== DEFAULT_RESTRICTED_LIMITS[key]);

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

	async function saveRule(admin: TestAccount, ruleId: string, overrides: Record<string, number>): Promise<void> {
		const {limit_config: config} = await getLimitConfig(admin);
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: config.traitDefinitions,
					rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) =>
						rule.id === ruleId ? {...rule, limits: {...rule.limits, ...overrides}} : rule,
					),
				},
			})
			.execute();
	}

	function saveDefaultRule(admin: TestAccount, overrides: Record<string, number>): Promise<void> {
		return saveRule(admin, 'default', overrides);
	}

	function findRule(response: LimitConfigResponse, ruleId: string) {
		const rule = response.limit_config.rules.find((candidate) => candidate.id === ruleId);
		expect(rule).toBeDefined();
		return rule!;
	}

	function defaultRule(response: LimitConfigResponse) {
		return findRule(response, 'default');
	}

	async function storeLegacyLimitConfig(config: LimitConfigSnapshot): Promise<void> {
		await getInstanceConfigRepository().setLimitConfig(config);
		await getLimitConfigService().refreshCache();
	}

	async function changeTag(account: TestAccount, discriminator: string): Promise<number> {
		const {response} = await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({discriminator, password: account.password})
			.executeRaw();
		return response.status;
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
		expect(stored?.overridesOnly).toBe(true);
		expect(stored?.rules).toEqual([
			{id: 'premium', filters: {traits: ['premium']}, limits: {}},
			{id: 'default', limits: {max_bio_length: 300}},
			{id: 'very_large_guild', filters: {guildFeatures: ['VERY_LARGE_GUILD']}, limits: {}},
		]);
	});

	it('repairs free limits stored in full before an earlier switch to everyone', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'everyone');
		await storeLegacyLimitConfig({
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
		const rule = defaultRule(await getLimitConfig(admin));
		expect(rule.limits).toEqual({...DEFAULT_STOCK_LIMITS, max_bio_length: 300});
		expect(rule.modifiedFields).toEqual(['max_bio_length']);
	});

	it('repairs premium limits stored in full before an earlier switch to mirror', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await storeLegacyLimitConfig({
			traitDefinitions: [],
			rules: [{id: 'default', limits: {...DEFAULT_STOCK_LIMITS}}],
		});
		const response = await getLimitConfig(admin);
		expect(defaultRule(response).limits).toEqual(DEFAULT_RESTRICTED_LIMITS);
		expect(defaultRule(response).modifiedFields).toBeUndefined();
	});

	it('repairs free limits saved again after an earlier switch to everyone', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'everyone');
		await storeLegacyLimitConfig({
			traitDefinitions: [],
			rules: [{id: 'default', limits: {...DEFAULT_RESTRICTED_LIMITS}, modifiedFields: MODE_KEYS}],
		});
		const rule = defaultRule(await getLimitConfig(admin));
		expect(rule.limits).toEqual(DEFAULT_STOCK_LIMITS);
		expect(rule.modifiedFields).toBeUndefined();
		const member = await createTestAccount(harness);
		expect(await changeTag(member, '4242')).toBe(200);
		await saveDefaultRule(admin, {max_bio_length: 300});
		const {config: stored} = await getInstanceConfigRepository().readLimitConfigInputs();
		expect(stored?.rules.find((candidate) => candidate.id === 'default')).toEqual({
			id: 'default',
			limits: {max_bio_length: 300},
		});
	});

	it('repairs premium limits saved again after an earlier switch to mirror', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await storeLegacyLimitConfig({
			traitDefinitions: [],
			rules: [{id: 'default', limits: {...DEFAULT_STOCK_LIMITS}, modifiedFields: MODE_KEYS}],
		});
		const response = await getLimitConfig(admin);
		expect(defaultRule(response).limits).toEqual(DEFAULT_RESTRICTED_LIMITS);
		expect(findRule(response, 'premium').limits).toEqual(DEFAULT_STOCK_LIMITS);
		expect(response.limit_config.traitDefinitions).toEqual(['premium']);
	});

	it('reads limits stored in full under the model that is being left', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		await storeLegacyLimitConfig({
			traitDefinitions: ['premium'],
			rules: [
				{id: 'premium', filters: {traits: ['premium']}, limits: {...DEFAULT_STOCK_LIMITS}},
				{id: 'default', limits: {...DEFAULT_RESTRICTED_LIMITS, max_guilds: 150}},
			],
		});
		await setPremiumMode(admin, 'everyone');
		expect(defaultRule(await getLimitConfig(admin)).limits).toEqual({...DEFAULT_STOCK_LIMITS, max_guilds: 150});
		const {config: stored} = await getInstanceConfigRepository().readLimitConfigInputs();
		expect(stored?.rules).toEqual([
			{id: 'premium', filters: {traits: ['premium']}, limits: {}},
			{id: 'default', limits: {max_guilds: 150}},
		]);
	});

	it('keeps edits that match the other model across a save there', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		const edits = {feature_custom_discriminator: 1, max_guilds: DEFAULT_STOCK_LIMITS.max_guilds};
		await saveDefaultRule(admin, edits);
		await setPremiumMode(admin, 'everyone');
		await saveDefaultRule(admin, {max_bio_length: 300});
		await setPremiumMode(admin, 'mirror');
		const rule = defaultRule(await getLimitConfig(admin));
		expect(rule.limits).toEqual({...DEFAULT_RESTRICTED_LIMITS, ...edits, max_bio_length: 300});
		expect(rule.modifiedFields).toEqual(['feature_custom_discriminator', 'max_bio_length', 'max_guilds']);
	});

	it('keeps premium rule edits across a save while everyone hides the rule', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'mirror');
		const edits = {max_guilds: 999, feature_animated_avatar: 0};
		await saveRule(admin, 'premium', edits);
		await setPremiumMode(admin, 'everyone');
		const hidden = await getLimitConfig(admin);
		expect(hidden.limit_config.rules.map((rule) => rule.id)).toEqual(['default', 'very_large_guild']);
		expect(hidden.limit_config.traitDefinitions).toEqual([]);
		await saveDefaultRule(admin, {max_bio_length: 300});
		await setPremiumMode(admin, 'mirror');
		const response = await getLimitConfig(admin);
		expect(findRule(response, 'premium').limits).toEqual({...DEFAULT_STOCK_LIMITS, ...edits});
		expect(response.limit_config.traitDefinitions).toEqual(['premium']);
		expect(defaultRule(response).limits).toEqual({...DEFAULT_RESTRICTED_LIMITS, max_bio_length: 300});
	});

	it('lets only premium members change their tag in mirror mode', async () => {
		const admin = await createAdmin();
		await setPremiumMode(admin, 'everyone');
		await saveDefaultRule(admin, {});
		await setPremiumMode(admin, 'mirror');
		const free = await createTestAccount(harness);
		const subscriber = await createTestAccount(harness);
		const gifted = await createTestAccount(harness);
		await grantPremium(harness, subscriber.userId, UserPremiumTypes.SUBSCRIPTION);
		await grantPremium(harness, gifted.userId, UserPremiumTypes.LIFETIME);
		expect(await changeTag(free, '4242')).toBe(400);
		expect(await changeTag(subscriber, '4242')).toBe(200);
		expect(await changeTag(gifted, '4243')).toBe(200);
		const {json: me} = await fetchUserMe(harness, subscriber.token);
		expect(me.traits).toContain('premium');
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		const {limits} = resolveLimits(expandWireFormat(discovery.limits), {
			traits: new Set(me.traits),
			guildFeatures: new Set(),
		});
		expect(limits.feature_custom_discriminator).toBe(1);
		expect(limits.max_guilds).toBe(DEFAULT_STOCK_LIMITS.max_guilds);
	});
});
