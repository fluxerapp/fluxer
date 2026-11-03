// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {fetchUserMe} from '@app/api/user/tests/UserTestUtils';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {DEFAULT_STOCK_LIMITS} from '@fluxer/limits/src/LimitDefaults';
import {expandWireFormat} from '@fluxer/limits/src/LimitDiffer';
import {resolveLimits} from '@fluxer/limits/src/LimitResolver';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import type {WellKnownFluxerResponse} from '@fluxer/schema/src/domains/instance/InstanceSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

describe('limit rules and traits on a self-hosted instance', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;
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
		await getInstanceConfigRepository().setAppPublicConfig({setup: {configured: true}});
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_LOOKUP,
			AdminACLs.USER_UPDATE_TRAITS,
			AdminACLs.INSTANCE_CONFIG_UPDATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	});

	async function setPremiumMode(premiumMode: 'mirror' | 'everyone'): Promise<void> {
		await createBuilder(harness, admin.token)
			.patch('/admin/instance/config')
			.body({policy: {premium_mode: premiumMode}})
			.execute();
	}

	async function getLimitConfig(): Promise<LimitConfigResponse> {
		return await createBuilder<LimitConfigResponse>(harness, admin.token).get('/admin/limit-config').execute();
	}

	async function saveConfig(transform: (config: LimitConfigResponse['limit_config']) => object): Promise<void> {
		const {limit_config: config} = await getLimitConfig();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: transform(config)})
			.execute();
	}

	async function setTraits(account: TestAccount, traits: Array<string>): Promise<void> {
		await createBuilder(harness, admin.token).put(`/admin/users/${account.userId}/traits`).body({traits}).execute();
	}

	async function changeTag(account: TestAccount, discriminator: string): Promise<number> {
		const {response} = await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({discriminator, password: account.password})
			.executeRaw();
		return response.status;
	}

	async function clientLimits(account: TestAccount) {
		const {json: me} = await fetchUserMe(harness, account.token);
		const discovery = await createBuilderWithoutAuth<WellKnownFluxerResponse>(harness)
			.get('/.well-known/fluxer')
			.execute();
		return {
			traits: me.traits,
			limits: resolveLimits(expandWireFormat(discovery.limits), {
				traits: new Set(me.traits),
				guildFeatures: new Set(),
			}).limits,
		};
	}

	it('gives the premium rule to an account that an admin gave the premium trait', async () => {
		await setPremiumMode('mirror');
		const member = await createTestAccount(harness);
		expect(await changeTag(member, '4242')).toBe(400);
		await setTraits(member, ['premium']);
		expect(await changeTag(member, '4242')).toBe(200);
		const {traits, limits} = await clientLimits(member);
		expect(traits).toContain('premium');
		expect(limits.feature_custom_discriminator).toBe(1);
		expect(limits.max_guilds).toBe(DEFAULT_STOCK_LIMITS.max_guilds);
	});

	it('matches trait names without regard to case', async () => {
		await setPremiumMode('mirror');
		await saveConfig((config) => ({
			traitDefinitions: [...config.traitDefinitions, 'Supporter'],
			rules: [
				...config.rules.map(({modifiedFields: _modifiedFields, ...rule}) => rule),
				{id: 'supporter', filters: {traits: ['Supporter']}, limits: {feature_custom_discriminator: 1}},
			],
		}));
		const member = await createTestAccount(harness);
		await setTraits(member, ['SUPPORTER']);
		expect(await changeTag(member, '4243')).toBe(200);
		const {traits, limits} = await clientLimits(member);
		expect(traits).toEqual(['supporter']);
		expect(limits.feature_custom_discriminator).toBe(1);
	});

	it('keeps an edit to the premium rule after saving', async () => {
		await setPremiumMode('mirror');
		await saveConfig((config) => ({
			traitDefinitions: config.traitDefinitions,
			rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) =>
				rule.id === 'premium' ? {...rule, limits: {...rule.limits, max_attachment_file_size: 123_456_789}} : rule,
			),
		}));
		const premium = (await getLimitConfig()).limit_config.rules.find((rule) => rule.id === 'premium');
		expect(premium?.limits.max_attachment_file_size).toBe(123_456_789);
	});

	it('applies the default rule to every account when everyone gets premium', async () => {
		await setPremiumMode('everyone');
		const member = await createTestAccount(harness);
		const {limits} = await clientLimits(member);
		expect(limits.feature_animated_banner).toBe(1);
		expect(await changeTag(member, '4244')).toBe(200);
	});
});
