// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getConfig} from '@app/api/Config';
import {getInstanceConfigRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {LimitRule} from '@fluxer/limits/src/LimitTypes';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

const REMOVED_KEY = 'max_created_packs';

describe.each([true, false])('limit config with a key from an older release (self-hosted: %s)', (selfHosted) => {
	let harness: ApiTestHarness;
	let originalSelfHosted: boolean;

	beforeAll(async () => {
		originalSelfHosted = getConfig().instance.selfHosted;
		getConfig().instance.selfHosted = selfHosted;
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		await getLimitConfigService().refreshCache();
	});

	afterAll(async () => {
		await harness.shutdown();
		getConfig().instance.selfHosted = originalSelfHosted;
	});

	async function createAdmin(): Promise<TestAccount> {
		return await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
	}

	async function storeRuleFromOlderRelease(): Promise<void> {
		const config = getLimitConfigService().getConfigSnapshot();
		const legacyRule = {
			id: 'vip',
			filters: {traits: ['vip']},
			limits: {max_bio_length: 200, [REMOVED_KEY]: 5},
		} as LimitRule;
		await getInstanceConfigRepository().setLimitConfig({
			traitDefinitions: [...config.traitDefinitions, 'vip'],
			rules: [...config.rules, legacyRule],
		});
		await getLimitConfigService().refreshCache();
	}

	it('leaves the removed key out when reading the stored config', async () => {
		const admin = await createAdmin();
		await storeRuleFromOlderRelease();
		const response = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		const vip = response.limit_config.rules.find((rule) => rule.id === 'vip');
		expect(vip?.limits).toEqual({max_bio_length: 200});
	});

	it('saves the config it read back and drops the removed key from storage', async () => {
		const admin = await createAdmin();
		await storeRuleFromOlderRelease();
		const {limit_config: config} = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		await createBuilder<LimitConfigResponse>(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: config.traitDefinitions,
					rules: config.rules.map(({modifiedFields: _modifiedFields, ...rule}) => rule),
				},
			})
			.execute();
		const {config: stored} = await getInstanceConfigRepository().readLimitConfigInputs();
		const vip = stored?.rules.find((rule) => rule.id === 'vip');
		expect(vip?.limits).toEqual({max_bio_length: 200});
	});

	it('drops a rule whose only limits were removed keys', async () => {
		const admin = await createAdmin();
		const config = getLimitConfigService().getConfigSnapshot();
		await getInstanceConfigRepository().setLimitConfig({
			traitDefinitions: [...config.traitDefinitions, 'packs'],
			rules: [...config.rules, {id: 'packs', filters: {traits: ['packs']}, limits: {[REMOVED_KEY]: 5}} as LimitRule],
		});
		await getLimitConfigService().refreshCache();
		const {limit_config: read} = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		expect(read.rules.some((rule) => rule.id === 'packs')).toBe(false);
		await createBuilder<LimitConfigResponse>(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: read.traitDefinitions,
					rules: read.rules.map(({modifiedFields: _modifiedFields, ...rule}) => rule),
				},
			})
			.execute();
	});
});
