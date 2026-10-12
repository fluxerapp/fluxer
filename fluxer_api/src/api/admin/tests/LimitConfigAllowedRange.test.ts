// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import {getInstanceConfigRepository, getLimitConfigService} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {
	MAX_BIO_LENGTH,
	MAX_SCREEN_SHARE_BITRATE_CEILING,
	MIN_SCREEN_SHARE_BITRATE_CEILING,
} from '@fluxer/constants/src/LimitConstants';
import type {LimitConfigGetResponse} from '@fluxer/schema/src/domains/admin/AdminSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';
import type {z} from 'zod';

type LimitConfigResponse = z.infer<typeof LimitConfigGetResponse>;

describe('limit config allowed ranges', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	async function createAdmin(): Promise<TestAccount> {
		return await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
	}

	function vipRule(maxBioLength: number) {
		return {id: 'vip', filters: {traits: ['vip']}, limits: {max_bio_length: maxBioLength}};
	}

	it('refuses a bio length above what a profile can hold', async () => {
		const admin = await createAdmin();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {traitDefinitions: ['vip'], rules: [vipRule(MAX_BIO_LENGTH + 1)]}})
			.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
			.execute();
		const saved = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {traitDefinitions: ['vip'], rules: [vipRule(MAX_BIO_LENGTH)]}})
			.execute();
		expect(saved.limit_config.rules.find((rule) => rule.id === 'vip')?.limits).toEqual({
			max_bio_length: MAX_BIO_LENGTH,
		});
		expect(saved.metadata.max_bio_length).toMatchObject({min: 0, max: MAX_BIO_LENGTH});
	});

	it('keeps a screen share bitrate ceiling inside its allowed range', async () => {
		const admin = await createAdmin();
		const rule = (bitrate: number) => ({
			id: 'vip',
			filters: {traits: ['vip']},
			limits: {max_screen_share_bitrate: bitrate},
		});
		for (const bitrate of [MIN_SCREEN_SHARE_BITRATE_CEILING - 1, MAX_SCREEN_SHARE_BITRATE_CEILING + 1]) {
			await createBuilder(harness, admin.token)
				.put('/admin/limit-config')
				.body({limit_config: {traitDefinitions: ['vip'], rules: [rule(bitrate)]}})
				.expect(HTTP_STATUS.BAD_REQUEST, APIErrorCodes.INVALID_FORM_BODY)
				.execute();
		}
		const saved = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {traitDefinitions: ['vip'], rules: [rule(25_000_000)]}})
			.execute();
		expect(saved.limit_config.rules.find((entry) => entry.id === 'vip')?.limits).toEqual({
			max_screen_share_bitrate: 25_000_000,
		});
		expect(saved.defaults.default?.max_screen_share_bitrate).toBe(9_000_000);
	});

	it('reads a stored bio length above the profile maximum as the maximum', async () => {
		const admin = await createAdmin();
		await getInstanceConfigRepository().setLimitConfig({traitDefinitions: ['vip'], rules: [vipRule(1000)]});
		await getLimitConfigService().refreshCache();
		const response = await createBuilder<LimitConfigResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		expect(response.limit_config.rules.find((rule) => rule.id === 'vip')?.limits).toEqual({
			max_bio_length: MAX_BIO_LENGTH,
		});
	});
});
