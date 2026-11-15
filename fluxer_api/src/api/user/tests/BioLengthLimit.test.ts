// SPDX-License-Identifier: AGPL-3.0-or-later

import type {TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createTestAccount, setUserACLs} from '@app/api/auth/tests/AuthTestUtils';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface LimitConfigReadResponse {
	limit_config: LimitConfigSnapshot;
}

describe('bio length limit', () => {
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

	const setBio = (account: TestAccount, length: number) =>
		createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({bio: 'a'.repeat(length)});

	const putMaxBioLength = async (maxBioLength: number, expectedStatus: number): Promise<void> => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
		const current = await createBuilder<LimitConfigReadResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.expect(HTTP_STATUS.OK)
			.execute();
		const rules = current.limit_config.rules.map((rule) =>
			rule.id === 'default' ? {...rule, limits: {...rule.limits, max_bio_length: maxBioLength}} : rule,
		);
		expect(rules.some((rule) => rule.id === 'default')).toBe(true);
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({limit_config: {...current.limit_config, rules}})
			.expect(expectedStatus)
			.execute();
	};

	it('keeps the stock limit at 320 characters', async () => {
		const account = await createTestAccount(harness);
		await setBio(account, 320).expect(HTTP_STATUS.OK).execute();
		await setBio(account, 321).expect(HTTP_STATUS.BAD_REQUEST).execute();
	});

	it('accepts bios up to a limit an instance raised to 1000', async () => {
		await putMaxBioLength(1001, HTTP_STATUS.BAD_REQUEST);
		await putMaxBioLength(1000, HTTP_STATUS.OK);
		const account = await createTestAccount(harness);
		await setBio(account, 1000).expect(HTTP_STATUS.OK).execute();
		await setBio(account, 1001).expect(HTTP_STATUS.BAD_REQUEST).execute();
	});
});
