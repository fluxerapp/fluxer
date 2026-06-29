// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserPremiumTypes} from '@fluxer/constants/src/UserConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {getConfig} from '../../Config';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {grantPremium, updateUserProfile} from './UserTestUtils';

async function withSelfHostedInstance(callback: () => Promise<void>): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	try {
		config.instance.selfHosted = true;
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
	}
}

describe('User discriminator #0000', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('blocks #0000 for non-visionary premium users on the main instance', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		const {response, text} = await createBuilder(harness, account.token)
			.patch('/users/@me')
			.body({discriminator: '0000', password: account.password})
			.executeRaw();
		expect(response.status).toBe(HTTP_STATUS.BAD_REQUEST);
		expect(text).toContain(ValidationErrorCodes.VISIONARY_REQUIRED_FOR_DISCRIMINATOR);
	});
	test('allows #0000 on self-hosted instances', async () => {
		const account = await createTestAccount(harness);
		await grantPremium(harness, account.userId, UserPremiumTypes.SUBSCRIPTION);
		await withSelfHostedInstance(async () => {
			const updated = await updateUserProfile(harness, account.token, {
				discriminator: '0000',
				password: account.password,
			});
			expect(updated.json.discriminator).toBe('0000');
		});
	});
});
