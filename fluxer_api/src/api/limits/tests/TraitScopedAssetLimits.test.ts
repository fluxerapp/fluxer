// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {VALID_PNG_BASE64} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const RAISED_AVATAR_SIZE = AVATAR_MAX_SIZE + 2 * 1024 * 1024;

function paddedImage(byteLength: number): string {
	const image = Buffer.from(VALID_PNG_BASE64, 'base64');
	return `data:image/png;base64,${Buffer.concat([image, Buffer.alloc(byteLength - image.length)]).toString('base64')}`;
}

describe('trait-scoped asset size limits', () => {
	let harness: ApiTestHarness;
	let admin: TestAccount;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.USER_UPDATE_TRAITS,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
		const current = await createBuilder<{limit_config: LimitConfigSnapshot}>(harness, admin.token)
			.get('/admin/limit-config')
			.execute();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: [...current.limit_config.traitDefinitions, 'big'],
					rules: [
						...current.limit_config.rules.map(({id, filters, limits}) => ({id, filters, limits})),
						{id: 'big', filters: {traits: ['big']}, limits: {avatar_max_size: RAISED_AVATAR_SIZE}},
					],
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	function uploadLargeAvatar(account: TestAccount) {
		return createBuilder<{avatar?: string | null; errors?: Array<{code?: string}>}>(harness, account.token)
			.patch('/users/@me')
			.body({avatar: paddedImage(AVATAR_MAX_SIZE + 1024 * 1024)});
	}

	it('accepts a larger avatar from an account with the trait', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		await createBuilder(harness, admin.token)
			.put(`/admin/users/${account.userId}/traits`)
			.body({traits: ['big']})
			.expect(HTTP_STATUS.OK)
			.execute();
		const json = await uploadLargeAvatar(account).expect(HTTP_STATUS.OK).execute();
		expect(json.avatar).toBeTruthy();
	});

	it('keeps the stock size for an account without the trait', async () => {
		const account = await createTestAccount(harness);
		await ensureSessionStarted(harness, account.token);
		const json = await uploadLargeAvatar(account).expect(HTTP_STATUS.BAD_REQUEST).execute();
		expect(json.errors?.[0]?.code).toBe(ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT);
	});
});
