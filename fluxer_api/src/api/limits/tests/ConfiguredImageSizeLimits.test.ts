// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, setUserACLs, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {VALID_PNG_BASE64} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {AdminACLs} from '@fluxer/constants/src/AdminACLs';
import type {LimitKey} from '@fluxer/constants/src/LimitConfigMetadata';
import {AVATAR_MAX_SIZE} from '@fluxer/constants/src/LimitConstants';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import type {LimitConfigSnapshot} from '@fluxer/limits/src/LimitTypes';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const RAISED_AVATAR_SIZE = AVATAR_MAX_SIZE + 2 * 1024 * 1024;

interface LimitConfigReadResponse {
	limit_config: LimitConfigSnapshot;
}

interface ValidationErrorResponse {
	errors?: Array<{path?: string; code?: string; message?: string}>;
}

function paddedImage(mime: string, base64: string, byteLength: number): string {
	const image = Buffer.from(base64, 'base64');
	const padded = Buffer.concat([image, Buffer.alloc(byteLength - image.length)]);
	return `data:${mime};base64,${padded.toString('base64')}`;
}

describe('configured image size limits', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	const raiseLimits = async (limits: Partial<Record<LimitKey, number>>): Promise<void> => {
		const admin = await setUserACLs(harness, await createTestAccount(harness), [
			AdminACLs.AUTHENTICATE,
			AdminACLs.INSTANCE_LIMIT_CONFIG_VIEW,
			AdminACLs.INSTANCE_LIMIT_CONFIG_UPDATE,
		]);
		const current = await createBuilder<LimitConfigReadResponse>(harness, admin.token)
			.get('/admin/limit-config')
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, admin.token)
			.put('/admin/limit-config')
			.body({
				limit_config: {
					traitDefinitions: current.limit_config.traitDefinitions,
					rules: current.limit_config.rules.map((rule) => ({
						id: rule.id,
						filters: rule.filters,
						limits: rule.id === 'very_large_guild' ? rule.limits : {...rule.limits, ...limits},
					})),
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
	};

	const createOwner = async (): Promise<TestAccount> => {
		const owner = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		return owner;
	};

	it('accepts an avatar above the stock size once avatar_max_size is raised', async () => {
		await raiseLimits({avatar_max_size: RAISED_AVATAR_SIZE});
		const account = await createOwner();

		const json = await createBuilder<{avatar: string | null}>(harness, account.token)
			.patch('/users/@me')
			.body({avatar: paddedImage('image/png', VALID_PNG_BASE64, AVATAR_MAX_SIZE + 1024 * 1024)})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(json.avatar).toBeTruthy();
	});

	it('rejects an avatar over a raised limit with the configured size', async () => {
		await raiseLimits({avatar_max_size: RAISED_AVATAR_SIZE});
		const account = await createOwner();

		const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
			.patch('/users/@me')
			.body({avatar: paddedImage('image/png', VALID_PNG_BASE64, RAISED_AVATAR_SIZE + 1)})
			.expect(HTTP_STATUS.BAD_REQUEST)
			.execute();
		expect(json.errors?.[0]).toMatchObject({
			path: 'avatar',
			code: ValidationErrorCodes.IMAGE_SIZE_EXCEEDS_LIMIT,
			message: `Image size exceeds ${RAISED_AVATAR_SIZE} bytes.`,
		});
	});
});
