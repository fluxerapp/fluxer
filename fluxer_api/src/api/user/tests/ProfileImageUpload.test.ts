// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getGifDataUrl, getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	grantPremium,
	type UserProfileUpdateResult,
	updateAvatar,
	updateBanner,
} from '@app/api/user/tests/UserTestUtils';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PREMIUM_TYPE_SUBSCRIPTION = 2;
const AVATAR_MAX_SIZE = 10 * 1024 * 1024;

interface ValidationErrorResponse {
	code: string;
	errors?: Array<{
		path?: string;
		code?: string;
	}>;
}

function getTooLargeImageDataUrl(): string {
	const largeData = 'A'.repeat(AVATAR_MAX_SIZE + 10000);
	const base64 = Buffer.from(largeData).toString('base64');
	return getPngDataUrl(base64);
}

describe('Profile Image Upload', () => {
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
	describe('User Avatar', () => {
		it('allows uploading valid PNG avatar', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const result = await updateAvatar(harness, account.token, getPngDataUrl());
			expect(result.avatar).toBeTruthy();
		});
		it('allows uploading valid GIF avatar with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateAvatar(harness, account.token, getGifDataUrl());
			expect(result.avatar).toBeTruthy();
		});
		it('rejects animated avatar without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await createBuilder<UserProfileUpdateResult>(harness, account.token)
				.patch('/users/@me')
				.body({avatar: getGifDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		it('rejects avatar that exceeds size limit', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await createBuilder<UserProfileUpdateResult>(harness, account.token)
				.patch('/users/@me')
				.body({avatar: getTooLargeImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		it('allows clearing avatar by setting to null', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await updateAvatar(harness, account.token, getPngDataUrl());
			const cleared = await updateAvatar(harness, account.token, null);
			expect(cleared.avatar).toBeNull();
		});
		it('replaces old avatar when uploading new one', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const first = await updateAvatar(harness, account.token, getPngDataUrl());
			expect(first.avatar).toBeTruthy();
			const firstHash = first.avatar;
			const second = await updateAvatar(harness, account.token, getPngDataUrl());
			expect(second.avatar).toBeTruthy();
			expect(second.avatar).toBe(firstHash);
		});
	});
	describe('User Banner', () => {
		it('rejects static banner upload without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({banner: getPngDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BANNERS_REQUIRE_PREMIUM');
		});
		it('rejects animated banner upload without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({banner: getGifDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BANNERS_REQUIRE_PREMIUM');
		});
		it('allows banner upload with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateBanner(harness, account.token, getPngDataUrl());
			expect(result.banner).toBeTruthy();
		});
		it('allows uploading GIF banner with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateBanner(harness, account.token, getGifDataUrl());
			expect(result.banner).toBeTruthy();
		});
		it('rejects banner that exceeds size limit', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			await createBuilder<UserProfileUpdateResult>(harness, account.token)
				.patch('/users/@me')
				.body({banner: getTooLargeImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
		});
		it('allows clearing banner by setting to null', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			await updateBanner(harness, account.token, getPngDataUrl());
			const cleared = await updateBanner(harness, account.token, null);
			expect(cleared.banner).toBeNull();
		});
	});
});
