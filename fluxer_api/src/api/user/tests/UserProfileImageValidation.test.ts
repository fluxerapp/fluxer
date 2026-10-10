// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createTestGuild,
	getGifDataUrl,
	getPngDataUrl,
	VALID_GIF_BASE64,
	VALID_PNG_BASE64,
} from '@app/api/emoji/tests/EmojiTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {grantPremium, updateAvatar, updateBanner} from '@app/api/user/tests/UserTestUtils';
import type {GuildMemberResponse} from '@fluxer/schema/src/domains/guild/GuildMemberSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const PREMIUM_TYPE_SUBSCRIPTION = 2;

interface ValidationErrorResponse {
	code: string;
	message?: string;
	errors?: Array<{
		path?: string;
		code?: string;
		message?: string;
	}>;
}

function getCorruptedImageDataUrl(): string {
	const corruptedBase64 = Buffer.from('not-an-image').toString('base64');
	return getPngDataUrl(corruptedBase64);
}

describe('User Profile Image Validation', () => {
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
	describe('Avatar validation', () => {
		it('allows uploading valid GIF avatar with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateAvatar(harness, account.token, getGifDataUrl(VALID_GIF_BASE64));
			expect(result.avatar).toBeTruthy();
		});
		it('rejects animated avatar without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({avatar: getGifDataUrl(VALID_GIF_BASE64)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('ANIMATED_AVATARS_REQUIRE_PREMIUM');
		});
		it('rejects avatar with corrupted image data', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({avatar: getCorruptedImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('INVALID_IMAGE_FORMAT');
		});
	});
	describe('Banner validation', () => {
		it('rejects static banner upload without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({banner: getPngDataUrl(VALID_PNG_BASE64)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BANNERS_REQUIRE_PREMIUM');
		});
		it('rejects animated banner upload without premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const json = await createBuilder<ValidationErrorResponse>(harness, account.token)
				.patch('/users/@me')
				.body({banner: getGifDataUrl(VALID_GIF_BASE64)})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BANNERS_REQUIRE_PREMIUM');
		});
		it('allows banner upload with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateBanner(harness, account.token, getPngDataUrl(VALID_PNG_BASE64));
			expect(result.banner).toBeTruthy();
		});
		it('allows GIF banner upload with premium', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			await grantPremium(harness, account.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const result = await updateBanner(harness, account.token, getGifDataUrl(VALID_GIF_BASE64));
			expect(result.banner).toBeTruthy();
		});
	});
	describe('Guild member avatar validation', () => {
		it('allows uploading valid PNG guild member avatar with premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await grantPremium(harness, owner.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const guild = await createTestGuild(harness, owner.token);
			const result = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getPngDataUrl(VALID_PNG_BASE64)})
				.execute();
			expect(result.avatar).toBeTruthy();
		});
		it('allows uploading valid GIF guild member avatar with premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await grantPremium(harness, owner.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const guild = await createTestGuild(harness, owner.token);
			const result = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getGifDataUrl(VALID_GIF_BASE64)})
				.execute();
			expect(result.avatar).toBeTruthy();
		});
		it('rejects guild member avatar without premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createTestGuild(harness, owner.token);
			const json = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({avatar: getPngDataUrl(VALID_PNG_BASE64)})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.avatar).toBeNull();
		});
	});
	describe('Guild member banner validation', () => {
		it('allows uploading valid PNG guild member banner with premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await grantPremium(harness, owner.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const guild = await createTestGuild(harness, owner.token);
			const result = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getPngDataUrl(VALID_PNG_BASE64)})
				.execute();
			expect(result.banner).toBeTruthy();
		});
		it('allows uploading valid GIF guild member banner with premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			await grantPremium(harness, owner.userId, PREMIUM_TYPE_SUBSCRIPTION);
			const guild = await createTestGuild(harness, owner.token);
			const result = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getGifDataUrl(VALID_GIF_BASE64)})
				.execute();
			expect(result.banner).toBeTruthy();
		});
		it('rejects guild member banner without premium', async () => {
			const owner = await createTestAccount(harness);
			await ensureSessionStarted(harness, owner.token);
			const guild = await createTestGuild(harness, owner.token);
			const json = await createBuilder<GuildMemberResponse>(harness, owner.token)
				.patch(`/guilds/${guild.id}/members/@me`)
				.body({banner: getPngDataUrl(VALID_PNG_BASE64)})
				.expect(HTTP_STATUS.OK)
				.execute();
			expect(json.banner).toBeNull();
		});
	});
});
