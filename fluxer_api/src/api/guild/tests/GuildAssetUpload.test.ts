// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getGifDataUrl, getPngDataUrl, getSvgDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const AVATAR_MAX_SIZE = 10 * 1024 * 1024;

function getTooLargeImageDataUrl(): string {
	const largeData = 'A'.repeat(AVATAR_MAX_SIZE + 10000);
	const base64 = Buffer.from(largeData).toString('base64');
	return getPngDataUrl(base64);
}

async function grantGuildFeature(harness: ApiTestHarness, guildId: string, feature: string): Promise<void> {
	await createBuilder<{
		success: boolean;
	}>(harness, '')
		.post(`/test/guilds/${guildId}/features`)
		.body({add_features: [feature]})
		.execute();
}

describe('Guild Asset Upload', () => {
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
	describe('Guild Icon', () => {
		it('allows uploading valid GIF icon', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Icon GIF Test Guild');
			const updated = await updateGuild(harness, account.token, guild.id, {
				icon: getGifDataUrl(),
			});
			expect(updated.icon).toBeTruthy();
		});
		it('rejects icon that exceeds size limit', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Icon Size Limit Test');
			const json = await createBuilder<{
				errors?: Array<{
					path?: string;
					code?: string;
				}>;
			}>(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({icon: getTooLargeImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BASE64_LENGTH_INVALID');
		});
		it('allows clearing icon by setting to null', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Icon Clear Test');
			await updateGuild(harness, account.token, guild.id, {
				icon: getPngDataUrl(),
			});
			const cleared = await updateGuild(harness, account.token, guild.id, {
				icon: null,
			});
			expect(cleared.icon).toBeNull();
		});
	});
	describe('Guild Banner', () => {
		it('allows banner upload with BANNER feature', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Banner Feature Test');
			await grantGuildFeature(harness, guild.id, 'BANNER');
			const updated = await updateGuild(harness, account.token, guild.id, {
				banner: getPngDataUrl(),
			});
			expect(updated.banner).toBeTruthy();
		});
		it('does not store raster dimensions for SVG banners', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'SVG Banner Test');
			await grantGuildFeature(harness, guild.id, 'BANNER');
			const updated = await updateGuild(harness, account.token, guild.id, {
				banner: getSvgDataUrl(),
			});
			expect(updated.banner).toBeTruthy();
			expect(updated.banner_width).toBeNull();
			expect(updated.banner_height).toBeNull();
		});
		it('allows animated banner with BANNER and ANIMATED_BANNER features', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Animated Banner Test');
			await grantGuildFeature(harness, guild.id, 'BANNER');
			await grantGuildFeature(harness, guild.id, 'ANIMATED_BANNER');
			const updated = await updateGuild(harness, account.token, guild.id, {
				banner: getGifDataUrl(),
			});
			expect(updated.banner).toBeTruthy();
		});
		it('rejects banner that exceeds size limit', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Banner Size Test');
			await grantGuildFeature(harness, guild.id, 'BANNER');
			const json = await createBuilder<{
				errors?: Array<{
					path?: string;
					code?: string;
				}>;
			}>(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({banner: getTooLargeImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BASE64_LENGTH_INVALID');
		});
	});
	describe('Guild Splash', () => {
		it('allows splash upload with INVITE_SPLASH feature', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Splash Feature Test');
			await grantGuildFeature(harness, guild.id, 'INVITE_SPLASH');
			const updated = await updateGuild(harness, account.token, guild.id, {
				splash: getPngDataUrl(),
			});
			expect(updated.splash).toBeTruthy();
		});
		it('allows clearing splash by setting to null', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Splash Clear Test');
			await grantGuildFeature(harness, guild.id, 'INVITE_SPLASH');
			await updateGuild(harness, account.token, guild.id, {
				splash: getPngDataUrl(),
			});
			const cleared = await updateGuild(harness, account.token, guild.id, {
				splash: null,
			});
			expect(cleared.splash).toBeNull();
		});
		it('rejects splash that exceeds size limit', async () => {
			const account = await createTestAccount(harness);
			await ensureSessionStarted(harness, account.token);
			const guild = await createGuild(harness, account.token, 'Splash Size Test');
			await grantGuildFeature(harness, guild.id, 'INVITE_SPLASH');
			const json = await createBuilder<{
				errors?: Array<{
					path?: string;
					code?: string;
				}>;
			}>(harness, account.token)
				.patch(`/guilds/${guild.id}`)
				.body({splash: getTooLargeImageDataUrl()})
				.expect(HTTP_STATUS.BAD_REQUEST)
				.execute();
			expect(json.errors?.[0]?.code).toBe('BASE64_LENGTH_INVALID');
		});
	});
});
