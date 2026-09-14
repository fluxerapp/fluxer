// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	GuildEmojiMetadataResponse,
	GuildEmojiWithUserResponse,
	GuildStickerMetadataResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const SOURCE_GUILD_KEYS = ['features', 'icon', 'id', 'name'];

interface MetadataSource {
	owner: TestAccount;
	guild: GuildResponse;
	emoji: GuildEmojiWithUserResponse;
	sticker: GuildStickerWithUserResponse;
}

async function createSource(harness: ApiTestHarness, name: string): Promise<MetadataSource> {
	const owner = await createTestAccount(harness);
	await ensureSessionStarted(harness, owner.token);
	const guild = await createGuild(harness, owner.token, name);
	const emoji = await createBuilder<GuildEmojiWithUserResponse>(harness, owner.token)
		.post(`/guilds/${guild.id}/emojis`)
		.body({name: 'source_emoji', image: getPngDataUrl()})
		.execute();
	const sticker = await createBuilder<GuildStickerWithUserResponse>(harness, owner.token)
		.post(`/guilds/${guild.id}/stickers`)
		.body({name: 'source_sticker', description: 'source sticker', tags: [], image: getPngDataUrl()})
		.execute();
	return {owner, guild, emoji, sticker};
}

async function grantGuildFeatures(harness: ApiTestHarness, guildId: string, features: Array<string>): Promise<void> {
	await createBuilder(harness, '').post(`/test/guilds/${guildId}/features`).body({add_features: features}).execute();
}

async function getEmojiMetadata(
	harness: ApiTestHarness,
	token: string,
	emojiId: string,
): Promise<GuildEmojiMetadataResponse> {
	return createBuilder<GuildEmojiMetadataResponse>(harness, token)
		.get(`/emojis/${emojiId}/metadata`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function getStickerMetadata(
	harness: ApiTestHarness,
	token: string,
	stickerId: string,
): Promise<GuildStickerMetadataResponse> {
	return createBuilder<GuildStickerMetadataResponse>(harness, token)
		.get(`/stickers/${stickerId}/metadata`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('Guild expression metadata source community', () => {
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

	test('returns the source community presentation for both kinds to a member and a non-member alike', async () => {
		const source = await createSource(harness, 'Metadata Presentation Source');
		const withIcon = await updateGuild(harness, source.owner.token, source.guild.id, {icon: getPngDataUrl()});
		expect(withIcon.icon).toBeTruthy();
		await grantGuildFeatures(harness, source.guild.id, [GuildFeatures.DISCOVERABLE, GuildFeatures.VERIFIED]);
		const outsider = await createTestAccount(harness);
		const responses = [
			await getEmojiMetadata(harness, source.owner.token, source.emoji.id),
			await getEmojiMetadata(harness, outsider.token, source.emoji.id),
			await getStickerMetadata(harness, source.owner.token, source.sticker.id),
			await getStickerMetadata(harness, outsider.token, source.sticker.id),
		];
		for (const response of responses) {
			expect(response.guild_id).toBe(source.guild.id);
			expect(response.guild.id).toBe(source.guild.id);
			expect(response.guild.name).toBe('Metadata Presentation Source');
			expect(response.guild.icon).toBe(withIcon.icon);
			expect(response.guild.features).toContain(GuildFeatures.DISCOVERABLE);
			expect(response.guild.features).toContain(GuildFeatures.VERIFIED);
			expect(response.allow_cloning).toBe(false);
		}
		const [memberEmoji, outsiderEmoji, memberSticker, outsiderSticker] = responses;
		expect(outsiderEmoji.guild).toEqual(memberEmoji.guild);
		expect(outsiderSticker.guild).toEqual(memberSticker.guild);
		expect(memberSticker.guild).toEqual(memberEmoji.guild);
	});

	test('reports no badge features and no icon for a community that has neither', async () => {
		const source = await createSource(harness, 'Metadata Plain Source');
		const outsider = await createTestAccount(harness);
		const emoji = await getEmojiMetadata(harness, outsider.token, source.emoji.id);
		const sticker = await getStickerMetadata(harness, outsider.token, source.sticker.id);
		for (const response of [emoji, sticker]) {
			expect(response.guild.icon).toBeNull();
			expect(response.guild.features).toEqual([]);
		}
	});

	test('exposes only id, name, icon and badge features to a non-member for both kinds', async () => {
		const source = await createSource(harness, 'Metadata Envelope Source');
		await updateGuild(harness, source.owner.token, source.guild.id, {verification_level: 1});
		await grantGuildFeatures(harness, source.guild.id, [
			GuildFeatures.DISCOVERABLE,
			GuildFeatures.VERIFIED,
			GuildFeatures.INVITES_DISABLED,
			GuildFeatures.CLONE_EMOJI_ENABLED,
			GuildFeatures.CLONE_STICKER_ENABLED,
			GuildFeatures.BANNER,
			GuildFeatures.INVITE_SPLASH,
		]);
		const outsider = await createTestAccount(harness);
		const emoji = await getEmojiMetadata(harness, outsider.token, source.emoji.id);
		const sticker = await getStickerMetadata(harness, outsider.token, source.sticker.id);
		for (const response of [emoji, sticker]) {
			expect(response.allow_cloning).toBe(true);
			expect(Object.keys(response.guild).sort()).toEqual(SOURCE_GUILD_KEYS);
			const leaked = response.guild as Record<string, unknown>;
			for (const key of [
				'banner',
				'banner_width',
				'banner_height',
				'splash',
				'splash_width',
				'splash_height',
				'embed_splash',
				'embed_splash_width',
				'embed_splash_height',
				'splash_card_alignment',
			]) {
				expect(leaked).not.toHaveProperty(key);
			}
			expect([...response.guild.features].sort()).toEqual([GuildFeatures.DISCOVERABLE, GuildFeatures.VERIFIED]);
			expect(response.guild.features).not.toContain(GuildFeatures.INVITES_DISABLED);
			expect(response.guild.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
			expect(response.guild.features).not.toContain(GuildFeatures.CLONE_STICKER_ENABLED);
			expect(response.guild.features).not.toContain(GuildFeatures.BANNER);
		}
		expect(sticker.guild).toEqual(emoji.guild);
		expect(Object.keys(emoji).sort()).toEqual(['allow_cloning', 'animated', 'guild', 'guild_id', 'id', 'name']);
		expect(Object.keys(sticker).sort()).toEqual(['allow_cloning', 'animated', 'guild', 'guild_id', 'id', 'name']);
	});
});
