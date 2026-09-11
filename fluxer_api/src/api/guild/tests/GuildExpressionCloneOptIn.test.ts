// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	GuildEmojiMetadataResponse,
	GuildEmojiWithUserResponse,
	GuildStickerMetadataResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount, setUserACLs, type TestAccount} from '../../auth/tests/AuthTestUtils';
import {getPngDataUrl} from '../../emoji/tests/EmojiTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createGuild, getGuild, updateGuild} from './GuildTestUtils';

interface CloneSource {
	owner: TestAccount;
	guild: GuildResponse;
	emoji: GuildEmojiWithUserResponse;
	sticker: GuildStickerWithUserResponse;
}

async function createSource(harness: ApiTestHarness, name: string): Promise<CloneSource> {
	const owner = await createTestAccount(harness);
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

async function addDeprecatedFeatures(
	harness: ApiTestHarness,
	source: CloneSource,
	features: Array<string>,
): Promise<void> {
	const admin = await setUserACLs(harness, source.owner, ['admin:authenticate', 'guild:update:features']);
	source.owner = admin;
	await createBuilder(harness, admin.token)
		.patch(`/admin/guilds/${source.guild.id}`)
		.body({add_features: features})
		.expect(HTTP_STATUS.OK)
		.execute();
	source.guild = await getGuild(harness, admin.token, source.guild.id);
}

async function optIn(harness: ApiTestHarness, source: CloneSource, features: Array<string>): Promise<void> {
	source.guild = await updateGuild(harness, source.owner.token, source.guild.id, {
		features: [...source.guild.features, ...features],
	});
}

async function createTarget(
	harness: ApiTestHarness,
	name: string,
): Promise<{account: TestAccount; guild: GuildResponse}> {
	const account = await createTestAccount(harness);
	const guild = await createGuild(harness, account.token, name);
	return {account, guild};
}

describe('Guild expression clone opt-in', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function expectEmojiCloneRejected(source: CloneSource, label: string): Promise<void> {
		const target = await createTarget(harness, `${label} Emoji Target`);
		await createBuilder(harness, target.account.token)
			.post(`/guilds/${target.guild.id}/emojis/clone`)
			.body({source_emoji_id: source.emoji.id})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	}

	async function expectStickerCloneRejected(source: CloneSource, label: string): Promise<void> {
		const target = await createTarget(harness, `${label} Sticker Target`);
		await createBuilder(harness, target.account.token)
			.post(`/guilds/${target.guild.id}/stickers/clone`)
			.body({source_sticker_id: source.sticker.id})
			.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.MISSING_ACCESS)
			.execute();
	}

	async function expectEmojiCloneAllowed(source: CloneSource, label: string): Promise<void> {
		const target = await createTarget(harness, `${label} Emoji Target`);
		const cloned = await createBuilder<GuildEmojiWithUserResponse>(harness, target.account.token)
			.post(`/guilds/${target.guild.id}/emojis/clone`)
			.body({source_emoji_id: source.emoji.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(cloned.id).not.toBe(source.emoji.id);
		expect(cloned.name).toBe(source.emoji.name);
	}

	async function expectStickerCloneAllowed(source: CloneSource, label: string): Promise<void> {
		const target = await createTarget(harness, `${label} Sticker Target`);
		const cloned = await createBuilder<GuildStickerWithUserResponse>(harness, target.account.token)
			.post(`/guilds/${target.guild.id}/stickers/clone`)
			.body({source_sticker_id: source.sticker.id})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(cloned.id).not.toBe(source.sticker.id);
		expect(cloned.name).toBe(source.sticker.name);
	}

	test('rejects both emoji and sticker cloning when the source guild carries no clone features', async () => {
		const source = await createSource(harness, 'No Clone Features Source');
		expect(source.guild.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		expect(source.guild.features).not.toContain(GuildFeatures.CLONE_STICKER_ENABLED);
		await expectEmojiCloneRejected(source, 'No Features');
		await expectStickerCloneRejected(source, 'No Features');
	});

	test('permits emoji cloning and still rejects sticker cloning with only CLONE_EMOJI_ENABLED', async () => {
		const source = await createSource(harness, 'Emoji Opt In Source');
		await optIn(harness, source, [GuildFeatures.CLONE_EMOJI_ENABLED]);
		expect(source.guild.features).toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		await expectEmojiCloneAllowed(source, 'Emoji Opt In');
		await expectStickerCloneRejected(source, 'Emoji Opt In');
	});

	test('permits sticker cloning and still rejects emoji cloning with only CLONE_STICKER_ENABLED', async () => {
		const source = await createSource(harness, 'Sticker Opt In Source');
		await optIn(harness, source, [GuildFeatures.CLONE_STICKER_ENABLED]);
		expect(source.guild.features).toContain(GuildFeatures.CLONE_STICKER_ENABLED);
		await expectStickerCloneAllowed(source, 'Sticker Opt In');
		await expectEmojiCloneRejected(source, 'Sticker Opt In');
	});

	test('permits cloning when the deprecated disabled features sit alongside the enabled ones', async () => {
		const source = await createSource(harness, 'Deprecated Plus Enabled Source');
		await addDeprecatedFeatures(harness, source, [
			GuildFeatures.CLONE_EMOJI_DISABLED,
			GuildFeatures.CLONE_STICKER_DISABLED,
		]);
		await optIn(harness, source, [GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED]);
		expect(source.guild.features).toContain(GuildFeatures.CLONE_EMOJI_DISABLED);
		expect(source.guild.features).toContain(GuildFeatures.CLONE_STICKER_DISABLED);
		await expectEmojiCloneAllowed(source, 'Deprecated Plus Enabled');
		await expectStickerCloneAllowed(source, 'Deprecated Plus Enabled');
	});

	test('rejects cloning when the source guild carries only the deprecated disabled features', async () => {
		const source = await createSource(harness, 'Deprecated Only Source');
		await addDeprecatedFeatures(harness, source, [
			GuildFeatures.CLONE_EMOJI_DISABLED,
			GuildFeatures.CLONE_STICKER_DISABLED,
		]);
		await expectEmojiCloneRejected(source, 'Deprecated Only');
		await expectStickerCloneRejected(source, 'Deprecated Only');
	});

	test('reports allow_cloning false until the source guild opts in', async () => {
		const source = await createSource(harness, 'Metadata Opt In Source');
		const viewer = await createTestAccount(harness);
		const emojiBefore = await createBuilder<GuildEmojiMetadataResponse>(harness, viewer.token)
			.get(`/emojis/${source.emoji.id}/metadata`)
			.execute();
		const stickerBefore = await createBuilder<GuildStickerMetadataResponse>(harness, viewer.token)
			.get(`/stickers/${source.sticker.id}/metadata`)
			.execute();
		expect(emojiBefore.allow_cloning).toBe(false);
		expect(stickerBefore.allow_cloning).toBe(false);
		await optIn(harness, source, [GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED]);
		const emojiAfter = await createBuilder<GuildEmojiMetadataResponse>(harness, viewer.token)
			.get(`/emojis/${source.emoji.id}/metadata`)
			.execute();
		const stickerAfter = await createBuilder<GuildStickerMetadataResponse>(harness, viewer.token)
			.get(`/stickers/${source.sticker.id}/metadata`)
			.execute();
		expect(emojiAfter.allow_cloning).toBe(true);
		expect(stickerAfter.allow_cloning).toBe(true);
	});

	test('reports allow_cloning false for a guild carrying only the deprecated disabled features', async () => {
		const source = await createSource(harness, 'Metadata Deprecated Source');
		await addDeprecatedFeatures(harness, source, [
			GuildFeatures.CLONE_EMOJI_DISABLED,
			GuildFeatures.CLONE_STICKER_DISABLED,
		]);
		const viewer = await createTestAccount(harness);
		const emoji = await createBuilder<GuildEmojiMetadataResponse>(harness, viewer.token)
			.get(`/emojis/${source.emoji.id}/metadata`)
			.execute();
		const sticker = await createBuilder<GuildStickerMetadataResponse>(harness, viewer.token)
			.get(`/stickers/${source.sticker.id}/metadata`)
			.execute();
		expect(emoji.allow_cloning).toBe(false);
		expect(sticker.allow_cloning).toBe(false);
	});

	test('reports allow_cloning true when the deprecated disabled feature sits alongside the enabled one', async () => {
		const source = await createSource(harness, 'Metadata Mixed Source');
		await addDeprecatedFeatures(harness, source, [
			GuildFeatures.CLONE_EMOJI_DISABLED,
			GuildFeatures.CLONE_STICKER_DISABLED,
		]);
		await optIn(harness, source, [GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED]);
		const viewer = await createTestAccount(harness);
		const emoji = await createBuilder<GuildEmojiMetadataResponse>(harness, viewer.token)
			.get(`/emojis/${source.emoji.id}/metadata`)
			.execute();
		const sticker = await createBuilder<GuildStickerMetadataResponse>(harness, viewer.token)
			.get(`/stickers/${source.sticker.id}/metadata`)
			.execute();
		expect(emoji.allow_cloning).toBe(true);
		expect(sticker.allow_cloning).toBe(true);
	});

	test('stops permitting cloning once the source guild opts back out', async () => {
		const source = await createSource(harness, 'Opt Out Again Source');
		await optIn(harness, source, [GuildFeatures.CLONE_EMOJI_ENABLED, GuildFeatures.CLONE_STICKER_ENABLED]);
		await expectEmojiCloneAllowed(source, 'Opt Out Again');
		source.guild = await updateGuild(harness, source.owner.token, source.guild.id, {
			features: source.guild.features.filter(
				(feature) => feature !== GuildFeatures.CLONE_EMOJI_ENABLED && feature !== GuildFeatures.CLONE_STICKER_ENABLED,
			),
		});
		expect(source.guild.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		await expectEmojiCloneRejected(source, 'Opt Out Again');
		await expectStickerCloneRejected(source, 'Opt Out Again');
	});
});
