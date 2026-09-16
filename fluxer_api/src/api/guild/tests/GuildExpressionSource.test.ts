// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {acceptInvite, createChannelInvite, createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	GuildEmojiMetadataResponse,
	GuildEmojiWithUserResponse,
	GuildExpressionSourceGuildResponse,
	GuildStickerMetadataResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const SOURCE_GUILD_KEYS = ['features', 'icon', 'id', 'name'];
const METADATA_KEYS = ['allow_cloning', 'animated', 'guild_id', 'id', 'name'];

type ExpressionPath = 'emojis' | 'stickers';

interface ExpressionSource {
	owner: TestAccount;
	guild: GuildResponse;
	emoji: GuildEmojiWithUserResponse;
	sticker: GuildStickerWithUserResponse;
}

async function createSource(harness: ApiTestHarness, name: string): Promise<ExpressionSource> {
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

async function joinSource(harness: ApiTestHarness, source: ExpressionSource): Promise<TestAccount> {
	const member = await createTestAccount(harness);
	await ensureSessionStarted(harness, member.token);
	const invite = await createChannelInvite(harness, source.owner.token, source.guild.system_channel_id!);
	await acceptInvite(harness, member.token, invite.code);
	return member;
}

function expressionIds(source: ExpressionSource): Array<[ExpressionPath, string]> {
	return [
		['emojis', source.emoji.id],
		['stickers', source.sticker.id],
	];
}

async function getSource(
	harness: ApiTestHarness,
	token: string,
	path: ExpressionPath,
	id: string,
): Promise<GuildExpressionSourceGuildResponse> {
	return createBuilder<GuildExpressionSourceGuildResponse>(harness, token)
		.get(`/${path}/${id}/source`)
		.expect(HTTP_STATUS.OK)
		.execute();
}

async function expectHiddenSource(harness: ApiTestHarness, token: string, path: ExpressionPath, id: string) {
	await createBuilder(harness, token)
		.get(`/${path}/${id}/source`)
		.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_GUILD)
		.execute();
}

describe('Guild expression source community', () => {
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

	test('reveals a discoverable community to a non-member for both kinds', async () => {
		const source = await createSource(harness, 'Discoverable Source');
		const withIcon = await updateGuild(harness, source.owner.token, source.guild.id, {icon: getPngDataUrl()});
		await grantGuildFeatures(harness, source.guild.id, [GuildFeatures.DISCOVERABLE, GuildFeatures.VERIFIED]);
		const outsider = await createTestAccount(harness);
		for (const [path, id] of expressionIds(source)) {
			const community = await getSource(harness, outsider.token, path, id);
			expect(Object.keys(community).sort()).toEqual(SOURCE_GUILD_KEYS);
			expect(community.id).toBe(source.guild.id);
			expect(community.name).toBe('Discoverable Source');
			expect(community.icon).toBe(withIcon.icon);
			expect(community.features).toContain(GuildFeatures.DISCOVERABLE);
			expect(community.features).toContain(GuildFeatures.VERIFIED);
		}
	});

	test('reveals a private community to its members for both kinds', async () => {
		const source = await createSource(harness, 'Private Source');
		const member = await joinSource(harness, source);
		for (const token of [source.owner.token, member.token]) {
			for (const [path, id] of expressionIds(source)) {
				const community = await getSource(harness, token, path, id);
				expect(community.id).toBe(source.guild.id);
				expect(community.name).toBe('Private Source');
				expect(community.features).not.toContain(GuildFeatures.DISCOVERABLE);
			}
		}
	});

	test('hides a private community from non-members for both kinds', async () => {
		const source = await createSource(harness, 'Hidden Source');
		const outsider = await createTestAccount(harness);
		for (const [path, id] of expressionIds(source)) {
			await expectHiddenSource(harness, outsider.token, path, id);
		}
	});

	test('limits the revealed features to the badge flags', async () => {
		const source = await createSource(harness, 'Feature Source');
		await grantGuildFeatures(harness, source.guild.id, [
			GuildFeatures.DISCOVERABLE,
			GuildFeatures.VERIFIED,
			GuildFeatures.INVITES_DISABLED,
			GuildFeatures.CLONE_EMOJI_ENABLED,
		]);
		const outsider = await createTestAccount(harness);
		for (const [path, id] of expressionIds(source)) {
			const community = await getSource(harness, outsider.token, path, id);
			expect(community.features).not.toContain(GuildFeatures.INVITES_DISABLED);
			expect(community.features).not.toContain(GuildFeatures.CLONE_EMOJI_ENABLED);
		}
	});

	test('keeps the expression metadata contract without the source community', async () => {
		const source = await createSource(harness, 'Metadata Source');
		await grantGuildFeatures(harness, source.guild.id, [GuildFeatures.DISCOVERABLE]);
		const outsider = await createTestAccount(harness);
		const emoji = await createBuilder<GuildEmojiMetadataResponse>(harness, outsider.token)
			.get(`/emojis/${source.emoji.id}/metadata`)
			.expect(HTTP_STATUS.OK)
			.execute();
		const sticker = await createBuilder<GuildStickerMetadataResponse>(harness, outsider.token)
			.get(`/stickers/${source.sticker.id}/metadata`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(Object.keys(emoji).sort()).toEqual(METADATA_KEYS);
		expect(Object.keys(sticker).sort()).toEqual(METADATA_KEYS);
	});
});
