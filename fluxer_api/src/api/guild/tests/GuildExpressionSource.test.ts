// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {getPngDataUrl} from '@app/api/emoji/tests/EmojiTestUtils';
import {createGuild, updateGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import type {
	GuildEmojiWithUserResponse,
	GuildExpressionSourceGuildResponse,
	GuildStickerWithUserResponse,
} from '@fluxer/schema/src/domains/guild/GuildEmojiSchemas';
import type {GuildResponse} from '@fluxer/schema/src/domains/guild/GuildResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

const SOURCE_GUILD_KEYS = ['features', 'icon', 'id', 'name'];

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

	test('hides a private community from non-members for both kinds', async () => {
		const source = await createSource(harness, 'Hidden Source');
		const outsider = await createTestAccount(harness);
		for (const [path, id] of expressionIds(source)) {
			await expectHiddenSource(harness, outsider.token, path, id);
		}
	});
});
