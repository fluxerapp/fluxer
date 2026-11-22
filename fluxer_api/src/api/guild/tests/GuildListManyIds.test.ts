// SPDX-License-Identifier: AGPL-3.0-or-later

import {createGuildID, createUserID, type GuildID} from '@app/api/BrandedTypes';
import {GuildRepository} from '@app/api/guild/repositories/GuildRepository';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

const GUILD_COUNT = 250;
const FIRST_GUILD_ID = 1_310_000_000_000_000_000n;
const OWNER_ID = createUserID(1_310_000_000_000_000_999n);

describe('Listing many guilds by id', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('returns every guild when the ids span several query chunks', async () => {
		const repository = new GuildRepository();
		const ids: Array<GuildID> = [];
		for (let index = 0; index < GUILD_COUNT; index++) {
			const guildId = createGuildID(FIRST_GUILD_ID + BigInt(index));
			await repository.upsertPartial(guildId, {name: `guild ${index}`, owner_id: OWNER_ID});
			ids.push(guildId);
		}

		const guilds = await repository.listGuilds(ids);

		expect(guilds.map((guild) => guild.id).sort()).toEqual([...ids].sort());
	});
});
