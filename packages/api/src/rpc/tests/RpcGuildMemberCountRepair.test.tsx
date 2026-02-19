/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {createTestAccount} from '@fluxer/api/src/auth/tests/AuthTestUtils';
import {createGuildID} from '@fluxer/api/src/BrandedTypes';
import {GuildRepository} from '@fluxer/api/src/guild/repositories/GuildRepository';
import {createGuild} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface RpcGuildCollectionResponse {
	type: 'guild_collection';
	data: {
		collection: 'guild';
		guild: {
			id: string;
		};
	};
}

async function setGuildMemberCount(harness: ApiTestHarness, guildId: string, memberCount: number): Promise<void> {
	await createBuilder(harness, '')
		.post(`/test/guilds/${guildId}/member-count`)
		.body({member_count: memberCount})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('RpcService guild member count repair', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('repairs guild member_count from guild_members count when fetching guild data', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'RPC Guild Member Count Repair');
		const guildId = createGuildID(BigInt(guild.id));
		const guildRepository = new GuildRepository();

		await setGuildMemberCount(harness, guild.id, 999);

		const staleGuild = await guildRepository.findUnique(guildId);
		expect(staleGuild).toBeTruthy();
		if (!staleGuild) {
			throw new Error('Expected guild to exist before RPC member_count repair');
		}
		expect(staleGuild.memberCount).toBe(999);

		const rpcResponse = await createBuilder<RpcGuildCollectionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'guild_collection',
				guild_id: guild.id,
				collection: 'guild',
			})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(rpcResponse.type).toBe('guild_collection');
		expect(rpcResponse.data.collection).toBe('guild');
		expect(rpcResponse.data.guild.id).toBe(guild.id);

		const repairedGuild = await guildRepository.findUnique(guildId);
		expect(repairedGuild).toBeTruthy();
		if (!repairedGuild) {
			throw new Error('Expected guild to exist after RPC member_count repair');
		}

		const actualMemberCount = await guildRepository.countMembers(guildId);
		expect(actualMemberCount).toBe(1);
		expect(repairedGuild.memberCount).toBe(actualMemberCount);
	});
});
