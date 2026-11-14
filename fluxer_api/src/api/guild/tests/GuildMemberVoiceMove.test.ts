// SPDX-License-Identifier: AGPL-3.0-or-later

import {createChannel, setupTestGuildWithMembers} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, test} from 'vitest';

describe('Guild Member Voice Move', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});

	test('rejects a voice move from a member without permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 2);
		const [actor, target] = members;
		const voiceChannel = await createChannel(harness, owner.token, guild.id, 'Voice', ChannelTypes.GUILD_VOICE);
		await createBuilder(harness, actor.token)
			.patch(`/guilds/${guild.id}/members/${target.userId}`)
			.body({channel_id: voiceChannel.id})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});

	test('rejects moving yourself without the move-members permission', async () => {
		const {owner, members, guild} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0];
		const voiceChannel = await createChannel(harness, owner.token, guild.id, 'Voice', ChannelTypes.GUILD_VOICE);
		await createBuilder(harness, member.token)
			.patch(`/guilds/${guild.id}/members/${member.userId}`)
			.body({channel_id: voiceChannel.id})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
