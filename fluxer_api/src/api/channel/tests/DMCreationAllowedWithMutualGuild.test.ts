// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createGuild,
	getChannel,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('DM creation allowed with mutual guild', () => {
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
	it('allows users sharing a guild to create DMs without being friends', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const guild = await createGuild(harness, user1.token, 'Test Community');
		const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
		await acceptInvite(harness, user2.token, invite.code);
		const dm = await createDmChannel(harness, user1.token, user2.userId);
		expect(dm.id).toBeTruthy();
		expect(dm.id.length).toBeGreaterThan(0);
	});
	it('allows a verified mutual guild to create DMs without being friends when the guild id is not disqualified', async () => {
		const user1 = await createTestAccount(harness);
		const user2 = await createTestAccount(harness);
		const guild = await createGuild(harness, user1.token, 'Verified Community');
		await createBuilder(harness, '')
			.post(`/test/guilds/${guild.id}/features`)
			.body({add_features: [GuildFeatures.VERIFIED]})
			.execute();
		const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
		await acceptInvite(harness, user2.token, invite.code);
		const dm = await createDmChannel(harness, user1.token, user2.userId);
		expect(dm.id).toBeTruthy();
	});
});
