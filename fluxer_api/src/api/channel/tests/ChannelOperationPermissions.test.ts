// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	getChannel,
	updateChannel,
	updateGuild,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Channel Operation Permissions', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should reject nonmember from getting channel', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const nonmember = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, nonmember.token)
			.get(`/channels/${systemChannel.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should let a minor manage a mature channel without reading it', async () => {
		const owner = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
		const guild = await createGuild(harness, owner.token, 'Mature Channel Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		await updateChannel(harness, owner.token, systemChannel.id, {nsfw: true});
		const renamed = await updateChannel(harness, owner.token, systemChannel.id, {name: 'still-manageable'});
		expect(renamed.name).toBe('still-manageable');
		await createBuilder(harness, owner.token)
			.get(`/channels/${systemChannel.id}/messages`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should gate channels created before a guild becomes adult-only', async () => {
		const owner = await createTestAccount(harness);
		const minor = await createTestAccount(harness, {dateOfBirth: '2010-01-01'});
		const guild = await createGuild(harness, owner.token, 'Later Mature Guild');
		const category = await createChannel(harness, owner.token, guild.id, 'category', 4);
		const child = await createBuilder<{id: string; nsfw_override?: boolean | null}>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'child', type: 0, parent_id: category.id})
			.execute();
		const opened = await createBuilder<{id: string}>(harness, owner.token)
			.post(`/guilds/${guild.id}/channels`)
			.body({name: 'opened', type: 0, nsfw_override: false})
			.execute();
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		expect(systemChannel.nsfw_override ?? null).toBeNull();
		expect(category.nsfw_override ?? null).toBeNull();
		expect(child.nsfw_override ?? null).toBeNull();
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, minor.token, invite.code);
		await updateGuild(harness, owner.token, guild.id, {nsfw: true});
		for (const channelId of [systemChannel.id, child.id]) {
			await createBuilder(harness, minor.token)
				.get(`/channels/${channelId}/messages`)
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		}
		await createBuilder(harness, minor.token).get(`/channels/${opened.id}/messages`).expect(HTTP_STATUS.OK).execute();
	});
	it('should reject member from updating channel without MANAGE_CHANNELS', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, member.token)
			.patch(`/channels/${systemChannel.id}`)
			.body({name: 'hacked'})
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
	it('should reject member from deleting channel without MANAGE_CHANNELS', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		await createBuilder(harness, member.token)
			.delete(`/channels/${systemChannel.id}`)
			.expect(HTTP_STATUS.FORBIDDEN)
			.execute();
	});
});
