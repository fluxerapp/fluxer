// SPDX-License-Identifier: AGPL-3.0-or-later

import {beforeAll, beforeEach, describe, expect, it} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
	deleteChannel,
	getChannel,
	updateChannel,
} from './ChannelTestUtils';

describe('Channel Operation Permissions', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	it('should allow member to get channel', async () => {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const systemChannel = await getChannel(harness, owner.token, guild.system_channel_id!);
		const invite = await createChannelInvite(harness, owner.token, systemChannel.id);
		await acceptInvite(harness, member.token, invite.code);
		const channel = await getChannel(harness, member.token, systemChannel.id);
		expect(channel.id).toBe(systemChannel.id);
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
	it('should allow owner to update channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const testChannel = await createChannel(harness, owner.token, guild.id, 'test-channel');
		const updated = await updateChannel(harness, owner.token, testChannel.id, {name: 'owner-updated'});
		expect(updated.name).toBe('owner-updated');
	});
	it('should allow owner to delete channel', async () => {
		const owner = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'Channel Perms Guild');
		const testChannel = await createChannel(harness, owner.token, guild.id, 'test-channel');
		await deleteChannel(harness, owner.token, testChannel.id);
		await createBuilder(harness, owner.token)
			.get(`/channels/${testChannel.id}`)
			.expect(HTTP_STATUS.NOT_FOUND)
			.execute();
	});
});
