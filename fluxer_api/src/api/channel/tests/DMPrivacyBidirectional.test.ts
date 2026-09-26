// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGuild,
	getChannel,
	leaveGuild,
	sendChannelMessage,
	updateUserSettings,
} from '@app/api/channel/tests/ChannelTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {removeRelationship} from '@app/api/user/tests/RelationshipTestUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {UserFlags} from '@fluxer/constants/src/UserConstants';
import {afterAll, beforeAll, beforeEach, describe, it} from 'vitest';

async function setUserFlags(harness: ApiTestHarness, userId: string, flags: bigint): Promise<void> {
	await createBuilder(harness, '')
		.patch(`/test/users/${userId}/flags`)
		.body({flags: flags.toString()})
		.expect(HTTP_STATUS.OK)
		.execute();
}

describe('DM Privacy Bidirectional Enforcement', () => {
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
	describe('sender DM restrictions', () => {
		it('blocks message when sender has defaultGuildsRestricted and no mutual guild', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			await createFriendship(harness, sender, target);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await sendChannelMessage(harness, sender.token, channel.id, 'while friends');
			await removeRelationship(harness, sender.token, target.userId);
			await updateUserSettings(harness, sender.token, {default_guilds_restricted: true});
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'after restricting'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
		it('allows message when sender has defaultGuildsRestricted but shares a mutual guild', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			const guild = await createGuild(harness, sender.token, 'Mutual Community');
			const systemChannel = await getChannel(harness, sender.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, sender.token, systemChannel.id);
			await acceptInvite(harness, target.token, invite.code);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await updateUserSettings(harness, sender.token, {default_guilds_restricted: true});
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'with mutual guild'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		it('allows message when the only mutual guild is verified and the guild id is not disqualified', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			await createFriendship(harness, sender, target);
			const guild = await createGuild(harness, sender.token, 'Verified Community');
			await createBuilder(harness, '')
				.post(`/test/guilds/${guild.id}/features`)
				.body({add_features: [GuildFeatures.VERIFIED]})
				.expect(HTTP_STATUS.OK)
				.execute();
			const systemChannel = await getChannel(harness, sender.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, sender.token, systemChannel.id);
			await acceptInvite(harness, target.token, invite.code);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await removeRelationship(harness, sender.token, target.userId);
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'verified mutual guild'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		it('blocks message when sender restricts the only mutual guild', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			const guild = await createGuild(harness, sender.token, 'Restricted Community');
			const systemChannel = await getChannel(harness, sender.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, sender.token, systemChannel.id);
			await acceptInvite(harness, target.token, invite.code);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await updateUserSettings(harness, sender.token, {restricted_guilds: [guild.id]});
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'restricted guild'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
		it('blocks message when sender has defaultGuildsRestricted and target leaves mutual guild', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			const guild = await createGuild(harness, sender.token, 'Community');
			const systemChannel = await getChannel(harness, sender.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, sender.token, systemChannel.id);
			await acceptInvite(harness, target.token, invite.code);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await updateUserSettings(harness, sender.token, {default_guilds_restricted: true});
			await leaveGuild(harness, target.token, guild.id);
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'after target left'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
	});
	describe('friends bypass sender restrictions', () => {
		it('allows message when sender has defaultGuildsRestricted but users are friends', async () => {
			const sender = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await ensureSessionStarted(harness, sender.token);
			await ensureSessionStarted(harness, target.token);
			await createFriendship(harness, sender, target);
			const channel = await createDmChannel(harness, sender.token, target.userId);
			await updateUserSettings(harness, sender.token, {default_guilds_restricted: true});
			await createBuilder(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'friends bypass restrictions'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
	describe('symmetry of enforcement', () => {
		it('blocks both directions when only sender has restrictions and no mutual guild', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await createFriendship(harness, user1, user2);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await removeRelationship(harness, user1.token, user2.userId);
			await updateUserSettings(harness, user1.token, {default_guilds_restricted: true});
			await createBuilder(harness, user2.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'target to restricted sender'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
			await createBuilder(harness, user1.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'restricted sender to target'})
				.expect(HTTP_STATUS.BAD_REQUEST, 'CANNOT_SEND_MESSAGES_TO_USER')
				.execute();
		});
		it('allows both directions when mutual guild exists and both have restrictions', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			const guild = await createGuild(harness, user1.token, 'Shared Community');
			const systemChannel = await getChannel(harness, user1.token, guild.system_channel_id!);
			const invite = await createChannelInvite(harness, user1.token, systemChannel.id);
			await acceptInvite(harness, user2.token, invite.code);
			const channel = await createDmChannel(harness, user1.token, user2.userId);
			await updateUserSettings(harness, user1.token, {default_guilds_restricted: true});
			await updateUserSettings(harness, user2.token, {default_guilds_restricted: true});
			await createBuilder(harness, user1.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'user1 to user2'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, user2.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'user2 to user1'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
		it('allows both directions for staff-access users with an existing DM relationship', async () => {
			const staff = await createTestAccount(harness);
			const target = await createTestAccount(harness);
			await setUserFlags(harness, staff.userId, UserFlags.STAFF);
			await ensureSessionStarted(harness, staff.token);
			await ensureSessionStarted(harness, target.token);
			await createFriendship(harness, staff, target);
			await updateUserSettings(harness, staff.token, {
				default_guilds_restricted: true,
				staff_dm_access_user_ids: [target.userId],
			});
			const channel = await createDmChannel(harness, staff.token, target.userId);
			await createBuilder(harness, staff.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'staff to target'})
				.expect(HTTP_STATUS.OK)
				.execute();
			await createBuilder(harness, target.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'target to staff'})
				.expect(HTTP_STATUS.OK)
				.execute();
		});
	});
});
