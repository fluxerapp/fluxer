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

import {getRoles, updateRole} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {
	acceptInvite,
	createChannelInvite,
	createDMChannel,
	createFriendship,
	createGuild,
	createMessageHarness,
	createTestAccount,
	ensureSessionStarted,
	sendMessage,
	updateChannelPermissions,
} from '@fluxer/api/src/message/tests/MessageTestUtils';
import type {ApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS, TEST_IDS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder} from '@fluxer/api/src/test/TestRequestBuilder';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {MessageReferenceTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message forwarding access control', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createMessageHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness?.shutdown();
	});

	describe('DM access control', () => {
		it('rejects forwarding from a DM the user is not a participant of', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);

			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await ensureSessionStarted(harness, attacker.token);

			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user1, attacker);

			const privateDm = await createDMChannel(harness, user1.token, user2.userId);
			const originalMessage = await sendMessage(harness, user1.token, privateDm.id, 'This is a private message');

			const attackerDm = await createDMChannel(harness, attacker.token, user1.userId);

			await createBuilder(harness, attacker.token)
				.post(`/channels/${attackerDm.id}/messages`)
				.body({
					message_reference: {
						message_id: originalMessage.id,
						channel_id: privateDm.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});

		it('rejects forwarding with a fabricated DM channel ID', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);

			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);

			await createFriendship(harness, user1, user2);

			const dm = await createDMChannel(harness, user1.token, user2.userId);
			const message = await sendMessage(harness, user1.token, dm.id, 'Test');

			await createBuilder(harness, user1.token)
				.post(`/channels/${dm.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: TEST_IDS.NONEXISTENT_CHANNEL,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});

		it('allows forwarding from a DM the user is a participant of', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const user3 = await createTestAccount(harness);

			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await ensureSessionStarted(harness, user3.token);

			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user1, user3);

			const sourceDm = await createDMChannel(harness, user1.token, user2.userId);
			const destDm = await createDMChannel(harness, user1.token, user3.userId);
			const originalMessage = await sendMessage(harness, user1.token, sourceDm.id, 'Message to forward');

			const forwarded = await createBuilder<{id: string; message_snapshots?: Array<{content?: string}>}>(
				harness,
				user1.token,
			)
				.post(`/channels/${destDm.id}/messages`)
				.body({
					message_reference: {
						message_id: originalMessage.id,
						channel_id: sourceDm.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(forwarded.message_snapshots).toBeDefined();
			expect(forwarded.message_snapshots!.length).toBeGreaterThan(0);
			expect(forwarded.message_snapshots![0].content).toBe('Message to forward');
		});
	});

	describe('Guild channel access control', () => {
		it('rejects forwarding from a guild channel the user cannot view', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);

			const guild = await createGuild(harness, owner.token, 'Access control test guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);

			const secretChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'secret', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const publicChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'public', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const secretMessage = await sendMessage(harness, owner.token, secretChannel.id, 'Top secret content');

			await updateChannelPermissions(harness, owner.token, secretChannel.id, member.userId, {
				type: 1,
				deny: Permissions.VIEW_CHANNEL.toString(),
			});

			await createBuilder(harness, member.token)
				.post(`/channels/${publicChannel.id}/messages`)
				.body({
					message_reference: {
						message_id: secretMessage.id,
						channel_id: secretChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});

		it('rejects forwarding from a guild channel without READ_MESSAGE_HISTORY', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);

			const guild = await createGuild(harness, owner.token, 'Read history test guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);

			const sourceChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'source', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const destChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'destination', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, owner.token, sourceChannel.id, 'Message with restricted history');

			await updateChannelPermissions(harness, owner.token, sourceChannel.id, member.userId, {
				type: 1,
				deny: Permissions.READ_MESSAGE_HISTORY.toString(),
			});

			await createBuilder(harness, member.token)
				.post(`/channels/${destChannel.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: sourceChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});

		it('rejects forwarding from a guild the user is not a member of', async () => {
			const guildOwner = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);

			await ensureSessionStarted(harness, guildOwner.token);
			await ensureSessionStarted(harness, outsider.token);

			await createFriendship(harness, guildOwner, outsider);

			const guild = await createGuild(harness, guildOwner.token, 'Private guild');
			const guildChannel = await createBuilder<{id: string}>(harness, guildOwner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'internal', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, guildOwner.token, guildChannel.id, 'Internal guild message');

			const outsiderGuild = await createGuild(harness, outsider.token, 'Outsider guild');

			await createBuilder(harness, outsider.token)
				.post(`/channels/${outsiderGuild.system_channel_id!}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: guildChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
		});

		it('rejects forwarding with a nonexistent source message ID', async () => {
			const owner = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);

			const guild = await createGuild(harness, owner.token, 'Nonexistent message test');

			const channel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'test', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			await createBuilder(harness, owner.token)
				.post(`/channels/${channel.id}/messages`)
				.body({
					message_reference: {
						message_id: TEST_IDS.NONEXISTENT_MESSAGE,
						channel_id: channel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_MESSAGE)
				.execute();
		});

		it('rejects forwarding from a guild channel where @everyone lacks READ_MESSAGE_HISTORY', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);

			const guild = await createGuild(harness, owner.token, 'Role permission test guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);

			const sourceChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'source', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const destChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'dest', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, owner.token, sourceChannel.id, 'Secret history message');

			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				const currentPerms = BigInt(everyoneRole.permissions);
				await updateRole(harness, owner.token, guild.id, everyoneRole.id, {
					permissions: (currentPerms & ~Permissions.READ_MESSAGE_HISTORY).toString(),
				});
			}

			await createBuilder(harness, member.token)
				.post(`/channels/${destChannel.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: sourceChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.execute();
		});
	});

	describe('Cross-boundary forwarding', () => {
		it('rejects forwarding from inaccessible guild channel to a DM', async () => {
			const guildOwner = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);

			await ensureSessionStarted(harness, guildOwner.token);
			await ensureSessionStarted(harness, outsider.token);

			await createFriendship(harness, guildOwner, outsider);

			const guild = await createGuild(harness, guildOwner.token, 'Guild to DM test');
			const guildChannel = await createBuilder<{id: string}>(harness, guildOwner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'private', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, guildOwner.token, guildChannel.id, 'Guild-only content');

			const dm = await createDMChannel(harness, outsider.token, guildOwner.userId);

			await createBuilder(harness, outsider.token)
				.post(`/channels/${dm.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: guildChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
		});

		it('rejects forwarding from inaccessible DM to a guild channel', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);

			await ensureSessionStarted(harness, user1.token);
			await ensureSessionStarted(harness, user2.token);
			await ensureSessionStarted(harness, attacker.token);

			await createFriendship(harness, user1, user2);

			const privateDm = await createDMChannel(harness, user1.token, user2.userId);
			const privateMessage = await sendMessage(harness, user1.token, privateDm.id, 'Secret DM content');

			const attackerGuild = await createGuild(harness, attacker.token, 'Attacker guild');

			await createBuilder(harness, attacker.token)
				.post(`/channels/${attackerGuild.system_channel_id!}/messages`)
				.body({
					message_reference: {
						message_id: privateMessage.id,
						channel_id: privateDm.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});

		it('allows forwarding from accessible guild channel to DM', async () => {
			const owner = await createTestAccount(harness);
			const friend = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, friend.token);

			await createFriendship(harness, owner, friend);

			const guild = await createGuild(harness, owner.token, 'Forward to DM guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, friend.token, invite.code);

			const guildChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'public', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, owner.token, guildChannel.id, 'Forwardable guild message');

			const dm = await createDMChannel(harness, friend.token, owner.userId);

			const forwarded = await createBuilder<{id: string; message_snapshots?: Array<{content?: string}>}>(
				harness,
				friend.token,
			)
				.post(`/channels/${dm.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: guildChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(forwarded.message_snapshots).toBeDefined();
			expect(forwarded.message_snapshots!.length).toBeGreaterThan(0);
			expect(forwarded.message_snapshots![0].content).toBe('Forwardable guild message');
		});

		it('allows forwarding between guilds when user has access to both', async () => {
			const user = await createTestAccount(harness);

			await ensureSessionStarted(harness, user.token);

			const guildA = await createGuild(harness, user.token, 'Guild A');
			const guildB = await createGuild(harness, user.token, 'Guild B');

			const channelA = await createBuilder<{id: string}>(harness, user.token)
				.post(`/guilds/${guildA.id}/channels`)
				.body({name: 'source', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const channelB = await createBuilder<{id: string}>(harness, user.token)
				.post(`/guilds/${guildB.id}/channels`)
				.body({name: 'destination', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const message = await sendMessage(harness, user.token, channelA.id, 'Cross-guild forward');

			const forwarded = await createBuilder<{id: string; message_snapshots?: Array<{content?: string}>}>(
				harness,
				user.token,
			)
				.post(`/channels/${channelB.id}/messages`)
				.body({
					message_reference: {
						message_id: message.id,
						channel_id: channelA.id,
						guild_id: guildA.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.OK)
				.execute();

			expect(forwarded.message_snapshots).toBeDefined();
			expect(forwarded.message_snapshots![0].content).toBe('Cross-guild forward');
		});
	});

	describe('Reported vulnerability: arbitrary ID forwarding', () => {
		it('rejects forwarding a DM message using raw channel and message IDs', async () => {
			const victim1 = await createTestAccount(harness);
			const victim2 = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);

			await ensureSessionStarted(harness, victim1.token);
			await ensureSessionStarted(harness, victim2.token);
			await ensureSessionStarted(harness, attacker.token);

			await createFriendship(harness, victim1, victim2);
			await createFriendship(harness, attacker, victim1);

			const victimDm = await createDMChannel(harness, victim1.token, victim2.userId);
			const privateMessage = await sendMessage(
				harness,
				victim1.token,
				victimDm.id,
				'Private conversation between victims',
			);

			const attackerGuild = await createGuild(harness, attacker.token, 'Attacker guild');

			await createBuilder(harness, attacker.token)
				.post(`/channels/${attackerGuild.system_channel_id!}/messages`)
				.body({
					content: '',
					message_reference: {
						message_id: privateMessage.id,
						channel_id: victimDm.id,
						type: 1,
					},
					flags: 1,
				})
				.expect(HTTP_STATUS.NOT_FOUND, APIErrorCodes.UNKNOWN_CHANNEL)
				.execute();
		});

		it('rejects forwarding a guild message using raw guild, channel, and message IDs', async () => {
			const guildOwner = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);

			await ensureSessionStarted(harness, guildOwner.token);
			await ensureSessionStarted(harness, attacker.token);

			await createFriendship(harness, guildOwner, attacker);

			const targetGuild = await createGuild(harness, guildOwner.token, 'Target private guild');
			const privateChannel = await createBuilder<{id: string}>(harness, guildOwner.token)
				.post(`/guilds/${targetGuild.id}/channels`)
				.body({name: 'private-channel', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const privateMessage = await sendMessage(
				harness,
				guildOwner.token,
				privateChannel.id,
				'Confidential guild message',
			);

			const attackerGuild = await createGuild(harness, attacker.token, 'Attacker guild');

			await createBuilder(harness, attacker.token)
				.post(`/channels/${attackerGuild.system_channel_id!}/messages`)
				.body({
					content: '',
					message_reference: {
						message_id: privateMessage.id,
						channel_id: privateChannel.id,
						guild_id: targetGuild.id,
						type: 1,
					},
					flags: 1,
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.ACCESS_DENIED)
				.execute();
		});

		it('snapshot content is never returned when forward is rejected', async () => {
			const victim1 = await createTestAccount(harness);
			const victim2 = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);

			await ensureSessionStarted(harness, victim1.token);
			await ensureSessionStarted(harness, victim2.token);
			await ensureSessionStarted(harness, attacker.token);

			await createFriendship(harness, victim1, victim2);
			await createFriendship(harness, attacker, victim1);

			const victimDm = await createDMChannel(harness, victim1.token, victim2.userId);
			const secretContent = 'Super secret message that must not leak';
			const privateMessage = await sendMessage(harness, victim1.token, victimDm.id, secretContent);

			const attackerDm = await createDMChannel(harness, attacker.token, victim1.userId);

			const {json} = await createBuilder<Record<string, unknown>>(harness, attacker.token)
				.post(`/channels/${attackerDm.id}/messages`)
				.body({
					content: '',
					message_reference: {
						message_id: privateMessage.id,
						channel_id: victimDm.id,
						type: 1,
					},
					flags: 1,
				})
				.expect(HTTP_STATUS.NOT_FOUND)
				.executeWithResponse();

			const responseText = JSON.stringify(json);
			expect(responseText).not.toContain(secretContent);
			expect(json).not.toHaveProperty('message_snapshots');
		});
	});

	describe('Snapshot content isolation', () => {
		it('does not leak message content through forward snapshot when access is denied', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);

			await ensureSessionStarted(harness, owner.token);
			await ensureSessionStarted(harness, member.token);

			const guild = await createGuild(harness, owner.token, 'Snapshot isolation test guild');
			const invite = await createChannelInvite(harness, owner.token, guild.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);

			const secretChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'secret', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			const destChannel = await createBuilder<{id: string}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'dest', type: 0})
				.expect(HTTP_STATUS.OK)
				.execute();

			await sendMessage(harness, owner.token, secretChannel.id, 'Highly confidential information');

			await updateChannelPermissions(harness, owner.token, secretChannel.id, member.userId, {
				type: 1,
				deny: Permissions.VIEW_CHANNEL.toString(),
			});

			const messages = await createBuilder<Array<{id: string}>>(harness, owner.token)
				.get(`/channels/${secretChannel.id}/messages`)
				.expect(HTTP_STATUS.OK)
				.execute();

			const secretMessageId = messages[0].id;

			const {json} = await createBuilder<Record<string, unknown>>(harness, member.token)
				.post(`/channels/${destChannel.id}/messages`)
				.body({
					message_reference: {
						message_id: secretMessageId,
						channel_id: secretChannel.id,
						guild_id: guild.id,
						type: MessageReferenceTypes.FORWARD,
					},
				})
				.expect(HTTP_STATUS.FORBIDDEN)
				.executeWithResponse();

			expect(json).not.toHaveProperty('message_snapshots');
			expect(JSON.stringify(json)).not.toContain('Highly confidential information');
		});
	});
});
