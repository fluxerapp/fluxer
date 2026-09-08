// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {GuildNSFWLevel} from '@fluxer/constants/src/GuildConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {
	acceptInvite,
	addMemberRole,
	createChannel,
	createChannelInvite,
	createDmChannel,
	createFriendship,
	createGroupDmChannel,
	createGuild,
	createPermissionOverwrite,
	createRole,
	deleteChannel,
	leaveGuild,
	removeRecipientFromGroupDm,
	sendChannelMessage,
	updateChannel,
	updateRole,
} from '../../channel/tests/ChannelTestUtils';
import {getRoles, updateGuild} from '../../guild/tests/GuildTestUtils';
import {
	markChannelAsIndexed,
	markGuildChannelsAsIndexed,
	markUserDmChannelsAsIndexed,
} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface MessageSearchResult {
	channels: Array<{
		id: string;
		recipients?: Array<{
			id: string;
		}>;
	}>;
	messages: Array<{
		id: string;
		channel_id: string;
		content: string;
	}>;
	total: number;
	hits_per_page: number;
	page: number;
}

interface MessageSearchIndexingResponse {
	indexing: true;
}

type MessageSearchResponse = MessageSearchResult | MessageSearchIndexingResponse;

function isSearchResult(response: MessageSearchResponse): response is MessageSearchResult {
	return 'messages' in response;
}

describe('Message Search Permissions', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness({search: 'enabled'});
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	describe('Guild membership permissions', () => {
		test('cannot search in guilds user is not a member of', async () => {
			const owner = await createTestAccount(harness);
			const nonMember = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Private Guild');
			const channelId = guild.system_channel_id!;
			await sendChannelMessage(harness, owner.token, channelId, 'secret guild message');
			await markChannelAsIndexed(harness, channelId);
			await createBuilder(harness, nonMember.token)
				.post('/search/messages')
				.body({
					content: 'secret',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
		test('can search in guilds user is a member of', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			await sendChannelMessage(harness, owner.token, channelId, 'searchable guild content');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'searchable',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
			expect(result.messages[0]?.content).toContain('searchable');
		});
		test('after leaving guild, search results exclude that guilds messages', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			await sendChannelMessage(harness, owner.token, channelId, 'member can see this initially');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const beforeLeave = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'member can see',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(beforeLeave)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(beforeLeave.messages.length).toBeGreaterThan(0);
			await leaveGuild(harness, member.token, guild.id);
			await createBuilder(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'member can see',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_GUILD')
				.execute();
		});
	});
	describe('Channel permissions', () => {
		test('requires READ_MESSAGE_HISTORY permission to search channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			await sendChannelMessage(harness, owner.token, channelId, 'restricted content');
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				const permissions = BigInt(everyoneRole.permissions);
				const newPermissions = permissions & ~Permissions.READ_MESSAGE_HISTORY;
				await updateRole(harness, owner.token, guild.id, everyoneRole.id, {
					permissions: newPermissions.toString(),
				});
			}
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'restricted',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages).toEqual([]);
		});
		test('without READ_MESSAGE_HISTORY, channel messages are excluded from guild search', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const systemChannelId = guild.system_channel_id!;
			const restrictedChannel = await createChannel(
				harness,
				owner.token,
				guild.id,
				'restricted',
				ChannelTypes.GUILD_TEXT,
			);
			await sendChannelMessage(harness, owner.token, restrictedChannel.id, 'hidden from member search');
			await sendChannelMessage(harness, owner.token, systemChannelId, 'visible to member search');
			const invite = await createChannelInvite(harness, owner.token, systemChannelId);
			await acceptInvite(harness, member.token, invite.code);
			await createPermissionOverwrite(harness, owner.token, restrictedChannel.id, member.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.READ_MESSAGE_HISTORY.toString(),
			});
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'search',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const restrictedMessages = result.messages.filter((m) => m.channel_id === restrictedChannel.id);
			const visibleMessages = result.messages.filter((m) => m.channel_id === systemChannelId);
			expect(restrictedMessages.length).toBe(0);
			expect(visibleMessages.length).toBeGreaterThan(0);
		});
		test('private channels only searchable by members with access', async () => {
			const owner = await createTestAccount(harness);
			const allowedMember = await createTestAccount(harness);
			const deniedMember = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const systemChannelId = guild.system_channel_id!;
			const privateChannel = await createChannel(harness, owner.token, guild.id, 'private', ChannelTypes.GUILD_TEXT);
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				await createPermissionOverwrite(harness, owner.token, privateChannel.id, everyoneRole.id, {
					type: 0,
					allow: '0',
					deny: Permissions.VIEW_CHANNEL.toString(),
				});
			}
			await sendChannelMessage(harness, owner.token, privateChannel.id, 'private channel secrets');
			const invite = await createChannelInvite(harness, owner.token, systemChannelId);
			await acceptInvite(harness, allowedMember.token, invite.code);
			await acceptInvite(harness, deniedMember.token, invite.code);
			await createPermissionOverwrite(harness, owner.token, privateChannel.id, allowedMember.userId, {
				type: 1,
				allow: (Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY).toString(),
				deny: '0',
			});
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const allowedResult = await createBuilder<MessageSearchResponse>(harness, allowedMember.token)
				.post('/search/messages')
				.body({
					content: 'secrets',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			const deniedResult = await createBuilder<MessageSearchResponse>(harness, deniedMember.token)
				.post('/search/messages')
				.body({
					content: 'secrets',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(allowedResult)) {
				expect.fail('Expected search result but got indexing response for allowed user');
			}
			if (!isSearchResult(deniedResult)) {
				expect.fail('Expected search result but got indexing response for denied user');
			}
			expect(allowedResult.messages.length).toBeGreaterThan(0);
			expect(deniedResult.messages.length).toBe(0);
		});
	});
	describe('NSFW filtering', () => {
		test('by default (include_nsfw: false), NSFW channel messages are excluded', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const systemChannelId = guild.system_channel_id!;
			const nsfwChannel = await createChannel(harness, owner.token, guild.id, 'nsfw-channel', ChannelTypes.GUILD_TEXT);
			await updateChannel(harness, owner.token, nsfwChannel.id, {nsfw: true});
			await sendChannelMessage(harness, owner.token, nsfwChannel.id, 'nsfw unique content here');
			await sendChannelMessage(harness, owner.token, systemChannelId, 'sfw unique content here');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'unique content',
					context_guild_id: guild.id,
					include_nsfw: false,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const nsfwMessages = result.messages.filter((m) => m.channel_id === nsfwChannel.id);
			const sfwMessages = result.messages.filter((m) => m.channel_id === systemChannelId);
			expect(nsfwMessages.length).toBe(0);
			expect(sfwMessages.length).toBeGreaterThan(0);
		});
		test('with include_nsfw: true, NSFW channel messages are included', async () => {
			const owner = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const nsfwChannel = await createChannel(harness, owner.token, guild.id, 'nsfw-channel', ChannelTypes.GUILD_TEXT);
			await updateChannel(harness, owner.token, nsfwChannel.id, {nsfw: true});
			await sendChannelMessage(harness, owner.token, nsfwChannel.id, 'nsfw searchable message');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'nsfw searchable',
					context_guild_id: guild.id,
					include_nsfw: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
			expect(result.messages[0]?.channel_id).toBe(nsfwChannel.id);
		});
		test('user must still have access to the NSFW channel', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const systemChannelId = guild.system_channel_id!;
			const nsfwChannel = await createChannel(harness, owner.token, guild.id, 'nsfw-channel', ChannelTypes.GUILD_TEXT);
			await updateChannel(harness, owner.token, nsfwChannel.id, {nsfw: true});
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				await createPermissionOverwrite(harness, owner.token, nsfwChannel.id, everyoneRole.id, {
					type: 0,
					allow: '0',
					deny: Permissions.VIEW_CHANNEL.toString(),
				});
			}
			await sendChannelMessage(harness, owner.token, nsfwChannel.id, 'nsfw restricted content');
			const invite = await createChannelInvite(harness, owner.token, systemChannelId);
			await acceptInvite(harness, member.token, invite.code);
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'nsfw restricted',
					context_guild_id: guild.id,
					include_nsfw: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBe(0);
		});
		test('age-restricted guild is searchable by an adult member without include_nsfw', async () => {
			const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const guild = await createGuild(harness, owner.token, 'Age Restricted Search Guild');
			const systemChannelId = guild.system_channel_id!;
			await sendChannelMessage(harness, owner.token, systemChannelId, 'age restricted guild searchable message');
			await updateGuild(harness, owner.token, guild.id, {nsfw_level: GuildNSFWLevel.AGE_RESTRICTED});
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const excluded = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'age restricted guild searchable',
					context_guild_id: guild.id,
					include_nsfw: false,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(excluded)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(excluded.messages.some((m) => m.channel_id === systemChannelId)).toBe(true);
			const included = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'age restricted guild searchable',
					context_guild_id: guild.id,
					include_nsfw: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(included)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(included.messages.length).toBeGreaterThan(0);
			expect(included.messages.some((m) => m.channel_id === systemChannelId)).toBe(true);
		});
		test('age-restricted guild is searchable in a channel pinned to nsfw_override: false', async () => {
			const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const guild = await createGuild(harness, owner.token, 'Age Restricted Override Guild');
			const channel = await createBuilder<{id: string; nsfw_override?: boolean | null}>(harness, owner.token)
				.post(`/guilds/${guild.id}/channels`)
				.body({name: 'override-channel', type: ChannelTypes.GUILD_TEXT, nsfw: false})
				.execute();
			expect(channel.nsfw_override).toBe(false);
			await sendChannelMessage(harness, owner.token, channel.id, 'age restricted override searchable message');
			await updateGuild(harness, owner.token, guild.id, {nsfw_level: GuildNSFWLevel.AGE_RESTRICTED});
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'age restricted override searchable',
					context_guild_id: guild.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.some((m) => m.channel_id === channel.id)).toBe(true);
		});
		test('underage user cannot search messages in an age-restricted guild', async () => {
			const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const underageMember = await createTestAccount(harness, {dateOfBirth: '2012-01-01'});
			const guild = await createGuild(harness, owner.token, 'Age Restricted Guild');
			const systemChannelId = guild.system_channel_id!;
			const invite = await createChannelInvite(harness, owner.token, systemChannelId);
			await acceptInvite(harness, underageMember.token, invite.code);
			await sendChannelMessage(harness, owner.token, systemChannelId, 'underage search test message');
			await updateGuild(harness, owner.token, guild.id, {nsfw_level: GuildNSFWLevel.AGE_RESTRICTED});
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			await createBuilder(harness, underageMember.token)
				.post('/search/messages')
				.body({
					content: 'underage search test',
					context_guild_id: guild.id,
					include_nsfw: true,
				})
				.expect(HTTP_STATUS.FORBIDDEN, APIErrorCodes.NSFW_CONTENT_AGE_RESTRICTED)
				.execute();
		});
		test('scope: all_guilds excludes NSFW channel messages by default', async () => {
			const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const guild = await createGuild(harness, owner.token, 'All Guilds NSFW Filter Guild');
			const systemChannelId = guild.system_channel_id!;
			const nsfwChannel = await createChannel(harness, owner.token, guild.id, 'nsfw-channel', ChannelTypes.GUILD_TEXT);
			await updateChannel(harness, owner.token, nsfwChannel.id, {nsfw: true});
			await sendChannelMessage(harness, owner.token, systemChannelId, 'all guilds sfw unique content');
			await sendChannelMessage(harness, owner.token, nsfwChannel.id, 'all guilds nsfw unique content');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'all guilds unique content',
					scope: 'all_guilds',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const nsfwMessages = result.messages.filter((m) => m.channel_id === nsfwChannel.id);
			const sfwMessages = result.messages.filter((m) => m.channel_id === systemChannelId);
			expect(nsfwMessages.length).toBe(0);
			expect(sfwMessages.length).toBeGreaterThan(0);
		});
		test('scope: all_guilds includes NSFW channel messages with include_nsfw: true', async () => {
			const owner = await createTestAccount(harness, {dateOfBirth: '2000-01-01'});
			const guild = await createGuild(harness, owner.token, 'All Guilds NSFW Include Guild');
			const systemChannelId = guild.system_channel_id!;
			const nsfwChannel = await createChannel(harness, owner.token, guild.id, 'nsfw-channel', ChannelTypes.GUILD_TEXT);
			await updateChannel(harness, owner.token, nsfwChannel.id, {nsfw: true});
			await sendChannelMessage(harness, owner.token, systemChannelId, 'all guilds sfw include content');
			await sendChannelMessage(harness, owner.token, nsfwChannel.id, 'all guilds nsfw include content');
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, owner.token)
				.post('/search/messages')
				.body({
					content: 'all guilds include content',
					scope: 'all_guilds',
					include_nsfw: true,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const nsfwMessages = result.messages.filter((m) => m.channel_id === nsfwChannel.id);
			const sfwMessages = result.messages.filter((m) => m.channel_id === systemChannelId);
			expect(nsfwMessages.length).toBeGreaterThan(0);
			expect(sfwMessages.length).toBeGreaterThan(0);
		});
	});
	describe('DM permissions', () => {
		test('can search own DM conversations', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await sendChannelMessage(harness, user1.token, dmChannel.id, 'private dm message search test');
			await markChannelAsIndexed(harness, dmChannel.id);
			const result = await createBuilder<MessageSearchResponse>(harness, user1.token)
				.post('/search/messages')
				.body({
					content: 'dm message search',
					context_channel_id: dmChannel.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
		});
		test('cannot search other users DM conversations', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await sendChannelMessage(harness, user1.token, dmChannel.id, 'secret dm conversation');
			await markChannelAsIndexed(harness, dmChannel.id);
			await createBuilder(harness, outsider.token)
				.post('/search/messages')
				.body({
					content: 'secret dm',
					context_channel_id: dmChannel.id,
				})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
		test('ignores channel_id when searching current DM context', async () => {
			const userA = await createTestAccount(harness);
			const userB = await createTestAccount(harness);
			const userC = await createTestAccount(harness);
			await createFriendship(harness, userA, userB);
			const dmAB = await createDmChannel(harness, userA.token, userB.userId);
			await sendChannelMessage(harness, userA.token, dmAB.id, 'public message from A to B');
			await markChannelAsIndexed(harness, dmAB.id);
			await createFriendship(harness, userB, userC);
			const dmBC = await createDmChannel(harness, userB.token, userC.userId);
			await sendChannelMessage(harness, userB.token, dmBC.id, 'top secret message from B to C');
			await markChannelAsIndexed(harness, dmBC.id);
			const result = await createBuilder<MessageSearchResponse>(harness, userA.token)
				.post('/search/messages')
				.body({
					content: 'secret',
					context_channel_id: dmAB.id,
					channel_id: [dmBC.id],
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBe(0);
		});
		test('group DMs only searchable by participants', async () => {
			const owner = await createTestAccount(harness);
			const participant1 = await createTestAccount(harness);
			const participant2 = await createTestAccount(harness);
			const outsider = await createTestAccount(harness);
			await createFriendship(harness, owner, participant1);
			await createFriendship(harness, owner, participant2);
			const groupDm = await createGroupDmChannel(harness, owner.token, [participant1.userId, participant2.userId]);
			await sendChannelMessage(harness, owner.token, groupDm.id, 'group dm secret message');
			await markChannelAsIndexed(harness, groupDm.id);
			const participantResult = await createBuilder<MessageSearchResponse>(harness, participant1.token)
				.post('/search/messages')
				.body({
					content: 'group dm secret',
					context_channel_id: groupDm.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(participantResult)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(participantResult.messages.length).toBeGreaterThan(0);
			await createBuilder(harness, outsider.token)
				.post('/search/messages')
				.body({
					content: 'group dm secret',
					context_channel_id: groupDm.id,
				})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
		test('after leaving group DM, user cannot search that group', async () => {
			const owner = await createTestAccount(harness);
			const participant = await createTestAccount(harness);
			const otherParticipant = await createTestAccount(harness);
			await createFriendship(harness, owner, participant);
			await createFriendship(harness, owner, otherParticipant);
			const groupDm = await createGroupDmChannel(harness, owner.token, [participant.userId, otherParticipant.userId]);
			await sendChannelMessage(harness, owner.token, groupDm.id, 'group dm content before leave');
			await markChannelAsIndexed(harness, groupDm.id);
			const beforeLeave = await createBuilder<MessageSearchResponse>(harness, participant.token)
				.post('/search/messages')
				.body({
					content: 'before leave',
					context_channel_id: groupDm.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(beforeLeave)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(beforeLeave.messages.length).toBeGreaterThan(0);
			await removeRecipientFromGroupDm(harness, owner.token, groupDm.id, participant.userId);
			await createBuilder(harness, participant.token)
				.post('/search/messages')
				.body({
					content: 'before leave',
					context_channel_id: groupDm.id,
				})
				.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_CHANNEL')
				.execute();
		});
	});
	describe('Scope-based access', () => {
		test('scope: all_guilds only returns guild messages user has access to', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild1 = await createGuild(harness, owner.token, 'Guild One');
			const guild2 = await createGuild(harness, owner.token, 'Guild Two');
			await sendChannelMessage(harness, owner.token, guild1.system_channel_id!, 'guild one searchable message');
			await sendChannelMessage(harness, owner.token, guild2.system_channel_id!, 'guild two searchable message');
			const invite = await createChannelInvite(harness, owner.token, guild1.system_channel_id!);
			await acceptInvite(harness, member.token, invite.code);
			await markGuildChannelsAsIndexed(harness, owner.token, guild1.id);
			await markGuildChannelsAsIndexed(harness, owner.token, guild2.id);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'searchable message',
					scope: 'all_guilds',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const guild1Messages = result.messages.filter((m) => m.channel_id === guild1.system_channel_id);
			const guild2Messages = result.messages.filter((m) => m.channel_id === guild2.system_channel_id);
			expect(guild1Messages.length).toBeGreaterThan(0);
			expect(guild2Messages.length).toBe(0);
		});
		test('scope: all_dms only returns DMs user is participant in', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const user3 = await createTestAccount(harness);
			const user4 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user3, user4);
			const dmChannel12 = await createDmChannel(harness, user1.token, user2.userId);
			const dmChannel34 = await createDmChannel(harness, user3.token, user4.userId);
			await sendChannelMessage(harness, user1.token, dmChannel12.id, 'dm searchable content user12');
			await sendChannelMessage(harness, user3.token, dmChannel34.id, 'dm searchable content user34');
			await markChannelAsIndexed(harness, dmChannel12.id);
			await markChannelAsIndexed(harness, dmChannel34.id);
			const result = await createBuilder<MessageSearchResponse>(harness, user1.token)
				.post('/search/messages')
				.body({
					content: 'dm searchable content',
					scope: 'all_dms',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const user12Messages = result.messages.filter((m) => m.channel_id === dmChannel12.id);
			const user34Messages = result.messages.filter((m) => m.channel_id === dmChannel34.id);
			expect(user12Messages.length).toBeGreaterThan(0);
			expect(user34Messages.length).toBe(0);
		});
		test.each([
			'all_dms',
			'open_dms',
			'all',
			'open_dms_and_all_guilds',
		] as const)('scope: %s does not trust foreign DM context_channel_id', async (scope) => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			const canary = `foreign-dm-context-canary-${Date.now()}-${scope}`;
			await sendChannelMessage(harness, user1.token, dmChannel.id, canary);
			await markChannelAsIndexed(harness, dmChannel.id);
			const result = await createBuilder<MessageSearchResponse>(harness, attacker.token)
				.post('/search/messages')
				.body({
					content: canary,
					scope,
					context_channel_id: dmChannel.id,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.some((message) => message.channel_id === dmChannel.id)).toBe(false);
			expect(result.messages.some((message) => message.content === canary)).toBe(false);
		});
		test('scope: all_dms does not trust a guild context_channel_id', async () => {
			const owner = await createTestAccount(harness);
			const attacker = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Foreign Context Guild');
			const channelId = guild.system_channel_id!;
			const canary = `foreign-guild-context-canary-${Date.now()}`;
			await sendChannelMessage(harness, owner.token, channelId, canary);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, attacker.token)
				.post('/search/messages')
				.body({
					content: canary,
					scope: 'all_dms',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.some((message) => message.channel_id === channelId)).toBe(false);
			expect(result.messages.some((message) => message.content === canary)).toBe(false);
		});
		test('scope: all_dms returns serialized channels for closed DMs', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			const dmChannel = await createDmChannel(harness, user1.token, user2.userId);
			await sendChannelMessage(harness, user1.token, dmChannel.id, 'closed dm searchable content');
			await markChannelAsIndexed(harness, dmChannel.id);
			await deleteChannel(harness, user1.token, dmChannel.id);
			const result = await createBuilder<MessageSearchResponse>(harness, user1.token)
				.post('/search/messages')
				.body({
					content: 'closed dm searchable',
					scope: 'all_dms',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.some((message) => message.channel_id === dmChannel.id)).toBe(true);
			expect(result.channels.some((channel) => channel.id === dmChannel.id)).toBe(true);
			expect(
				result.channels
					.find((channel) => channel.id === dmChannel.id)
					?.recipients?.some((user) => user.id === user2.userId),
			).toBe(true);
		});
		test('scope: open_dms returns only currently open DMs', async () => {
			const user1 = await createTestAccount(harness);
			const user2 = await createTestAccount(harness);
			const user3 = await createTestAccount(harness);
			await createFriendship(harness, user1, user2);
			await createFriendship(harness, user1, user3);
			const openDm = await createDmChannel(harness, user1.token, user2.userId);
			await sendChannelMessage(harness, user1.token, openDm.id, 'open dm searchable message');
			await markUserDmChannelsAsIndexed(harness, user1.token);
			const result = await createBuilder<MessageSearchResponse>(harness, user1.token)
				.post('/search/messages')
				.body({
					content: 'open dm searchable',
					scope: 'open_dms',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
			expect(result.messages[0]?.channel_id).toBe(openDm.id);
		});
		test('scope: all combines guilds and DMs correctly', async () => {
			const user = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, user, friend);
			const guild = await createGuild(harness, user.token, 'Test Guild');
			const dmChannel = await createDmChannel(harness, user.token, friend.userId);
			await sendChannelMessage(harness, user.token, guild.system_channel_id!, 'guild combined searchable');
			await sendChannelMessage(harness, user.token, dmChannel.id, 'dm combined searchable');
			await markGuildChannelsAsIndexed(harness, user.token, guild.id);
			await markChannelAsIndexed(harness, dmChannel.id);
			const result = await createBuilder<MessageSearchResponse>(harness, user.token)
				.post('/search/messages')
				.body({
					content: 'combined searchable',
					scope: 'all',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const guildMessages = result.messages.filter((m) => m.channel_id === guild.system_channel_id);
			const dmMessages = result.messages.filter((m) => m.channel_id === dmChannel.id);
			expect(guildMessages.length).toBeGreaterThan(0);
			expect(dmMessages.length).toBeGreaterThan(0);
		});
		test('scope: open_dms_and_all_guilds works correctly', async () => {
			const user = await createTestAccount(harness);
			const friend = await createTestAccount(harness);
			await createFriendship(harness, user, friend);
			const guild = await createGuild(harness, user.token, 'Test Guild');
			const openDm = await createDmChannel(harness, user.token, friend.userId);
			await sendChannelMessage(harness, user.token, guild.system_channel_id!, 'guild open_dms_guilds searchable');
			await sendChannelMessage(harness, user.token, openDm.id, 'dm open_dms_guilds searchable');
			await markGuildChannelsAsIndexed(harness, user.token, guild.id);
			await markChannelAsIndexed(harness, openDm.id);
			const result = await createBuilder<MessageSearchResponse>(harness, user.token)
				.post('/search/messages')
				.body({
					content: 'open_dms_guilds searchable',
					scope: 'open_dms_and_all_guilds',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const guildMessages = result.messages.filter((m) => m.channel_id === guild.system_channel_id);
			const dmMessages = result.messages.filter((m) => m.channel_id === openDm.id);
			expect(guildMessages.length).toBeGreaterThan(0);
			expect(dmMessages.length).toBeGreaterThan(0);
		});
		test('scope: all_guilds excludes inaccessible channels within accessible guilds', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const systemChannelId = guild.system_channel_id!;
			const privateChannel = await createChannel(harness, owner.token, guild.id, 'private', ChannelTypes.GUILD_TEXT);
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				await createPermissionOverwrite(harness, owner.token, privateChannel.id, everyoneRole.id, {
					type: 0,
					allow: '0',
					deny: (Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY).toString(),
				});
			}
			await sendChannelMessage(harness, owner.token, systemChannelId, 'public all_guilds searchable');
			await sendChannelMessage(harness, owner.token, privateChannel.id, 'private all_guilds searchable');
			const invite = await createChannelInvite(harness, owner.token, systemChannelId);
			await acceptInvite(harness, member.token, invite.code);
			await markGuildChannelsAsIndexed(harness, owner.token, guild.id);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'all_guilds searchable',
					scope: 'all_guilds',
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			const publicMessages = result.messages.filter((m) => m.channel_id === systemChannelId);
			const privateMessages = result.messages.filter((m) => m.channel_id === privateChannel.id);
			expect(publicMessages.length).toBeGreaterThan(0);
			expect(privateMessages.length).toBe(0);
		});
	});
	describe('Role-based channel access', () => {
		test('role with VIEW_CHANNEL but without READ_MESSAGE_HISTORY cannot search', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			const viewOnlyRole = await createRole(harness, owner.token, guild.id, {
				name: 'View Only',
				permissions: Permissions.VIEW_CHANNEL.toString(),
			});
			await sendChannelMessage(harness, owner.token, channelId, 'restricted history content');
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				const permissions = BigInt(everyoneRole.permissions);
				const newPermissions = permissions & ~(Permissions.VIEW_CHANNEL | Permissions.READ_MESSAGE_HISTORY);
				await updateRole(harness, owner.token, guild.id, everyoneRole.id, {
					permissions: newPermissions.toString(),
				});
			}
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			await addMemberRole(harness, owner.token, guild.id, member.userId, viewOnlyRole.id);
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'restricted history',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages).toEqual([]);
		});
		test('channel permission overwrite can grant search access', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			await sendChannelMessage(harness, owner.token, channelId, 'overwrite granted content');
			const roles = await getRoles(harness, owner.token, guild.id);
			const everyoneRole = roles.find((r) => r.id === guild.id);
			if (everyoneRole) {
				const permissions = BigInt(everyoneRole.permissions);
				const newPermissions = permissions & ~Permissions.READ_MESSAGE_HISTORY;
				await updateRole(harness, owner.token, guild.id, everyoneRole.id, {
					permissions: newPermissions.toString(),
				});
			}
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			await createPermissionOverwrite(harness, owner.token, channelId, member.userId, {
				type: 1,
				allow: Permissions.READ_MESSAGE_HISTORY.toString(),
				deny: '0',
			});
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'overwrite granted',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages.length).toBeGreaterThan(0);
		});
		test('channel permission overwrite can deny search access despite role permission', async () => {
			const owner = await createTestAccount(harness);
			const member = await createTestAccount(harness);
			const guild = await createGuild(harness, owner.token, 'Test Guild');
			const channelId = guild.system_channel_id!;
			await sendChannelMessage(harness, owner.token, channelId, 'overwrite denied content');
			const invite = await createChannelInvite(harness, owner.token, channelId);
			await acceptInvite(harness, member.token, invite.code);
			await createPermissionOverwrite(harness, owner.token, channelId, member.userId, {
				type: 1,
				allow: '0',
				deny: Permissions.READ_MESSAGE_HISTORY.toString(),
			});
			await markChannelAsIndexed(harness, channelId);
			const result = await createBuilder<MessageSearchResponse>(harness, member.token)
				.post('/search/messages')
				.body({
					content: 'overwrite denied',
					context_channel_id: channelId,
				})
				.expect(HTTP_STATUS.OK)
				.execute();
			if (!isSearchResult(result)) {
				expect.fail('Expected search result but got indexing response');
			}
			expect(result.messages).toEqual([]);
		});
	});
});
