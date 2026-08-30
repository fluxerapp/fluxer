// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {authorizeBot, createTestBotAccount} from '../../bot/tests/BotTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {getGatewayService} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createPermissionOverwrite, setupTestGuildWithMembers} from './ChannelTestUtils';

describe('Message send permissions', () => {
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

	it('returns the created message when the sender cannot read history or add reactions', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: (Permissions.READ_MESSAGE_HISTORY | Permissions.ADD_REACTIONS).toString(),
		});
		await ensureSessionStarted(harness, member.token);

		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'no history send'})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(sentMessage.content).toBe('no history send');
		expect(sentMessage.author.id).toBe(member.userId);
	});

	it('rejects sending and editing when VIEW_CHANNEL is denied', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'before the lockout'})
			.execute();
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: '0',
			deny: Permissions.VIEW_CHANNEL.toString(),
		});

		await createBuilder(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'after the lockout'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
		await createBuilder(harness, member.token)
			.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
			.body({content: 'after the lockout'})
			.expect(HTTP_STATUS.FORBIDDEN, 'MISSING_PERMISSIONS')
			.execute();
	});

	it('hides the edited message when the editor cannot read history', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await ensureSessionStarted(harness, member.token);
		const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'history is a luxury'})
			.execute();
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});

		await createBuilder(harness, member.token)
			.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
			.body({content: 'history is still a luxury'})
			.expect(HTTP_STATUS.NOT_FOUND, 'UNKNOWN_MESSAGE')
			.execute();
	});

	it('authenticates the channel once per send and once per edit', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES | Permissions.READ_MESSAGE_HISTORY).toString(),
			deny: '0',
		});
		await ensureSessionStarted(harness, member.token);
		const gatewayService = getGatewayService();
		const getGuildData = vi.spyOn(gatewayService, 'getGuildData');
		const getGuildMember = vi.spyOn(gatewayService, 'getGuildMember');
		const checkPermission = vi.spyOn(gatewayService, 'checkPermission');
		const countViewChannelChecks = () =>
			checkPermission.mock.calls.filter((call) => call[0].permission === Permissions.VIEW_CHANNEL).length;

		try {
			const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
				.post(`/channels/${systemChannel.id}/messages`)
				.body({content: 'authenticate me once'})
				.execute();
			const sendCounts = {
				guildData: getGuildData.mock.calls.length,
				guildMember: getGuildMember.mock.calls.length,
				viewChannel: countViewChannelChecks(),
			};
			getGuildData.mockClear();
			getGuildMember.mockClear();
			checkPermission.mockClear();
			await createBuilder<MessageResponse>(harness, member.token)
				.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
				.body({content: 'authenticate me once again'})
				.execute();
			const editCounts = {
				guildData: getGuildData.mock.calls.length,
				guildMember: getGuildMember.mock.calls.length,
				viewChannel: countViewChannelChecks(),
			};

			expect(sendCounts).toEqual({guildData: 1, guildMember: 1, viewChannel: 1});
			expect(editCounts).toEqual({guildData: 1, guildMember: 1, viewChannel: 1});
		} finally {
			getGuildData.mockRestore();
			getGuildMember.mockRestore();
			checkPermission.mockRestore();
		}
	});

	it('allows bot message content up to 4000 characters', async () => {
		const {owner, guild, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const botAccount = await createTestBotAccount(harness);
		const botPermissions = (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES).toString();
		await authorizeBot(harness, owner.token, botAccount.appId, ['bot'], guild.id, botPermissions);
		const content = 'b'.repeat(MAX_MESSAGE_LENGTH_PREMIUM);

		const sentMessage = await createBuilder<MessageResponse>(harness, `Bot ${botAccount.botToken}`)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content})
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(sentMessage.content).toBe(content);
		expect(sentMessage.author.id).toBe(botAccount.botUserId);
	});
});
