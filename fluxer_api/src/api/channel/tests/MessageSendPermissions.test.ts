// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGE_LENGTH_PREMIUM} from '@fluxer/constants/src/LimitConstants';
import {UnknownGuildError} from '@fluxer/errors/src/domains/guild/UnknownGuildError';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {authorizeBot, createTestBotAccount} from '../../bot/tests/BotTestUtils';
import {ensureSessionStarted} from '../../message/tests/MessageTestUtils';
import {getGatewayService} from '../../middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS, TEST_TIMEOUTS, wait} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';
import {ChannelDataRepository} from '../repositories/ChannelDataRepository';
import {
	createDmChannel,
	createFriendship,
	createPermissionOverwrite,
	setupTestGuildWithMembers,
} from './ChannelTestUtils';

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
		const getGuildAuthContext = vi.spyOn(gatewayService, 'getGuildAuthContext');
		const getGuildMember = vi.spyOn(gatewayService, 'getGuildMember');
		const getUserPermissions = vi.spyOn(gatewayService, 'getUserPermissions');

		try {
			const sentMessage = await createBuilder<MessageResponse>(harness, member.token)
				.post(`/channels/${systemChannel.id}/messages`)
				.body({content: 'authenticate me once'})
				.execute();
			const sendCounts = {
				authContext: getGuildAuthContext.mock.calls.length,
				guildMember: getGuildMember.mock.calls.length,
				userPermissions: getUserPermissions.mock.calls.length,
			};
			getGuildAuthContext.mockClear();
			getGuildMember.mockClear();
			getUserPermissions.mockClear();
			await createBuilder<MessageResponse>(harness, member.token)
				.patch(`/channels/${systemChannel.id}/messages/${sentMessage.id}`)
				.body({content: 'authenticate me once again'})
				.execute();
			const editCounts = {
				authContext: getGuildAuthContext.mock.calls.length,
				guildMember: getGuildMember.mock.calls.length,
				userPermissions: getUserPermissions.mock.calls.length,
			};

			expect(sendCounts).toEqual({authContext: 1, guildMember: 1, userPermissions: 1});
			expect(editCounts).toEqual({authContext: 1, guildMember: 1, userPermissions: 1});
		} finally {
			getGuildAuthContext.mockRestore();
			getGuildMember.mockRestore();
			getUserPermissions.mockRestore();
		}
	});

	it('resolves the DM channel once for routing and once for authentication when sending', async () => {
		const sender = await createTestAccount(harness);
		const recipient = await createTestAccount(harness);
		await ensureSessionStarted(harness, sender.token);
		await createFriendship(harness, sender, recipient);
		const channel = await createDmChannel(harness, sender.token, recipient.userId);
		const findUnique = vi.spyOn(ChannelDataRepository.prototype, 'findUnique');

		try {
			await createBuilder<MessageResponse>(harness, sender.token)
				.post(`/channels/${channel.id}/messages`)
				.body({content: 'no redundant reads'})
				.expect(HTTP_STATUS.OK)
				.execute();
			const channelReads = findUnique.mock.calls.filter((call) => call[0].toString() === channel.id).length;

			expect(channelReads).toBe(2);
		} finally {
			findUnique.mockRestore();
		}
	});

	it('returns ACCESS_DENIED not UNKNOWN_GUILD when the Gateway has dropped a guild whose record survives', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		await ensureSessionStarted(harness, owner.token);
		const gatewayService = getGatewayService();

		for (const memberSettlesFirst of [true, false]) {
			const getGuildAuthContext = vi.spyOn(gatewayService, 'getGuildAuthContext').mockImplementation(async () => {
				if (memberSettlesFirst) await wait(TEST_TIMEOUTS.IMMEDIATE);
				throw new UnknownGuildError();
			});
			const getGuildMember = vi.spyOn(gatewayService, 'getGuildMember').mockImplementation(async () => {
				if (!memberSettlesFirst) await wait(TEST_TIMEOUTS.IMMEDIATE);
				throw new UnknownGuildError();
			});

			try {
				await createBuilder(harness, owner.token)
					.post(`/channels/${systemChannel.id}/messages`)
					.body({content: 'the gateway forgot this guild'})
					.expect(HTTP_STATUS.FORBIDDEN, 'ACCESS_DENIED')
					.execute();
			} finally {
				getGuildAuthContext.mockRestore();
				getGuildMember.mockRestore();
			}
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
