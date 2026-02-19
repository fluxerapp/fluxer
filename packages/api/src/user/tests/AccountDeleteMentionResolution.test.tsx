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
import {
	acceptInvite,
	createChannel,
	createChannelInvite,
	createGuild,
} from '@fluxer/api/src/guild/tests/GuildTestUtils';
import {ensureSessionStarted, sendMessage} from '@fluxer/api/src/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@fluxer/api/src/test/ApiTestHarness';
import {HTTP_STATUS} from '@fluxer/api/src/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@fluxer/api/src/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@fluxer/api/src/user/tests/UserTestUtils';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import type {UserPartialResponse} from '@fluxer/schema/src/domains/user/UserResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Account Delete Mention Resolution', () => {
	let harness: ApiTestHarness;

	beforeEach(async () => {
		harness = await createApiTestHarness();
	});

	afterEach(async () => {
		await harness?.shutdown();
	});

	test('messages mentioning a deleted user remain readable', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		const guild = await createGuild(harness, alice.token, 'Mention Test Guild');
		let channelId = guild.system_channel_id;
		if (!channelId) {
			const channel = await createChannel(harness, alice.token, guild.id, 'general');
			channelId = channel.id;
		}

		const invite = await createChannelInvite(harness, alice.token, channelId);
		await acceptInvite(harness, bob.token, invite.code);

		await ensureSessionStarted(harness, alice.token);
		const mentionMessage = await sendMessage(harness, alice.token, channelId, `Hello <@${bob.userId}>`);
		expect(mentionMessage.mentions).toBeDefined();
		expect(mentionMessage.mentions!.length).toBe(1);

		await deleteAccount(harness, bob.token, bob.password);
		const past = new Date();
		past.setMinutes(past.getMinutes() - 1);
		await setPendingDeletionAt(harness, bob.userId, past);
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, bob.userId);

		await createBuilderWithoutAuth(harness).post('/test/cache-clear').expect(HTTP_STATUS.OK).execute();

		const messages = await createBuilder<Array<MessageResponse>>(harness, alice.token)
			.get(`/channels/${channelId}/messages?limit=50`)
			.expect(HTTP_STATUS.OK)
			.execute();

		const mentionMsg = messages.find((m) => m.id === mentionMessage.id);
		expect(mentionMsg).toBeDefined();
		expect(mentionMsg!.mentions).toBeDefined();
		expect(mentionMsg!.mentions!.length).toBe(1);

		const mention = mentionMsg!.mentions![0];
		expect(mention.id).toBe(bob.userId);
		expect(mention.username).toBe('DeletedUser');
		expect(mention.discriminator).toBe('0000');
		expect(mention.global_name).toBe('Deleted User');
		expect(mention.avatar).toBeNull();

		expect(mentionMsg!.author.id).toBe(alice.userId);
		expect(mentionMsg!.author.username).not.toBe('DeletedUser');
	}, 60_000);

	test('message author resolution works for users with DELETED flag', async () => {
		const account = await createTestAccount(harness);

		const guild = await createGuild(harness, account.token, 'Author Test Guild');
		let channelId = guild.system_channel_id;
		if (!channelId) {
			const channel = await createChannel(harness, account.token, guild.id, 'general');
			channelId = channel.id;
		}

		await ensureSessionStarted(harness, account.token);
		const sentMessage = await sendMessage(harness, account.token, channelId, 'Hello world');

		const viewer = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, account.token, channelId);
		await acceptInvite(harness, viewer.token, invite.code);

		await createBuilderWithoutAuth(harness)
			.post(`/test/users/${account.userId}/security-flags`)
			.body({set_flags: ['DELETED']})
			.expect(HTTP_STATUS.OK)
			.execute();

		await createBuilderWithoutAuth(harness).post('/test/cache-clear').expect(HTTP_STATUS.OK).execute();

		const messages = await createBuilder<Array<MessageResponse>>(harness, viewer.token)
			.get(`/channels/${channelId}/messages?limit=50`)
			.expect(HTTP_STATUS.OK)
			.execute();

		const msg = messages.find((m) => m.id === sentMessage.id);
		expect(msg).toBeDefined();
		expect(msg!.author.username).toBe('DeletedUser');
		expect(msg!.author.discriminator).toBe('0000');
		expect(msg!.author.global_name).toBe('Deleted User');
		expect(msg!.author.avatar).toBeNull();
	}, 60_000);

	test('direct user lookup returns deleted user fallback for deleted users', async () => {
		const alice = await createTestAccount(harness);
		const bob = await createTestAccount(harness);

		await deleteAccount(harness, bob.token, bob.password);
		const past = new Date();
		past.setMinutes(past.getMinutes() - 1);
		await setPendingDeletionAt(harness, bob.userId, past);
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, bob.userId);

		const user = await createBuilder<UserPartialResponse>(harness, alice.token)
			.get(`/users/${bob.userId}`)
			.expect(HTTP_STATUS.OK)
			.execute();

		expect(user.id).toBe(bob.userId);
		expect(user.username).toBe('DeletedUser');
		expect(user.discriminator).toBe('0000');
		expect(user.global_name).toBe('Deleted User');
		expect(user.avatar).toBeNull();
	}, 60_000);
});
