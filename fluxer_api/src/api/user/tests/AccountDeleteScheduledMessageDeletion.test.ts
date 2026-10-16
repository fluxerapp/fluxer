// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {ensureSessionStarted, sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder, createBuilderWithoutAuth} from '@app/api/test/TestRequestBuilder';
import {
	deleteAccount,
	setPendingDeletionAt,
	triggerDeletionWorker,
	waitForDeletionCompletion,
} from '@app/api/user/tests/UserTestUtils';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

describe('Account deletion with a scheduled message deletion', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('deletes the scheduled messages instead of anonymizing them', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Scheduled Deletion Guild');
		let channelId = guild.system_channel_id;
		if (!channelId) {
			const channel = await createChannel(harness, account.token, guild.id, 'general');
			channelId = channel.id;
		}
		await ensureSessionStarted(harness, account.token);
		for (let i = 0; i < 3; i++) {
			await sendMessage(harness, account.token, channelId, `Scheduled ${i + 1}`);
		}
		const newOwner = await createTestAccount(harness);
		const invite = await createChannelInvite(harness, account.token, channelId);
		await acceptInvite(harness, newOwner.token, invite.code);
		await createBuilder(harness, account.token)
			.post(`/guilds/${guild.id}/transfer-ownership`)
			.body({new_owner_id: newOwner.userId, password: account.password})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder<void>(harness, account.token)
			.post('/users/@me/messages/delete')
			.body({password: account.password})
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await deleteAccount(harness, account.token, account.password);
		await setPendingDeletionAt(harness, account.userId, new Date(Date.now() - 60_000));
		await triggerDeletionWorker(harness);
		await waitForDeletionCompletion(harness, account.userId);
		const messages = await createBuilder<Array<MessageResponse>>(harness, newOwner.token)
			.get(`/channels/${channelId}/messages?limit=100`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(messages.filter((message) => message.content.startsWith('Scheduled '))).toEqual([]);
		const countJson = await createBuilderWithoutAuth<{count: number}>(harness)
			.get(`/test/users/${account.userId}/messages/count`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(countJson.count).toBe(0);
	}, 60000);
});
