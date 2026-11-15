// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createChannelID, createMessageID, createUserID} from '@app/api/BrandedTypes';
import {MessageMentionService} from '@app/api/channel/services/message/MessageMentionService';
import {blockUser, setupTestGuildWithMembers} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {RecordingWorkerService} from '@app/api/test/RecordingWorkerService';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {UserRepository} from '@app/api/user/repositories/UserRepository';
import {MessageTypes} from '@fluxer/constants/src/ChannelConstants';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

describe('Message mentions of users who blocked the author', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	it('shows reply mentions the same way and leaves blockers out of the fanout', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 3);
		const [first, second, third] = members;
		assert(first && second && third, 'Expected three guild members');
		await blockUser(harness, second, owner.userId);
		await blockUser(harness, owner, third.userId);
		const original = await createBuilder<MessageResponse>(harness, second.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({content: 'hello'})
			.execute();
		const reply = await createBuilder<MessageResponse>(harness, owner.token)
			.post(`/channels/${systemChannel.id}/messages`)
			.body({
				content: `<@${first.userId}> <@${third.userId}>`,
				message_reference: {message_id: original.id},
				allowed_mentions: {replied_user: true},
			})
			.execute();
		expect(new Set(reply.mentions.map((user) => user.id))).toEqual(
			new Set([first.userId, second.userId, third.userId]),
		);
		const worker = new RecordingWorkerService();
		const mentionService = new MessageMentionService(
			new UserRepository(),
			null as never,
			null as never,
			worker,
			null as never,
		);
		await mentionService.handleMentionTasks({
			guildId: null,
			message: {
				id: createMessageID(BigInt(reply.id)),
				channelId: createChannelID(BigInt(reply.channel_id)),
				type: MessageTypes.REPLY,
				reference: null,
				mentionEveryone: false,
				mentionedUserIds: new Set(reply.mentions.map((user) => createUserID(BigInt(user.id)))),
				mentionedRoleIds: new Set(),
			} as never,
			authorId: createUserID(BigInt(owner.userId)),
		});
		const [job] = worker.byTask('handleMentions');
		expect(new Set(job?.payload.mentionUserIds as Array<string>)).toEqual(new Set([first.userId, third.userId]));
	});
});
