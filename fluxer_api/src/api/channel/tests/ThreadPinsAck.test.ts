// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {getReadStateRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

interface Setup {
	owner: TestAccount;
	member: TestAccount;
	outsider: TestAccount;
	threadId: string;
}

describe('thread pins ack', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
		clearChannelThreadsTaintCacheForTesting();
	});

	afterAll(async () => {
		clearChannelThreadsTaintCacheForTesting();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const outsider = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'pins');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'private', type: ChannelTypes.PRIVATE_THREAD})
			.expect(201)
			.execute();
		const message = await threadsRequest<MessageResponse>(harness, owner.token)
			.post(`/channels/${thread.id}/messages`)
			.body({content: 'pin me'})
			.expect(200)
			.execute();
		await threadsRequest(harness, owner.token).put(`/channels/${thread.id}/pins/${message.id}`).expect(204).execute();
		return {owner, member, outsider, threadId: thread.id};
	}

	async function pinAckRow(account: TestAccount, threadId: string) {
		const row = await getReadStateRepository().getReadState(
			createUserID(BigInt(account.userId)),
			createChannelID(BigInt(threadId)),
		);
		return row?.lastPinTimestamp ? row : null;
	}

	it('refuses viewers who cannot see the private thread', async () => {
		const s = await setup();
		await threadsRequest(harness, s.member.token).post(`/channels/${s.threadId}/pins/ack`).expect(403).execute();
		await threadsRequest(harness, s.outsider.token).post(`/channels/${s.threadId}/pins/ack`).expect(403).execute();
		expect(await pinAckRow(s.member, s.threadId)).toBeNull();
		expect(await pinAckRow(s.outsider, s.threadId)).toBeNull();
	});
});
