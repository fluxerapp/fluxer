// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount, type TestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {getReadStateRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ReadStateFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

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
		resetChannelThreadsConfig();
	});

	afterEach(() => {
		vi.restoreAllMocks();
	});

	afterAll(async () => {
		resetChannelThreadsConfig();
		await harness.shutdown();
	});

	async function setup(): Promise<Setup> {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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

	function pinsAckDispatches(dispatch: {mock: {calls: Array<Array<unknown>>}}) {
		return dispatch.mock.calls.filter(([params]) => (params as {event: string}).event === 'CHANNEL_PINS_ACK');
	}

	it('stamps the ack for a thread member', async () => {
		const s = await setup();
		await threadsRequest(harness, s.owner.token).post(`/channels/${s.threadId}/pins/ack`).expect(204).execute();
		const row = await pinAckRow(s.owner, s.threadId);
		expect(row).not.toBeNull();
		expect(row?.flags).toBe(ReadStateFlags.IS_GUILD_CHANNEL | ReadStateFlags.IS_THREAD);
	});

	it('treats a thread as an unknown channel for non-viewers', async () => {
		const s = await setup();
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		for (const account of [s.member, s.outsider]) {
			await threadsRequest(harness, account.token, {capable: false})
				.post(`/channels/${s.threadId}/pins/ack`)
				.expect(204)
				.execute();
			expect(await pinAckRow(account, s.threadId)).toBeNull();
		}
		await setChannelThreadsConfig({...ALL_THREADS_ACTIVE, enabled: false});
		await threadsRequest(harness, s.owner.token).post(`/channels/${s.threadId}/pins/ack`).expect(204).execute();
		expect(await pinAckRow(s.owner, s.threadId)).toBeNull();
		expect(pinsAckDispatches(dispatch)).toEqual([]);
	});

	it('refuses viewers who cannot see the private thread', async () => {
		const s = await setup();
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchPresence');
		await threadsRequest(harness, s.member.token).post(`/channels/${s.threadId}/pins/ack`).expect(403).execute();
		await threadsRequest(harness, s.outsider.token).post(`/channels/${s.threadId}/pins/ack`).expect(403).execute();
		expect(await pinAckRow(s.member, s.threadId)).toBeNull();
		expect(await pinAckRow(s.outsider, s.threadId)).toBeNull();
		expect(pinsAckDispatches(dispatch)).toEqual([]);
	});
});
