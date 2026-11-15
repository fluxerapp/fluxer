// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';
import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {threadsRequest} from '@app/api/channel/tests/ThreadTestUtils';
import {clearChannelThreadsTaintCacheForTesting} from '@app/api/experiment/ChannelThreadsGate';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

const repository = new ChannelRepository();

describe('thread member settings', () => {
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

	async function setup() {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const guild = await createGuild(harness, owner.token, 'threads');
		const channel = await createChannel(harness, owner.token, guild.id, 'general');
		const invite = await createChannelInvite(harness, owner.token, channel.id);
		await acceptInvite(harness, member.token, invite.code);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		const thread = await threadsRequest<ThreadChannelResponse>(harness, owner.token)
			.post(`/channels/${channel.id}/threads`)
			.body({name: 'topic', type: ChannelTypes.PUBLIC_THREAD})
			.expect(HttpStatus.CREATED)
			.execute();
		return {owner, member, thread};
	}

	function settings(
		token: string,
		threadId: string,
		body: Record<string, unknown>,
		status: number = HttpStatus.OK,
		code?: string,
	) {
		return threadsRequest<ThreadMemberResponse>(harness, token)
			.patch(`/channels/${threadId}/thread-members/@me/settings`)
			.body(body)
			.expect(status, code)
			.execute();
	}

	it('rejects HAS_INTERACTED and more than one notification level', async () => {
		const s = await setup();
		await settings(
			s.owner.token,
			s.thread.id,
			{flags: ThreadMemberFlags.HAS_INTERACTED},
			HttpStatus.BAD_REQUEST,
			APIErrorCodes.INVALID_THREAD_NOTIFICATION_SETTINGS,
		);
		await settings(
			s.owner.token,
			s.thread.id,
			{flags: ThreadMemberFlags.ALL_MESSAGES | ThreadMemberFlags.NO_MESSAGES},
			HttpStatus.BAD_REQUEST,
			APIErrorCodes.INVALID_THREAD_NOTIFICATION_SETTINGS,
		);
	});

	it('rejects a settings write computed from stale member flags', async () => {
		const s = await setup();
		const threadId = createChannelID(BigInt(s.thread.id));
		const ownerId = createUserID(BigInt(s.owner.userId));
		const joined = await repository.threads.getMember(threadId, ownerId);
		assert(joined !== null);
		const stale = await repository.threads.updateMemberSettings(joined, {flags: 0});
		assert(stale !== null);
		expect(
			await repository.threads.updateMemberSettings(stale, {flags: ThreadMemberFlags.HAS_INTERACTED}),
		).not.toBeNull();
		expect(await repository.threads.updateMemberSettings(stale, {flags: ThreadMemberFlags.ALL_MESSAGES})).toBeNull();
		expect((await repository.threads.getMember(threadId, ownerId))?.flags).toBe(ThreadMemberFlags.HAS_INTERACTED);
		const updated = await settings(s.owner.token, s.thread.id, {flags: ThreadMemberFlags.ALL_MESSAGES});
		expect(updated.flags).toBe(ThreadMemberFlags.HAS_INTERACTED | ThreadMemberFlags.ALL_MESSAGES);
	});

	it('requires membership', async () => {
		const s = await setup();
		await settings(
			s.member.token,
			s.thread.id,
			{muted: true},
			HttpStatus.NOT_FOUND,
			APIErrorCodes.UNKNOWN_THREAD_MEMBER,
		);
	});
});
