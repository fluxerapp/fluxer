// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannelID, createUserID} from '@app/api/BrandedTypes';
import {ChannelRepository} from '@app/api/channel/ChannelRepository';
import {ThreadRepository} from '@app/api/channel/repositories/ThreadRepository';
import {acceptInvite, createChannel, createChannelInvite, createGuild} from '@app/api/channel/tests/ChannelTestUtils';
import {
	ALL_THREADS_ACTIVE,
	resetChannelThreadsConfig,
	setChannelThreadsConfig,
	threadsRequest,
} from '@app/api/channel/tests/ThreadTestUtils';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {ThreadMemberFlags} from '@fluxer/constants/src/ThreadConstants';
import type {ThreadChannelResponse} from '@fluxer/schema/src/domains/channel/ThreadRequestSchemas';
import type {ThreadMemberResponse} from '@fluxer/schema/src/domains/channel/ThreadSchemas';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

const repository = new ChannelRepository();

describe('thread member settings', () => {
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

	async function setup() {
		await setChannelThreadsConfig(ALL_THREADS_ACTIVE);
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
			.expect(201)
			.execute();
		return {owner, member, thread};
	}

	function settings(token: string, threadId: string, body: Record<string, unknown>, status = 200, code?: string) {
		return threadsRequest<ThreadMemberResponse>(harness, token)
			.patch(`/channels/${threadId}/thread-members/@me/settings`)
			.body(body)
			.expect(status, code)
			.execute();
	}

	it('is an unregistered path outside the experiment', async () => {
		const s = await setup();
		await threadsRequest(harness, s.owner.token, {capable: false})
			.patch(`/channels/${s.thread.id}/thread-members/@me/settings`)
			.body({flags: ThreadMemberFlags.ALL_MESSAGES})
			.expect(404)
			.execute();
		await setChannelThreadsConfig({enabled: false});
		await threadsRequest(harness, s.owner.token)
			.patch(`/channels/${s.thread.id}/thread-members/@me/settings`)
			.body({flags: ThreadMemberFlags.ALL_MESSAGES})
			.expect(404)
			.execute();
	});

	it('updates flags and mute, keeps HAS_INTERACTED and tells only the member', async () => {
		const s = await setup();
		const threadId = createChannelID(BigInt(s.thread.id));
		const ownerId = createUserID(BigInt(s.owner.userId));
		const joined = await repository.threads.getMember(threadId, ownerId);
		await repository.threads.updateMemberSettings(joined!, {flags: ThreadMemberFlags.HAS_INTERACTED});
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		const updated = await settings(s.owner.token, s.thread.id, {
			flags: ThreadMemberFlags.ONLY_MENTIONS,
			muted: true,
			mute_config: {selected_time_window: 3600, end_time: '2030-01-01T00:00:00.000Z'},
		});
		expect(updated).toMatchObject({
			id: s.thread.id,
			user_id: s.owner.userId,
			flags: ThreadMemberFlags.HAS_INTERACTED | ThreadMemberFlags.ONLY_MENTIONS,
			muted: true,
			mute_config: {selected_time_window: 3600, end_time: '2030-01-01T00:00:00.000Z'},
		});
		const events = dispatch.mock.calls.map(([params]) => params);
		expect(events).toEqual([
			expect.objectContaining({
				event: 'THREAD_MEMBER_UPDATE',
				data: expect.objectContaining({user_id: s.owner.userId, guild_id: s.thread.guild_id}),
			}),
		]);
		const stored = await repository.threads.getMember(threadId, ownerId);
		expect(stored?.muted).toBe(true);
	});

	it('answers 204 without an event when nothing changes', async () => {
		const s = await setup();
		await settings(s.owner.token, s.thread.id, {flags: ThreadMemberFlags.NO_MESSAGES});
		const dispatch = vi.spyOn(NoopGatewayService.prototype, 'dispatchGuild');
		await threadsRequest(harness, s.owner.token)
			.patch(`/channels/${s.thread.id}/thread-members/@me/settings`)
			.body({flags: ThreadMemberFlags.NO_MESSAGES})
			.expect(204)
			.execute();
		expect(dispatch).not.toHaveBeenCalled();
	});

	it('rejects HAS_INTERACTED and more than one notification level', async () => {
		const s = await setup();
		await settings(
			s.owner.token,
			s.thread.id,
			{flags: ThreadMemberFlags.HAS_INTERACTED},
			400,
			APIErrorCodes.INVALID_THREAD_NOTIFICATION_SETTINGS,
		);
		await settings(
			s.owner.token,
			s.thread.id,
			{flags: ThreadMemberFlags.ALL_MESSAGES | ThreadMemberFlags.NO_MESSAGES},
			400,
			APIErrorCodes.INVALID_THREAD_NOTIFICATION_SETTINGS,
		);
	});

	it('retries a settings write that lost the member race', async () => {
		const s = await setup();
		const write = vi.spyOn(ThreadRepository.prototype, 'updateMemberSettings').mockResolvedValueOnce(null);
		const updated = await settings(s.owner.token, s.thread.id, {muted: true});
		expect(updated.muted).toBe(true);
		expect(write).toHaveBeenCalledTimes(2);
		const stored = await repository.threads.getMember(
			createChannelID(BigInt(s.thread.id)),
			createUserID(BigInt(s.owner.userId)),
		);
		expect(stored?.muted).toBe(true);
	});

	it('rejects a settings write computed from stale member flags', async () => {
		const s = await setup();
		const threadId = createChannelID(BigInt(s.thread.id));
		const ownerId = createUserID(BigInt(s.owner.userId));
		const joined = await repository.threads.getMember(threadId, ownerId);
		const stale = await repository.threads.updateMemberSettings(joined!, {flags: 0});
		expect(
			await repository.threads.updateMemberSettings(stale!, {flags: ThreadMemberFlags.HAS_INTERACTED}),
		).not.toBeNull();
		expect(await repository.threads.updateMemberSettings(stale!, {flags: ThreadMemberFlags.ALL_MESSAGES})).toBeNull();
		expect((await repository.threads.getMember(threadId, ownerId))?.flags).toBe(ThreadMemberFlags.HAS_INTERACTED);
		const updated = await settings(s.owner.token, s.thread.id, {flags: ThreadMemberFlags.ALL_MESSAGES});
		expect(updated.flags).toBe(ThreadMemberFlags.HAS_INTERACTED | ThreadMemberFlags.ALL_MESSAGES);
	});

	it('leaves a mute config it was not asked to change to a concurrent writer', async () => {
		const s = await setup();
		const threadId = createChannelID(BigInt(s.thread.id));
		const ownerId = createUserID(BigInt(s.owner.userId));
		const stale = await repository.threads.getMember(threadId, ownerId);
		const muteConfig = {end_time: new Date('2030-01-01T00:00:00.000Z'), selected_time_window: 3600};
		expect(await repository.threads.updateMemberSettings(stale!, {muteConfig})).not.toBeNull();
		const write = vi.spyOn(ThreadRepository.prototype, 'updateMemberSettings');
		vi.spyOn(ThreadRepository.prototype, 'getMember').mockResolvedValueOnce(stale);
		await settings(s.owner.token, s.thread.id, {muted: true});
		expect(write.mock.calls.map(([, patch]) => patch)).toEqual([{flags: stale!.flags, muted: true}]);
		const stored = await repository.threads.getMember(threadId, ownerId);
		expect(stored?.muted).toBe(true);
		expect(stored?.muteConfig?.toMuteConfig()).toEqual(muteConfig);
	});

	it('requires membership', async () => {
		const s = await setup();
		await settings(s.member.token, s.thread.id, {muted: true}, 404, APIErrorCodes.UNKNOWN_THREAD_MEMBER);
	});
});
