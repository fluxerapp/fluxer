// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {
	createFriendship,
	createGroupDmChannel,
	getChannel,
	removeRecipientFromGroupDm,
} from '@app/api/channel/tests/ChannelTestUtils';
import {DisabledLiveKitService} from '@app/api/infrastructure/DisabledLiveKitService';
import {InMemoryVoiceRoomStore} from '@app/api/infrastructure/InMemoryVoiceRoomStore';
import {ensureSessionStarted} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {NoopGatewayService} from '@app/api/test/NoopGatewayService';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('Group DM recipient removal call teardown', () => {
	let harness: ApiTestHarness;
	beforeAll(async () => {
		harness = await createApiTestHarness();
	});
	beforeEach(async () => {
		await harness.reset();
	});
	afterEach(() => {
		vi.restoreAllMocks();
	});
	afterAll(async () => {
		await harness?.shutdown();
	});

	async function setupGroupDm() {
		const owner = await createTestAccount(harness);
		const member = await createTestAccount(harness);
		const other = await createTestAccount(harness);
		await ensureSessionStarted(harness, owner.token);
		await ensureSessionStarted(harness, member.token);
		await ensureSessionStarted(harness, other.token);
		await createFriendship(harness, owner, member);
		await createFriendship(harness, owner, other);
		const groupDm = await createGroupDmChannel(harness, owner.token, [member.userId, other.userId]);
		return {owner, member, other, groupDm};
	}

	it('disconnects the removed recipient from the call and the voice room', async () => {
		const {owner, member, other, groupDm} = await setupGroupDm();
		vi.spyOn(NoopGatewayService.prototype, 'getVoiceStatesForChannel').mockResolvedValue({
			voiceStates: [
				{connectionId: 'member-conn', userId: member.userId, channelId: groupDm.id},
				{connectionId: 'other-conn', userId: other.userId, channelId: groupDm.id},
			],
		});
		vi.spyOn(InMemoryVoiceRoomStore.prototype, 'getPinnedRoomServer').mockResolvedValue({
			regionId: 'region-a',
			serverId: 'server-a',
			endpoint: 'wss://voice.invalid',
		});
		const disconnectFromCall = vi.spyOn(NoopGatewayService.prototype, 'disconnectVoiceUserIfInChannel');
		const disconnectParticipant = vi.spyOn(DisabledLiveKitService.prototype, 'disconnectParticipant');

		await removeRecipientFromGroupDm(harness, owner.token, groupDm.id, member.userId);

		expect(disconnectFromCall).toHaveBeenCalledTimes(1);
		const callParams = disconnectFromCall.mock.calls[0]![0];
		expect(callParams.guildId).toBeUndefined();
		expect(callParams.channelId.toString()).toBe(groupDm.id);
		expect(callParams.userId.toString()).toBe(member.userId);
		expect(disconnectParticipant).toHaveBeenCalledTimes(1);
		const participantParams = disconnectParticipant.mock.calls[0]![0];
		expect(participantParams.userId.toString()).toBe(member.userId);
		expect(participantParams.channelId.toString()).toBe(groupDm.id);
		expect(participantParams.connectionId).toBe('member-conn');
		expect(participantParams.regionId).toBe('region-a');
		expect(participantParams.serverId).toBe('server-a');
	});

	it('disconnects a recipient who leaves the group DM themselves', async () => {
		const {member, groupDm} = await setupGroupDm();
		const disconnectFromCall = vi.spyOn(NoopGatewayService.prototype, 'disconnectVoiceUserIfInChannel');

		await removeRecipientFromGroupDm(harness, member.token, groupDm.id, member.userId);

		expect(disconnectFromCall).toHaveBeenCalledTimes(1);
		expect(disconnectFromCall.mock.calls[0]![0].userId.toString()).toBe(member.userId);
	});

	it('still removes the recipient when the call teardown fails', async () => {
		const {owner, member, groupDm} = await setupGroupDm();
		vi.spyOn(NoopGatewayService.prototype, 'disconnectVoiceUserIfInChannel').mockRejectedValue(
			new Error('gateway unavailable'),
		);

		await removeRecipientFromGroupDm(harness, owner.token, groupDm.id, member.userId);

		const channel = await getChannel(harness, owner.token, groupDm.id);
		expect(channel.recipients?.map((recipient) => recipient.id)).not.toContain(member.userId);
	});
});
