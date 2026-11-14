// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createPermissionOverwrite,
	pinMessage,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {getGatewayService} from '@app/api/middleware/ServiceRegistry';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {MessageTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelPinsResponse, MessageResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';

describe('Message pin permissions', () => {
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

	async function listPinnedMessageIds(token: string, channelId: string): Promise<Array<string>> {
		const pins = await createBuilder<ChannelPinsResponse>(harness, token)
			.get(`/channels/${channelId}/messages/pins`)
			.execute();
		return pins.items.map((item) => item.message.id);
	}

	it('allows pinning and unpinning an own message without Read Message History', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.SEND_MESSAGES | Permissions.PIN_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		const message = await sendChannelMessage(harness, member.token, systemChannel.id, 'pin me');

		await pinMessage(harness, member.token, systemChannel.id, message.id);
		expect(await listPinnedMessageIds(owner.token, systemChannel.id)).toEqual([message.id]);

		await createBuilder(harness, member.token)
			.delete(`/channels/${systemChannel.id}/pins/${message.id}`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		expect(await listPinnedMessageIds(owner.token, systemChannel.id)).toEqual([]);
	});

	it('allows pinning source history without Read Message History', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'hidden history');
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.PIN_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});

		await pinMessage(harness, member.token, systemChannel.id, message.id);

		expect(await listPinnedMessageIds(owner.token, systemChannel.id)).toEqual([message.id]);
	});

	it('omits the pinned message body from the pin notice event', async () => {
		const {owner, members, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const message = await sendChannelMessage(harness, owner.token, systemChannel.id, 'hidden history');
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: (Permissions.VIEW_CHANNEL | Permissions.PIN_MESSAGES).toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		const dispatchGuild = vi.spyOn(getGatewayService(), 'dispatchGuild');
		try {
			await pinMessage(harness, member.token, systemChannel.id, message.id);
			const notice = dispatchGuild.mock.calls
				.map(([params]) => params)
				.filter((params) => params.event === 'MESSAGE_CREATE')
				.map((params) => params.data as MessageResponse)
				.find((data) => data.type === MessageTypes.CHANNEL_PINNED_MESSAGE);
			expect(notice?.message_reference?.message_id).toBe(message.id);
			expect(notice).toBeDefined();
			expect('referenced_message' in notice!).toBe(false);
		} finally {
			dispatchGuild.mockRestore();
		}
	});
});
