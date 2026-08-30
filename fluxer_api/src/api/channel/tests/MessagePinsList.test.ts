// SPDX-License-Identifier: AGPL-3.0-or-later

import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelPinsResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {extractTimestamp} from '@fluxer/snowflake/src/SnowflakeUtils';
import {afterAll, afterEach, beforeAll, beforeEach, describe, expect, it, vi} from 'vitest';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {createBuilder} from '../../test/TestRequestBuilder';
import {createMessageResponseDataService} from '../services/message/MessageResponseDataService';
import {
	createPermissionOverwrite,
	pinMessage,
	sendChannelMessage,
	setupTestGuildWithMembers,
	updateGuild,
} from './ChannelTestUtils';

describe('Channel pins listing', () => {
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

	it('builds a full page of pins with a single batched request', async () => {
		const {owner, systemChannel} = await setupTestGuildWithMembers(harness, 0);
		const pinnedIds: Array<string> = [];
		for (let index = 0; index < 6; index++) {
			const message = await sendChannelMessage(harness, owner.token, systemChannel.id, `pinned ${index}`);
			await pinMessage(harness, owner.token, systemChannel.id, message.id);
			pinnedIds.push(message.id);
		}
		const responseDataService = createMessageResponseDataService();
		const buildMessages = vi.spyOn(responseDataService, 'buildMessages');
		const getMessage = vi.spyOn(responseDataService, 'getMessage');
		const before = encodeURIComponent(new Date(Date.now() + 60_000).toISOString());

		const pins = await createBuilder<ChannelPinsResponse>(harness, owner.token)
			.get(`/channels/${systemChannel.id}/messages/pins?limit=5&before=${before}`)
			.execute();

		expect(pins.has_more).toBe(true);
		expect(pins.items).toHaveLength(5);
		for (const item of pins.items) {
			expect(pinnedIds).toContain(item.message.id);
			expect(item.message.pinned).toBe(true);
			expect(item.pinned_at).toEqual(expect.any(String));
		}
		expect(getMessage).not.toHaveBeenCalled();
		expect(buildMessages).toHaveBeenCalledTimes(1);
		expect(buildMessages.mock.calls[0]![0].messages).toHaveLength(5);
	});

	it('applies message history access filtering to every pin in the batch', async () => {
		const {owner, members, guild, systemChannel} = await setupTestGuildWithMembers(harness, 1);
		const member = members[0]!;
		const oldMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'old pin');
		const newMessage = await sendChannelMessage(harness, owner.token, systemChannel.id, 'new pin');
		await pinMessage(harness, owner.token, systemChannel.id, oldMessage.id);
		await pinMessage(harness, owner.token, systemChannel.id, newMessage.id);
		const cutoff = new Date(extractTimestamp(newMessage.id)).toISOString();
		await updateGuild(harness, owner.token, guild.id, {message_history_cutoff: cutoff});
		await createPermissionOverwrite(harness, owner.token, systemChannel.id, member.userId, {
			type: 1,
			allow: Permissions.VIEW_CHANNEL.toString(),
			deny: Permissions.READ_MESSAGE_HISTORY.toString(),
		});
		const responseDataService = createMessageResponseDataService();
		const buildMessages = vi.spyOn(responseDataService, 'buildMessages');

		const pins = await createBuilder<ChannelPinsResponse>(harness, member.token)
			.get(`/channels/${systemChannel.id}/messages/pins`)
			.execute();

		expect(pins.items.map((item) => item.message.id)).toEqual([newMessage.id]);
		expect(buildMessages).toHaveBeenCalledTimes(1);
		const batch = buildMessages.mock.calls[0]![0];
		expect(batch.messages.map((message) => message.id.toString())).toEqual([newMessage.id]);
		expect(batch.access.canReadMessageHistory).toBe(false);
		expect(batch.access.messageHistoryCutoff).toBe(cutoff);
	});
});
