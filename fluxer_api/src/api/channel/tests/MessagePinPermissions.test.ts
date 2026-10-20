// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createPermissionOverwrite,
	pinMessage,
	sendChannelMessage,
	setupTestGuildWithMembers,
} from '@app/api/channel/tests/ChannelTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {ChannelPinsResponse} from '@fluxer/schema/src/domains/message/MessageResponseSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, it} from 'vitest';

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
});
