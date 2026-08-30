// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {Config} from '../../Config';
import {createGuild} from '../../guild/tests/GuildTestUtils';
import {pinMessage, sendMessage} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

describe('Channel Pins Response Shape', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		Config.dev.validateResponses = true;
		await harness.shutdown();
	});
	test('omits referenced_message and reactions from pinned messages when response validation is disabled', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Pins Shape Guild');
		const channelId = guild.system_channel_id!;
		const target = await sendMessage(harness, account.token, channelId, 'pin target');
		const reply = await createBuilder<{id: string}>(harness, account.token)
			.post(`/channels/${channelId}/messages`)
			.body({
				content: 'pin reply',
				message_reference: {channel_id: channelId, guild_id: guild.id, message_id: target.id},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, account.token)
			.put(`/channels/${channelId}/messages/${reply.id}/reactions/${encodeURIComponent('👍')}/@me`)
			.body(null)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		await pinMessage(harness, account.token, channelId, reply.id);
		Config.dev.validateResponses = false;
		const pins = await createBuilder<{items: Array<{message: Record<string, unknown>}>}>(harness, account.token)
			.get(`/channels/${channelId}/messages/pins`)
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(pins.items).toHaveLength(1);
		expect(pins.items[0]!.message).not.toHaveProperty('referenced_message');
		expect(pins.items[0]!.message).not.toHaveProperty('reactions');
	});
});
