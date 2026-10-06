// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {sendMessage} from '@app/api/message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface AckResponse {
	read_states: Array<{
		id: string;
		last_message_id: string | null;
	}>;
}

describe('DELETE /channels/:channel_id/messages/ack', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('leaves the read state untouched', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Read State Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'read-state-channel');
		const older = await sendMessage(harness, account.token, channel.id, 'older');
		const newer = await sendMessage(harness, account.token, channel.id, 'newer');
		await createBuilder<AckResponse>(harness, account.token)
			.post('/read-states/ack')
			.body({read_states: [{channel_id: channel.id, message_id: newer.id}]})
			.expect(HTTP_STATUS.OK)
			.execute();
		await createBuilder(harness, account.token)
			.delete(`/channels/${channel.id}/messages/ack`)
			.expect(HTTP_STATUS.NO_CONTENT)
			.execute();
		const response = await createBuilder<AckResponse>(harness, account.token)
			.post('/read-states/ack')
			.body({read_states: [{channel_id: channel.id, message_id: older.id}]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.read_states[0]?.last_message_id).toBe(newer.id);
	});
});
