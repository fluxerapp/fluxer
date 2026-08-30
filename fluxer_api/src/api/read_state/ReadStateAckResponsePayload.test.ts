// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '../guild/tests/GuildTestUtils';
import {sendMessage} from '../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../test/ApiTestHarness';
import {HTTP_STATUS} from '../test/TestConstants';
import {createBuilder} from '../test/TestRequestBuilder';

interface AckResponse {
	read_states: Array<{
		id: string;
		mention_count: number;
		last_message_id: string | null;
		version: string;
	}>;
}

describe('POST /read-states/ack response payload', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('returns the authoritative read states without a protobuf bundle', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Read State Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'read-state-channel');
		const message = await sendMessage(harness, account.token, channel.id, 'hello');
		const response = await createBuilder<AckResponse>(harness, account.token)
			.post('/read-states/ack')
			.body({read_states: [{channel_id: channel.id, message_id: message.id, manual: true, mention_count: 0}]})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.read_states).toHaveLength(1);
		expect(response.read_states[0]?.id).toBe(channel.id);
		expect(response.read_states[0]?.last_message_id).toBe(message.id);
		expect(typeof response.read_states[0]?.version).toBe('string');
		expect(Object.hasOwn(response, 'read_state_proto')).toBe(false);
	});
});
