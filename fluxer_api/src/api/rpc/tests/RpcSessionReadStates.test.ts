// SPDX-License-Identifier: AGPL-3.0-or-later

import {afterEach, beforeEach, describe, expect, test} from 'vitest';
import {createTestAccount} from '../../auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '../../guild/tests/GuildTestUtils';
import {sendMessage} from '../../message/tests/MessageTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '../../test/ApiTestHarness';
import {HTTP_STATUS} from '../../test/TestConstants';
import {createBuilder} from '../../test/TestRequestBuilder';

interface RpcSessionResponse {
	type: 'session';
	data: {
		read_states: Array<{id: string; last_message_id: string | null}>;
	};
}

describe('RpcService session read states', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('sends read states as JSON only', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'Read State Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'read-state-channel');
		const message = await sendMessage(harness, account.token, channel.id, 'hello');
		await createBuilder(harness, account.token)
			.post('/read-states/ack')
			.body({read_states: [{channel_id: channel.id, message_id: message.id, manual: true, mention_count: 0}]})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await createBuilder<RpcSessionResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'session', token: account.token, version: 1, ip: '127.0.0.1'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.data.read_states.some((readState) => readState.id === channel.id)).toBe(true);
		expect(Object.hasOwn(response.data, 'read_state_proto')).toBe(false);
	});
});
