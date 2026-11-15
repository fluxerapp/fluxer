// SPDX-License-Identifier: AGPL-3.0-or-later

import {createTestAccount} from '@app/api/auth/tests/AuthTestUtils';
import {createChannel, createGuild} from '@app/api/guild/tests/GuildTestUtils';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {ChannelTypes} from '@fluxer/constants/src/ChannelConstants';
import {afterEach, beforeEach, describe, expect, test} from 'vitest';

interface RpcVoiceStatesResponse {
	type: 'guild_collection';
	data: {
		voice_states: Array<{connection_id: string; p2p?: boolean}>;
	};
}

describe('RpcService guild voice state p2p flag', () => {
	let harness: ApiTestHarness;
	beforeEach(async () => {
		harness = await createApiTestHarness();
	});
	afterEach(async () => {
		await harness?.shutdown();
	});
	test('keeps p2p on a persisted guild voice state', async () => {
		const account = await createTestAccount(harness);
		const guild = await createGuild(harness, account.token, 'P2P Voice State Guild');
		const channel = await createChannel(harness, account.token, guild.id, 'voice', ChannelTypes.GUILD_VOICE);
		await createBuilder(harness, '')
			.post('/test/rpc-session-init')
			.body({
				type: 'voice_state_upsert',
				guild_id: guild.id,
				voice_state: {
					guild_id: guild.id,
					channel_id: channel.id,
					user_id: account.userId,
					connection_id: 'brisk-forte',
					mute: false,
					deaf: false,
					self_mute: false,
					self_deaf: false,
					p2p: true,
				},
			})
			.expect(HTTP_STATUS.OK)
			.execute();
		const response = await createBuilder<RpcVoiceStatesResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'guild_collection', guild_id: guild.id, collection: 'voice_states'})
			.expect(HTTP_STATUS.OK)
			.execute();
		expect(response.data.voice_states).toMatchObject([{connection_id: 'brisk-forte', p2p: true}]);
	});
});
