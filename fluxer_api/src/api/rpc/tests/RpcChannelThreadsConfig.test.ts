// SPDX-License-Identifier: AGPL-3.0-or-later

import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	type ChannelThreadsConfig,
	DEFAULT_CHANNEL_THREADS_CONFIG,
	everyoneChannelThreadsConfig,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';
import {afterAll, beforeAll, beforeEach, describe, expect, test} from 'vitest';

interface ChannelThreadsConfigRpcResponse {
	type: 'get_channel_threads_config';
	data: {config: ChannelThreadsConfig};
}

describe('RpcService get_channel_threads_config', () => {
	let harness: ApiTestHarness;

	beforeAll(async () => {
		harness = await createApiTestHarness();
	});

	beforeEach(async () => {
		await harness.reset();
	});

	afterAll(async () => {
		await harness.shutdown();
	});

	const fetchConfig = () =>
		createBuilder<ChannelThreadsConfigRpcResponse>(harness, '')
			.post('/test/rpc-session-init')
			.body({type: 'get_channel_threads_config'})
			.expect(HTTP_STATUS.OK)
			.execute();

	test('serves the everyone config at version zero while no row is stored', async () => {
		expect(await fetchConfig()).toEqual({
			type: 'get_channel_threads_config',
			data: {config: everyoneChannelThreadsConfig(0)},
		});
	});

	test('serves the everyone config at the stored version for a disabled partial row', async () => {
		await getInstanceConfigRepository().setConfig(
			'channel_threads_config',
			JSON.stringify({
				...DEFAULT_CHANNEL_THREADS_CONFIG,
				enabled: false,
				ever_enabled: true,
				config_version: 12,
				guild_basis_points: 2500,
				enabled_guild_ids: ['123'],
				excluded_user_ids: ['456'],
			}),
		);

		expect((await fetchConfig()).data.config).toEqual(everyoneChannelThreadsConfig(12));
	});
});
