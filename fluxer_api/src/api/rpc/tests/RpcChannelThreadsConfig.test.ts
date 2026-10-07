// SPDX-License-Identifier: AGPL-3.0-or-later

import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import {type ApiTestHarness, createApiTestHarness} from '@app/api/test/ApiTestHarness';
import {HTTP_STATUS} from '@app/api/test/TestConstants';
import {createBuilder} from '@app/api/test/TestRequestBuilder';
import {
	applyChannelThreadsConfigUpdate,
	type ChannelThreadsConfig,
	DEFAULT_CHANNEL_THREADS_CONFIG,
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

	test('serves the disabled default before any admin write', async () => {
		expect(await fetchConfig()).toEqual({
			type: 'get_channel_threads_config',
			data: {config: DEFAULT_CHANNEL_THREADS_CONFIG},
		});
	});

	test('serves the stored config with its version and sticky ever_enabled', async () => {
		const repository = getInstanceConfigRepository();
		await repository.updateChannelThreadsConfig((current) =>
			applyChannelThreadsConfigUpdate(current, {enabled: true, enabled_guild_ids: ['123']}),
		);
		await repository.updateChannelThreadsConfig((current) =>
			applyChannelThreadsConfigUpdate(current, {enabled: false}),
		);

		const response = await fetchConfig();

		expect(response.data.config).toMatchObject({
			enabled: false,
			ever_enabled: true,
			config_version: 2,
			enabled_guild_ids: ['123'],
		});
	});
});
