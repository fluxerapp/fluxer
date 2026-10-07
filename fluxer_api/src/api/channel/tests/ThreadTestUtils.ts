// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	clearChannelThreadsTaintCacheForTesting,
	syncChannelThreadsConfig,
} from '@app/api/experiment/ChannelThreadsGate';
import {getInstanceConfigRepository} from '@app/api/middleware/ServiceSingletons';
import type {ApiTestHarness} from '@app/api/test/ApiTestHarness';
import {createBuilder, type TestRequestBuilder} from '@app/api/test/TestRequestBuilder';
import {
	type ChannelThreadsConfig,
	ChannelThreadsConfigSchema,
} from '@fluxer/schema/src/domains/admin/ChannelThreadsSchemas';

export const THREADS_FEATURE_HEADER = 'X-Fluxer-Features';
export const THREADS_FEATURE = 'channel_threads';

export const ALL_THREADS_ACTIVE: Partial<ChannelThreadsConfig> = {
	enabled: true,
	guild_basis_points: 10000,
	user_basis_points: 10000,
};

export async function setChannelThreadsConfig(patch: Partial<ChannelThreadsConfig>): Promise<void> {
	await getInstanceConfigRepository().updateChannelThreadsConfig((current) =>
		ChannelThreadsConfigSchema.parse({
			...patch,
			ever_enabled: current.ever_enabled || patch.enabled === true,
			config_version: current.config_version + 1,
		}),
	);
	clearChannelThreadsTaintCacheForTesting();
}

export function resetChannelThreadsConfig(): void {
	syncChannelThreadsConfig(null, (raw) => ChannelThreadsConfigSchema.parse(raw ? JSON.parse(raw) : {}));
	clearChannelThreadsTaintCacheForTesting();
}

export function threadsRequest<T = unknown>(
	harness: ApiTestHarness,
	token: string,
	options: {capable?: boolean} = {},
): TestRequestBuilder<T> {
	const builder = createBuilder<T>(harness, token);
	return options.capable === false ? builder : builder.header(THREADS_FEATURE_HEADER, THREADS_FEATURE);
}
