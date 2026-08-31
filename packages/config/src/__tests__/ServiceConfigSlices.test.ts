// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MasterConfig} from '@fluxer/config/src/MasterConfig';
import {
	extractBaseServiceConfig,
	extractBuildInfoConfig,
	extractKVClientConfig,
	extractRateLimit,
} from '@fluxer/config/src/ServiceConfigSlices';
import {describe, expect, test, vi} from 'vitest';

function createMasterStub(overrides: Partial<MasterConfig> = {}): MasterConfig {
	return {
		env: 'development',
		internal: {
			kv: 'redis://127.0.0.1:6379/0',
			kv_provider: 'redis',
			kv_mode: 'standalone',
			kv_cluster_nodes: [],
			kv_cluster_nat_map: {},
			api: 'http://127.0.0.1:8080',
			media_proxy: 'http://localhost:8088/media',
		},
		services: {} as MasterConfig['services'],
		...overrides,
	} as MasterConfig;
}

describe('extractBaseServiceConfig', () => {
	test('returns env from master config', () => {
		const master = createMasterStub({env: 'production'});
		const result = extractBaseServiceConfig(master);
		expect(result).toEqual({
			env: 'production',
		});
	});
});

describe('extractKVClientConfig', () => {
	test('returns kvUrl', () => {
		const master = createMasterStub();
		const result = extractKVClientConfig(master);
		expect(result).toEqual({
			kvUrl: 'redis://127.0.0.1:6379/0',
			kvMode: 'standalone',
			kvClusterNodes: [],
			kvClusterNatMap: {},
		});
	});
	test('throws when internal is missing', () => {
		const master = createMasterStub();
		(master as unknown as Record<string, unknown>).internal = undefined;
		expect(() => extractKVClientConfig(master)).toThrow('internal configuration is required');
	});
});

describe('extractBuildInfoConfig', () => {
	test('returns fallback build metadata and reports the missing environment variables', () => {
		vi.stubEnv('BUILD_VERSION', undefined);
		vi.stubEnv('RELEASE_CHANNEL', undefined);
		const stdout = vi.spyOn(process.stdout, 'write').mockImplementation(() => true);
		try {
			const result = extractBuildInfoConfig();
			expect(result).toEqual({
				releaseChannel: 'stable',
				buildVersion: 'dev',
			});
			expect(stdout).toHaveBeenCalledExactlyOnceWith(
				'[build-metadata] Using fallback values for: BUILD_VERSION, RELEASE_CHANNEL. This indicates missing env vars in CI/production.\n',
			);
		} finally {
			stdout.mockRestore();
			vi.unstubAllEnvs();
		}
	});
});

describe('extractRateLimit', () => {
	test('returns undefined for null input', () => {
		expect(extractRateLimit(null)).toBeUndefined();
	});
	test('returns undefined for undefined input', () => {
		expect(extractRateLimit(undefined)).toBeUndefined();
	});
	test('returns undefined when limit is missing', () => {
		expect(extractRateLimit({window_ms: 60000})).toBeUndefined();
	});
	test('returns undefined when window_ms is missing', () => {
		expect(extractRateLimit({limit: 100})).toBeUndefined();
	});
	test('returns normalised object for valid input', () => {
		const result = extractRateLimit({limit: 100, window_ms: 60000});
		expect(result).toEqual({limit: 100, windowMs: 60000});
	});
});
