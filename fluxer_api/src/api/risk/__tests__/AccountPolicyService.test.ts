// SPDX-License-Identifier: AGPL-3.0-or-later

import type {APIConfig} from '@app/api/config/APIConfig';
import {describe, expect, it, vi} from 'vitest';

function createRuntimePolicyConfig(params: {
	nodeEnv: 'development' | 'production';
	selfHosted: boolean;
	testModeEnabled: boolean;
	accountPolicyDsl?: unknown;
}): APIConfig {
	return {
		nodeEnv: params.nodeEnv,
		risk: {
			enabled: true,
			accountPolicyDsl: params.accountPolicyDsl,
		},
		dev: {
			testModeEnabled: params.testModeEnabled,
		},
		instance: {
			selfHosted: params.selfHosted,
		},
	} as APIConfig;
}

async function loadIsolatedAccountPolicyService(config: APIConfig) {
	vi.resetModules();
	const {initializeConfig} = await import('@app/api/Config');
	initializeConfig(config);
	return import('@app/api/risk/AccountPolicyService');
}

describe('AccountPolicyService runtime config', () => {
	it('uses a disabled policy when config is absent in development', async () => {
		const service = await loadIsolatedAccountPolicyService(
			createRuntimePolicyConfig({
				nodeEnv: 'development',
				selfHosted: false,
				testModeEnabled: false,
			}),
		);
		service.setInjectedAccountPolicyEvaluator(undefined);
		expect(service.getAccountPolicyEvaluator().classifyEmailTld('unknown')).toBeNull();
	});

	it('uses a disabled policy when config is absent in self-hosted production', async () => {
		const service = await loadIsolatedAccountPolicyService(
			createRuntimePolicyConfig({
				nodeEnv: 'production',
				selfHosted: true,
				testModeEnabled: false,
			}),
		);
		service.setInjectedAccountPolicyEvaluator(undefined);
		expect(service.getAccountPolicyEvaluator().classifyEmailTld('unknown')).toBeNull();
	});

	it('requires policy config when config is absent in hosted production', async () => {
		const service = await loadIsolatedAccountPolicyService(
			createRuntimePolicyConfig({
				nodeEnv: 'production',
				selfHosted: false,
				testModeEnabled: false,
			}),
		);
		service.setInjectedAccountPolicyEvaluator(undefined);
		expect(() => service.getAccountPolicyEvaluator()).toThrow(
			'FLUXER_ACCOUNT_POLICY_DSL is required in hosted production',
		);
	});
});
