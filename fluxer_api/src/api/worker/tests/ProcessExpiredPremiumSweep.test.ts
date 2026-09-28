// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
import {getCachedInstancePremiumMode, setCachedInstancePremiumMode} from '@app/api/limits/InstancePremiumModeCache';
import {NoopLogger} from '@app/api/test/mocks/NoopLogger';
import type {UserRepository} from '@app/api/user/repositories/UserRepository';
import processExpiredPremiumSweep from '@app/api/worker/tasks/ProcessExpiredPremiumSweep';
import {clearWorkerDependencies, setWorkerDependenciesForTest} from '@app/api/worker/WorkerContext';
import type {WorkerTaskHelpers} from '@pkgs/worker/src/contracts/WorkerTask';
import {afterEach, describe, expect, test} from 'vitest';

function createHarness() {
	const scanLimits: Array<number> = [];
	const userRepository = {
		async scanAllUsersPage(limit: number): Promise<{users: []; pageState: null}> {
			scanLimits.push(limit);
			return {users: [], pageState: null};
		},
	} as unknown as UserRepository;
	setWorkerDependenciesForTest({userRepository});
	return {scanLimits};
}

function createHelpers(): WorkerTaskHelpers {
	return {
		logger: new NoopLogger(),
		jobId: 1n,
		addJob: async () => 0n,
		reportProgress: async () => {},
		shouldCancel: async () => false,
		setContextLink: async () => {},
	};
}

async function withSelfHosted(
	selfHosted: boolean,
	callback: () => Promise<void>,
	premiumMode: 'mirror' | 'everyone' = 'everyone',
): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	const originalPremiumMode = getCachedInstancePremiumMode();
	try {
		config.instance.selfHosted = selfHosted;
		setCachedInstancePremiumMode(premiumMode);
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
		setCachedInstancePremiumMode(originalPremiumMode);
	}
}

describe('processExpiredPremiumSweep', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('scans no users on a self-hosted instance where everyone is premium', async () => {
		const harness = createHarness();

		await withSelfHosted(true, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([]);
	});

	test('scans users on a self-hosted instance in mirror mode', async () => {
		const harness = createHarness();

		await withSelfHosted(
			true,
			async () => {
				await processExpiredPremiumSweep({}, createHelpers());
			},
			'mirror',
		);

		expect(harness.scanLimits).toEqual([100]);
	});

	test('scans users on a hosted instance', async () => {
		const harness = createHarness();

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([100]);
	});
});
