// SPDX-License-Identifier: AGPL-3.0-or-later

import {getConfig} from '@app/api/Config';
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

async function withSelfHosted(selfHosted: boolean, callback: () => Promise<void>): Promise<void> {
	const config = getConfig();
	const originalSelfHosted = config.instance.selfHosted;
	try {
		config.instance.selfHosted = selfHosted;
		await callback();
	} finally {
		config.instance.selfHosted = originalSelfHosted;
	}
}

describe('processExpiredPremiumSweep', () => {
	afterEach(() => {
		clearWorkerDependencies();
	});

	test('scans no users on a self-hosted instance', async () => {
		const harness = createHarness();

		await withSelfHosted(true, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([]);
	});

	test('scans users on a hosted instance', async () => {
		const harness = createHarness();

		await withSelfHosted(false, async () => {
			await processExpiredPremiumSweep({}, createHelpers());
		});

		expect(harness.scanLimits).toEqual([100]);
	});
});
