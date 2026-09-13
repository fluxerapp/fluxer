// SPDX-License-Identifier: AGPL-3.0-or-later

import {RpcTimingRecorder} from '@app/api/rpc/RpcTimings';
import {afterEach, describe, expect, test} from 'vitest';

const ORIGINAL_ENV = {
	POD_NAME: process.env.POD_NAME,
	NODE_NAME: process.env.NODE_NAME,
	HOSTNAME: process.env.HOSTNAME,
};

function restoreEnv(): void {
	for (const [key, value] of Object.entries(ORIGINAL_ENV)) {
		if (value === undefined) {
			delete process.env[key];
		} else {
			process.env[key] = value;
		}
	}
}

describe('RpcTimingRecorder', () => {
	afterEach(() => {
		restoreEnv();
	});

	test('includes API pod metadata', () => {
		process.env.POD_NAME = 'api-pod-1';
		process.env.NODE_NAME = 'worker-node-7';
		process.env.HOSTNAME = 'ignored-hostname';

		const timings = new RpcTimingRecorder().finalize();

		expect(timings.pod_name).toBe('api-pod-1');
		expect('node_name' in timings).toBe(false);
		expect('role' in timings).toBe(false);
		expect(timings.nodes).toBeUndefined();
	});
});
