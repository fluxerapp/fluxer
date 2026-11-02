// SPDX-License-Identifier: AGPL-3.0-or-later

import {withOperatorPassLock} from '@app/api/operator_pass/OperatorPassLocks';
import type {ICacheService} from '@pkgs/cache/src/ICacheService';
import {expect, test, vi} from 'vitest';

test('the purchase lock is renewed while slow work runs and the work stops once the lock is lost', async () => {
	vi.useFakeTimers();
	try {
		let held = true;
		const extendLock = vi.fn(async () => held);
		const cache = {
			acquireLock: async () => 'token',
			extendLock,
			releaseLock: vi.fn(async () => true),
		} as unknown as ICacheService;
		let release: () => void = () => {};
		const run = withOperatorPassLock(
			cache,
			'operator_pass:purchase_lock:cs_test_lock',
			async (lease) => {
				await new Promise<void>((resolve) => {
					release = resolve;
				});
				await lease.assertHeld();
				return 'done';
			},
			() => new Error('busy'),
		);
		await vi.advanceTimersByTimeAsync(160_000);
		expect(extendLock.mock.calls.length).toBeGreaterThanOrEqual(5);
		held = false;
		await vi.advanceTimersByTimeAsync(30_000);
		release();
		await expect(run).rejects.toThrow('busy');
		expect(cache.releaseLock).toHaveBeenCalledWith('operator_pass:purchase_lock:cs_test_lock', 'token');
	} finally {
		vi.useRealTimers();
	}
});
