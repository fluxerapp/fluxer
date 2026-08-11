// SPDX-License-Identifier: AGPL-3.0-or-later

import {describe, expect, it, vi} from 'vitest';
import {createJoinableShutdown, stopWorkerRunners} from '../WorkerMain';

describe('worker main shutdown coordination', () => {
	it('makes overlapping shutdown callers join the same cleanup', async () => {
		let finish: (() => void) | undefined;
		const action = vi.fn(
			() =>
				new Promise<void>((resolve) => {
					finish = resolve;
				}),
		);
		const shutdown = createJoinableShutdown(action);
		let secondSettled = false;
		const first = shutdown();
		const second = shutdown().then(() => {
			secondSettled = true;
		});
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(secondSettled).toBe(false);
		finish?.();
		await Promise.all([first, second]);
		expect(action).toHaveBeenCalledOnce();
	});

	it('waits for every runner even when one stop fails', async () => {
		let finishSecond: (() => void) | undefined;
		const firstError = new Error('first stop failed');
		const first = {stop: vi.fn().mockRejectedValue(firstError)};
		const second = {
			stop: vi.fn(
				() =>
					new Promise<void>((resolve) => {
						finishSecond = resolve;
					}),
			),
		};
		let settled = false;
		const stopping = stopWorkerRunners([first, second]).finally(() => {
			settled = true;
		});
		await new Promise<void>((resolve) => setImmediate(resolve));
		expect(settled).toBe(false);
		finishSecond?.();
		await expect(stopping).rejects.toBeInstanceOf(AggregateError);
		expect(first.stop).toHaveBeenCalledOnce();
		expect(second.stop).toHaveBeenCalledOnce();
	});
});
