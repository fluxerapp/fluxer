// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

export async function mapWithConcurrency<T, TResult>(
	items: ReadonlyArray<T>,
	concurrency: number,
	mapper: (item: T, index: number) => Promise<TResult>,
): Promise<Array<TResult>> {
	assert(Number.isSafeInteger(concurrency) && concurrency > 0, 'Concurrency must be a positive safe integer');
	const results = new Array<TResult>(items.length);
	let nextIndex = 0;
	async function worker(): Promise<void> {
		for (;;) {
			const index = nextIndex++;
			if (index >= items.length) return;
			results[index] = await mapper(items[index]!, index);
		}
	}
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, () => worker()));
	return results;
}
