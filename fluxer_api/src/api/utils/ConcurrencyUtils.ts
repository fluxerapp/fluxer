// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

export async function awaitAll(tasks: ReadonlyArray<Promise<unknown>>, failureMessage: string): Promise<void> {
	const results = await Promise.allSettled(tasks);
	const errors: Array<unknown> = [];
	for (const result of results) {
		if (result.status === 'rejected') errors.push(result.reason);
	}
	throwCompletionErrors(errors, failureMessage);
}

export async function runAllInOrder(
	steps: ReadonlyArray<() => Promise<unknown>>,
	failureMessage: string,
): Promise<void> {
	const errors: Array<unknown> = [];
	for (const step of steps) {
		try {
			await step();
		} catch (error) {
			errors.push(error);
		}
	}
	throwCompletionErrors(errors, failureMessage);
}

function throwCompletionErrors(errors: ReadonlyArray<unknown>, failureMessage: string): void {
	if (errors.length === 1) throw errors[0];
	if (errors.length > 1) throw new AggregateError(errors, failureMessage);
}

export async function mapWithConcurrency<T, TResult>(
	items: ReadonlyArray<T>,
	concurrency: number,
	mapper: (item: T, index: number) => Promise<TResult>,
): Promise<Array<TResult>> {
	assert(Number.isSafeInteger(concurrency) && concurrency > 0, 'Concurrency must be a positive safe integer');
	const results = new Array<TResult>(items.length);
	let nextIndex = 0;
	let failure: {error: unknown} | undefined;
	async function worker(): Promise<void> {
		while (!failure && nextIndex < items.length) {
			const index = nextIndex++;
			try {
				results[index] = await mapper(items[index]!, index);
			} catch (error) {
				failure ??= {error};
			}
		}
	}
	await Promise.all(Array.from({length: Math.min(concurrency, items.length)}, () => worker()));
	if (failure) throw failure.error;
	return results;
}
