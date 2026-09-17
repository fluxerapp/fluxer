import {mapWithConcurrency} from '@app/api/utils/ConcurrencyUtils';
import {describe, expect, it} from 'vitest';

describe('mapWithConcurrency', () => {
	it('limits in-flight work and preserves input order when later items finish first', async () => {
		const items = ['a', 'b', 'c', 'd'];
		const gates = items.map(() => Promise.withResolvers<number>());
		const thirdStarted = Promise.withResolvers<void>();
		const fourthStarted = Promise.withResolvers<void>();
		const started: Array<[string, number]> = [];
		let active = 0;
		let peakActive = 0;
		const result = mapWithConcurrency(items, 2, async (item, index) => {
			started.push([item, index]);
			active++;
			peakActive = Math.max(peakActive, active);
			if (index === 2) thirdStarted.resolve();
			if (index === 3) fourthStarted.resolve();
			try {
				return await gates[index]!.promise;
			} finally {
				active--;
			}
		});

		expect(started).toEqual([
			['a', 0],
			['b', 1],
		]);
		gates[1]!.resolve(20);
		await thirdStarted.promise;
		expect(started).toEqual([
			['a', 0],
			['b', 1],
			['c', 2],
		]);
		expect(active).toBe(2);
		gates[2]!.resolve(30);
		await fourthStarted.promise;
		expect(started).toEqual([
			['a', 0],
			['b', 1],
			['c', 2],
			['d', 3],
		]);
		gates[3]!.resolve(40);
		gates[0]!.resolve(10);

		await expect(result).resolves.toEqual([10, 20, 30, 40]);
		expect(peakActive).toBe(2);
		expect(active).toBe(0);
	});

	it.each([1, 2, 10])('maps every item exactly once with concurrency %i', async (concurrency) => {
		const indexes: Array<number> = [];
		const result = await mapWithConcurrency([10, 20, 30], concurrency, async (item, index) => {
			indexes.push(index);
			return item + index;
		});
		expect(indexes).toEqual([0, 1, 2]);
		expect(result).toEqual([10, 21, 32]);
	});

	it('returns an empty array without invoking the mapper', async () => {
		await expect(
			mapWithConcurrency([], 2, () => {
				throw new Error('An empty input must not invoke the mapper');
			}),
		).resolves.toEqual([]);
	});

	it.each([0, -1, 1.5, Number.NaN, Number.POSITIVE_INFINITY, Number.MAX_SAFE_INTEGER + 1])(
		'rejects invalid concurrency %j before scheduling work',
		async (concurrency) => {
			let calls = 0;
			const mapper = async () => {
				calls++;
				return 1;
			};
			await expect(mapWithConcurrency([1], concurrency, mapper)).rejects.toThrow(
				'Concurrency must be a positive safe integer',
			);
			await expect(mapWithConcurrency([], concurrency, mapper)).rejects.toThrow(
				'Concurrency must be a positive safe integer',
			);
			expect(calls).toBe(0);
		},
	);

	it('waits for in-flight work before propagating a mapper rejection', async () => {
		const first = Promise.withResolvers<number>();
		const second = Promise.withResolvers<number>();
		const failure = new Error('Mapper failed');
		const completed: Array<unknown> = [];
		const secondCompleted = second.promise.then((value) => completed.push(value));
		const result = mapWithConcurrency([first.promise, second.promise], 2, (promise) => promise);
		const rejected = result.catch((error) => completed.push(error));
		first.reject(failure);
		await new Promise<void>((resolve) => setImmediate(resolve));
		second.resolve(2);
		await Promise.all([rejected, secondCompleted]);
		expect(completed).toEqual([2, failure]);
		expect(completed[1]).toBe(failure);
	});

	it('rejects synchronous mapper failures without starting queued serial work', async () => {
		const failure = new Error('Mapper failed synchronously');
		const started: Array<number> = [];
		await expect(
			mapWithConcurrency([1, 2], 1, (item) => {
				started.push(item);
				throw failure;
			}),
		).rejects.toBe(failure);
		expect(started).toEqual([1]);
	});
});
