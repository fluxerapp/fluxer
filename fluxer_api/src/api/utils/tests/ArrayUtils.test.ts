import {describe, expect, it} from 'vitest';
import {chunkArray} from '../ArrayUtils';

describe('chunkArray', () => {
	it.each([
		{size: 1, expected: [[1], [2], [3], [4], [5]]},
		{size: 2, expected: [[1, 2], [3, 4], [5]]},
		{size: 5, expected: [[1, 2, 3, 4, 5]]},
		{size: 10, expected: [[1, 2, 3, 4, 5]]},
	])('partitions input into chunks of at most $size', ({size, expected}) => {
		expect(chunkArray([1, 2, 3, 4, 5], size)).toEqual(expected);
	});

	it('preserves the input and returns independent chunk arrays', () => {
		const items = Object.freeze([1, 2, 3]);
		const chunks = chunkArray(items, 2);
		chunks[0]!.push(4);
		expect(items).toEqual([1, 2, 3]);
		expect(chunks).toEqual([[1, 2, 4], [3]]);
	});

	it('returns no chunks for empty input', () => {
		expect(chunkArray([], 2)).toEqual([]);
	});

	it.each([
		0,
		-1,
		1.5,
		Number.NaN,
		Number.POSITIVE_INFINITY,
		Number.MAX_SAFE_INTEGER + 1,
	])('rejects invalid chunk size %j even for empty input', (size) => {
		expect(() => chunkArray([1], size)).toThrow('Chunk size must be a positive safe integer');
		expect(() => chunkArray([], size)).toThrow('Chunk size must be a positive safe integer');
	});
});
