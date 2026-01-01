/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {describe, expect, it, vi} from 'vitest';
import {
	BucketScanDirection,
	type BucketScanTraceEvent,
	BucketScanTraceKind,
	scanBucketsWithIndex,
} from './BucketScanEngine';

interface FakeRow {
	id: bigint;
}

function makeIndexBuckets(
	allBuckets: Array<number>,
	direction: BucketScanDirection,
): (query: {minBucket: number; maxBucket: number; limit: number}) => Promise<Array<number>> {
	return async (query) => {
		const filtered = allBuckets.filter((b) => b >= query.minBucket && b <= query.maxBucket);
		filtered.sort((a, b) => (direction === BucketScanDirection.Desc ? b - a : a - b));
		return filtered.slice(0, query.limit);
	};
}

describe('scanBucketsWithIndex', () => {
	it('scans buckets in DESC order and stops when limit is satisfied', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([5, 4, 3, 2, 1], BucketScanDirection.Desc));
		const fetchRowsForBucket = vi.fn(async (bucket: number) => ({rows: [{id: BigInt(bucket)}], unbounded: true}));

		const trace: Array<BucketScanTraceEvent> = [];
		const result = await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
				trace: (event) => trace.push(event),
			},
			{
				minBucket: 1,
				maxBucket: 5,
				limit: 3,
				direction: BucketScanDirection.Desc,
				indexPageSize: 200,
			},
		);

		expect(listBucketsFromIndex).toHaveBeenCalledTimes(1);
		expect(fetchRowsForBucket.mock.calls.map((c) => c[0])).toEqual([5, 4, 3]);
		expect(result.rows.map((r) => r.id)).toEqual([5n, 4n, 3n]);

		const processed = trace.filter((e) => e.kind === BucketScanTraceKind.ProcessBucket).map((e) => e.bucket);
		expect(processed).toEqual([5, 4, 3]);
	});

	it('falls back to the numeric scan when the index is missing buckets', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([5, 3, 1], BucketScanDirection.Desc));
		const fetchRowsForBucket = vi.fn(async (bucket: number) => ({rows: [{id: BigInt(bucket)}], unbounded: true}));

		const trace: Array<BucketScanTraceEvent> = [];
		const result = await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
				trace: (event) => trace.push(event),
			},
			{
				minBucket: 1,
				maxBucket: 5,
				limit: 5,
				direction: BucketScanDirection.Desc,
				indexPageSize: 200,
			},
		);

		expect(result.rows.map((r) => r.id)).toEqual([5n, 3n, 1n, 4n, 2n]);

		const processed = trace.filter((e) => e.kind === BucketScanTraceKind.ProcessBucket).map((e) => e.bucket);
		expect(processed).toEqual([5, 3, 1, 4, 2]);
	});

	it('honors stopAfterBucket in DESC scans', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([5, 4, 3, 2, 1], BucketScanDirection.Desc));
		const fetchRowsForBucket = vi.fn(async (bucket: number) => ({rows: [{id: BigInt(bucket)}], unbounded: true}));

		const trace: Array<BucketScanTraceEvent> = [];
		const result = await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
				trace: (event) => trace.push(event),
			},
			{
				minBucket: 1,
				maxBucket: 5,
				limit: 100,
				direction: BucketScanDirection.Desc,
				indexPageSize: 200,
				stopAfterBucket: 4,
			},
		);

		expect(result.rows.map((r) => r.id)).toEqual([5n, 4n]);

		const processed = trace.filter((e) => e.kind === BucketScanTraceKind.ProcessBucket).map((e) => e.bucket);
		expect(processed).toEqual([5, 4]);
	});

	it('scans buckets in ASC order and returns closest rows first', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([400, 401], BucketScanDirection.Asc));
		const fetchRowsForBucket = vi.fn(async (bucket: number, limit: number) => {
			const rowsByBucket = new Map<number, Array<FakeRow>>([
				[400, [{id: 51n}, {id: 52n}, {id: 53n}]],
				[401, [{id: 100n}, {id: 101n}]],
			]);
			return {rows: (rowsByBucket.get(bucket) ?? []).slice(0, limit), unbounded: true};
		});

		const result = await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
			},
			{
				minBucket: 400,
				maxBucket: 401,
				limit: 4,
				direction: BucketScanDirection.Asc,
				indexPageSize: 200,
			},
		);

		expect(result.rows.map((r) => r.id)).toEqual([51n, 52n, 53n, 100n]);
		expect(fetchRowsForBucket.mock.calls.map((c) => c[0])).toEqual([400, 401]);
	});

	it('deduplicates rows using getRowId', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([5, 4], BucketScanDirection.Desc));
		const fetchRowsForBucket = vi.fn(async (bucket: number) => {
			const rowsByBucket = new Map<number, Array<FakeRow>>([
				[5, [{id: 1n}, {id: 2n}]],
				[4, [{id: 2n}, {id: 3n}]],
			]);
			return {rows: rowsByBucket.get(bucket) ?? [], unbounded: true};
		});

		const result = await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
			},
			{
				minBucket: 4,
				maxBucket: 5,
				limit: 10,
				direction: BucketScanDirection.Desc,
				indexPageSize: 200,
			},
		);

		expect(result.rows.map((r) => r.id)).toEqual([1n, 2n, 3n]);
	});

	it('marks empty buckets only when the query was unbounded', async () => {
		const listBucketsFromIndex = vi.fn(makeIndexBuckets([3, 2, 1], BucketScanDirection.Desc));

		const emptied: Array<number> = [];
		const fetchRowsForBucket = vi.fn(async (bucket: number) => {
			if (bucket === 3) return {rows: [], unbounded: false};
			if (bucket === 2) return {rows: [], unbounded: true};
			return {rows: [{id: 1n}], unbounded: true};
		});

		await scanBucketsWithIndex<FakeRow>(
			{
				listBucketsFromIndex,
				fetchRowsForBucket,
				getRowId: (row) => row.id,
				onEmptyUnboundedBucket: async (bucket) => {
					emptied.push(bucket);
				},
			},
			{
				minBucket: 1,
				maxBucket: 3,
				limit: 1,
				direction: BucketScanDirection.Desc,
				indexPageSize: 200,
			},
		);

		expect(emptied).toEqual([2]);
	});
});
