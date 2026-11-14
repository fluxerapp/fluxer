// SPDX-License-Identifier: AGPL-3.0-or-later

export enum BucketScanDirection {
	Asc = 'asc',
	Desc = 'desc',
}

interface BucketScanIndexQuery {
	minBucket: number;
	maxBucket: number;
	limit: number;
}

interface BucketScanBucketFetchResult<Row> {
	rows: Array<Row>;
	unbounded: boolean;
}

interface BucketScanDeps<Row> {
	listBucketsFromIndex: (query: BucketScanIndexQuery) => Promise<Array<number>>;
	fetchRowsForBucket: (bucket: number, limit: number) => Promise<BucketScanBucketFetchResult<Row>>;
	getRowId: (row: Row) => bigint;
	onEmptyUnboundedBucket?: (bucket: number) => Promise<void>;
	onBucketHasRows?: (bucket: number) => Promise<void>;
}

interface BucketScanOptions {
	minBucket: number;
	maxBucket: number;
	limit: number;
	direction: BucketScanDirection;
	indexPageSize: number;
}

interface BucketScanResult<Row> {
	rows: Array<Row>;
}

export async function scanBucketsWithIndex<Row>(
	deps: BucketScanDeps<Row>,
	opts: BucketScanOptions,
): Promise<BucketScanResult<Row>> {
	if (opts.limit <= 0) {
		return {rows: []};
	}
	let remaining = opts.limit;
	const out: Array<Row> = [];
	const seenRowIds = new Set<bigint>();
	const processedBuckets = new Set<number>();
	const processBucket = async (bucket: number) => {
		const {rows, unbounded} = await deps.fetchRowsForBucket(bucket, remaining);
		if (rows.length === 0) {
			if (unbounded && deps.onEmptyUnboundedBucket) {
				await deps.onEmptyUnboundedBucket(bucket);
			}
			return;
		}
		if (deps.onBucketHasRows) {
			await deps.onBucketHasRows(bucket);
		}
		for (const row of rows) {
			if (remaining <= 0) break;
			const rowId = deps.getRowId(row);
			if (seenRowIds.has(rowId)) continue;
			seenRowIds.add(rowId);
			out.push(row);
			remaining--;
		}
	};
	if (opts.direction === BucketScanDirection.Desc) {
		let cursorMax: number | null = opts.maxBucket;
		while (remaining > 0 && cursorMax !== null && cursorMax >= opts.minBucket) {
			const buckets = await deps.listBucketsFromIndex({
				minBucket: opts.minBucket,
				maxBucket: cursorMax,
				limit: opts.indexPageSize,
			});
			if (buckets.length === 0) break;
			for (const bucket of buckets) {
				if (remaining <= 0) break;
				if (bucket < opts.minBucket || bucket > opts.maxBucket) continue;
				if (processedBuckets.has(bucket)) continue;
				processedBuckets.add(bucket);
				await processBucket(bucket);
			}
			const last = buckets[buckets.length - 1];
			const nextCursor = last - 1;
			cursorMax = nextCursor >= opts.minBucket ? nextCursor : null;
		}
		if (remaining > 0) {
			for (let bucket = opts.maxBucket; remaining > 0 && bucket >= opts.minBucket; bucket--) {
				if (processedBuckets.has(bucket)) continue;
				processedBuckets.add(bucket);
				await processBucket(bucket);
			}
		}
	} else {
		let cursorMin: number | null = opts.minBucket;
		while (remaining > 0 && cursorMin !== null && cursorMin <= opts.maxBucket) {
			const buckets = await deps.listBucketsFromIndex({
				minBucket: cursorMin,
				maxBucket: opts.maxBucket,
				limit: opts.indexPageSize,
			});
			if (buckets.length === 0) break;
			for (const bucket of buckets) {
				if (remaining <= 0) break;
				if (bucket < opts.minBucket || bucket > opts.maxBucket) continue;
				if (processedBuckets.has(bucket)) continue;
				processedBuckets.add(bucket);
				await processBucket(bucket);
			}
			const last = buckets[buckets.length - 1];
			const nextCursor = last + 1;
			cursorMin = nextCursor <= opts.maxBucket ? nextCursor : null;
		}
		if (remaining > 0) {
			for (let bucket = opts.minBucket; remaining > 0 && bucket <= opts.maxBucket; bucket++) {
				if (processedBuckets.has(bucket)) continue;
				processedBuckets.add(bucket);
				await processBucket(bucket);
			}
		}
	}
	return {rows: out};
}
