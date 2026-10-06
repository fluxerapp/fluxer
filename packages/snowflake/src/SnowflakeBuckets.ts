// SPDX-License-Identifier: AGPL-3.0-or-later

import {FLUXER_EPOCH, TIMESTAMP_SHIFT} from '@fluxer/snowflake/src/Snowflake';
import {ms} from 'itty-time';

export const SNOWFLAKE_BUCKET_SIZE_MS = BigInt(ms('10 days'));

function getRelativeTimestampForBucket(snowflake: bigint | null): bigint {
	if (snowflake == null) {
		return BigInt(Date.now()) - FLUXER_EPOCH;
	}
	return snowflake >> TIMESTAMP_SHIFT;
}

function createBucketRange(startBucket: number, endBucket: number): Array<number> {
	if (endBucket < startBucket) {
		return [];
	}
	const size = endBucket - startBucket + 1;
	const range = new Array<number>(size);
	for (let index = 0; index < size; index += 1) {
		range[index] = startBucket + index;
	}
	return range;
}

export function makeBucket(snowflake: bigint | null): number {
	const timestamp = getRelativeTimestampForBucket(snowflake);
	return Number(timestamp / SNOWFLAKE_BUCKET_SIZE_MS);
}

export function makeBucketString(snowflake: string | null): number {
	if (snowflake == null) {
		return makeBucket(null);
	}
	return makeBucket(BigInt(snowflake));
}

export function makeBuckets(startId: bigint | null, endId: bigint | null = null): Array<number> {
	const startBucket = makeBucket(startId);
	const endBucket = makeBucket(endId);
	return createBucketRange(startBucket, endBucket);
}

export function makeBucketsString(startId: string | null, endId: string | null = null): Array<number> {
	const startBigInt = startId != null ? BigInt(startId) : null;
	const endBigInt = endId != null ? BigInt(endId) : null;
	return makeBuckets(startBigInt, endBigInt);
}
