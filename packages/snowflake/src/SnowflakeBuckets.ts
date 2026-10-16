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

export function makeBucket(snowflake: bigint | null): number {
	const timestamp = getRelativeTimestampForBucket(snowflake);
	return Number(timestamp / SNOWFLAKE_BUCKET_SIZE_MS);
}
