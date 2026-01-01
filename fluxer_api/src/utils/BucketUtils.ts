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

import {FLUXER_EPOCH} from '~/Constants';

const BUCKET_SIZE = BigInt(1000 * 60 * 60 * 24 * 10);

export const makeBucket = (snowflake: bigint | null): number => {
	let timestamp: bigint;
	if (snowflake == null) {
		timestamp = BigInt(Date.now() - FLUXER_EPOCH);
	} else {
		timestamp = snowflake >> 22n;
	}
	return Math.floor(Number(timestamp / BUCKET_SIZE));
};

export const makeBuckets = (startId: bigint | null, endId: bigint | null = null): Array<number> => {
	const start = makeBucket(startId);
	const end = makeBucket(endId);
	const result: Array<number> = [];
	for (let i = start; i <= end; i++) {
		result.push(i);
	}
	return result;
};
