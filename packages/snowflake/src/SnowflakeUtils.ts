// SPDX-License-Identifier: AGPL-3.0-or-later

import {createSnowflake, FLUXER_EPOCH, TIMESTAMP_SHIFT} from '@fluxer/snowflake/src/Snowflake';

const FLUXER_EPOCH_NUMBER = Number(FLUXER_EPOCH);

function extractTimestampWithEpoch(snowflake: bigint, epoch: bigint): number {
	return Number((snowflake >> TIMESTAMP_SHIFT) + epoch);
}

function toClampedTimestamp(timestamp: number): number {
	if (timestamp <= FLUXER_EPOCH_NUMBER) {
		return FLUXER_EPOCH_NUMBER;
	}
	return timestamp;
}

export function extractTimestamp(snowflake: string): number {
	return extractTimestampFromSnowflake(snowflake);
}

export function extractTimestampBigInt(snowflake: bigint): number {
	return extractTimestampWithEpoch(snowflake, FLUXER_EPOCH);
}

export function fromTimestamp(timestamp: number): string {
	return fromTimestampBigInt(timestamp).toString();
}

export function fromTimestampBigInt(timestamp: number): bigint {
	const clampedTimestamp = toClampedTimestamp(timestamp);
	if (clampedTimestamp === FLUXER_EPOCH_NUMBER) {
		return 0n;
	}
	return createSnowflake({timestamp: clampedTimestamp});
}

export function atPreviousMillisecond(snowflake: string): string {
	return fromTimestamp(extractTimestamp(snowflake) - 1);
}

export function compare(snowflake1: string | null, snowflake2: string | null): number {
	if (snowflake1 === snowflake2) {
		return 0;
	}
	if (snowflake2 == null) {
		return 1;
	}
	if (snowflake1 == null) {
		return -1;
	}
	if (snowflake1.length > snowflake2.length) {
		return 1;
	}
	if (snowflake1.length < snowflake2.length) {
		return -1;
	}
	return snowflake1 > snowflake2 ? 1 : -1;
}

export function isProbablyAValidSnowflake(value: string | null | undefined): boolean {
	if (value == null) {
		return false;
	}
	try {
		const num = BigInt(value);
		return num > 0n;
	} catch (_error) {
		return false;
	}
}

export function sortBySnowflakeDesc<
	T extends {
		id: string;
	},
>(items: ReadonlyArray<T>): Array<T> {
	return [...items].sort((a, b) => compare(b.id, a.id));
}

export function age(snowflake: string): number {
	const timestamp = extractTimestamp(snowflake);
	if (Number.isNaN(timestamp)) {
		return 0;
	}
	return Date.now() - timestamp;
}

export function extractTimestampFromSnowflake(snowflake: string, epoch?: string | bigint): number {
	try {
		const epochBigInt = epoch != null ? (typeof epoch === 'string' ? BigInt(epoch) : epoch) : FLUXER_EPOCH;
		return extractTimestampWithEpoch(BigInt(snowflake), epochBigInt);
	} catch (_error) {
		return Number.NaN;
	}
}

export function extractTimestampFromSnowflakeAsDate(snowflake: string, epoch?: string | bigint): Date {
	const timestamp = extractTimestampFromSnowflake(snowflake, epoch);
	if (Number.isNaN(timestamp)) {
		return new Date();
	}
	return new Date(timestamp);
}
