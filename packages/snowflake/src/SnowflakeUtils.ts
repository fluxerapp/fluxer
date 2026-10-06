// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSnowflake,
	FLUXER_EPOCH,
	MAX_SEQUENCE,
	MAX_WORKER_ID,
	TIMESTAMP_SHIFT,
} from '@fluxer/snowflake/src/Snowflake';

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

function assertValidWorkerId(workerId: number): void {
	if (!Number.isInteger(workerId)) {
		throw new Error(`Worker ID must be between 0 and ${MAX_WORKER_ID}`);
	}
	const workerIdBigInt = BigInt(workerId);
	if (workerIdBigInt < 0n || workerIdBigInt > MAX_WORKER_ID) {
		throw new Error(`Worker ID must be between 0 and ${MAX_WORKER_ID}`);
	}
}

function assertValidSequenceValue(sequence: number): void {
	if (!Number.isInteger(sequence)) {
		throw new Error(`Snowflake sequence number overflow: ${sequence}`);
	}
	if (sequence < 0 || sequence > Number(MAX_SEQUENCE)) {
		throw new Error(`Snowflake sequence number overflow: ${sequence}`);
	}
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

export function fromTimestampWithSequence(timestamp: number, sequence: SnowflakeSequence): string {
	const clampedTimestamp = toClampedTimestamp(timestamp);
	return createSnowflake({
		timestamp: clampedTimestamp,
		sequence: sequence.next(),
	}).toString();
}

export function fromTimestampWithSequenceBigInt(timestamp: number, sequence: SnowflakeSequence, workerId = 0): bigint {
	assertValidWorkerId(workerId);
	const clampedTimestamp = toClampedTimestamp(timestamp);
	return createSnowflake({
		timestamp: clampedTimestamp,
		workerId,
		sequence: sequence.next(),
	});
}

export function atPreviousMillisecond(snowflake: string): string {
	return fromTimestamp(extractTimestamp(snowflake) - 1);
}

export function atPreviousMillisecondBigInt(snowflake: bigint): bigint {
	return fromTimestampBigInt(extractTimestampBigInt(snowflake) - 1);
}

export function atNextMillisecond(snowflake: string): string {
	return fromTimestamp(extractTimestamp(snowflake) + 1);
}

export function atNextMillisecondBigInt(snowflake: bigint): bigint {
	return fromTimestampBigInt(extractTimestampBigInt(snowflake) + 1);
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

export function compareBigInt(snowflake1: bigint | null, snowflake2: bigint | null): number {
	if (snowflake1 === snowflake2) {
		return 0;
	}
	if (snowflake2 == null) {
		return 1;
	}
	if (snowflake1 == null) {
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

export function sortBySnowflakeDescBigInt<
	T extends {
		id: bigint;
	},
>(items: ReadonlyArray<T>): Array<T> {
	return [...items].sort((a, b) => compareBigInt(b.id, a.id));
}

export function age(snowflake: string): number {
	const timestamp = extractTimestamp(snowflake);
	if (Number.isNaN(timestamp)) {
		return 0;
	}
	return Date.now() - timestamp;
}

export function ageBigInt(snowflake: bigint): number {
	const timestamp = extractTimestampBigInt(snowflake);
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

export function extractTimestampFromSnowflakeAsDateBigInt(snowflake: bigint): Date {
	return new Date(extractTimestampBigInt(snowflake));
}

export interface SnowflakeSequenceOptions {
	initialValue?: number;
}

export class SnowflakeSequence {
	private seq: number;

	constructor(options: SnowflakeSequenceOptions = {}) {
		const initialValue = options.initialValue ?? 0;
		assertValidSequenceValue(initialValue);
		this.seq = initialValue;
	}

	next(): number {
		if (this.seq > Number(MAX_SEQUENCE)) {
			throw new Error(`Snowflake sequence number overflow: ${this.seq}`);
		}
		const current = this.seq;
		this.seq += 1;
		return current;
	}

	willOverflowNext(): boolean {
		return this.seq > Number(MAX_SEQUENCE);
	}

	peek(): number {
		return this.seq;
	}

	reset(nextValue = 0): void {
		assertValidSequenceValue(nextValue);
		this.seq = nextValue;
	}
}
