// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {Throws} from '../../utils/throws.ts';
import {LivekitReasonedError} from '../errors.ts';
import {U16_MAX_SIZE} from './utils.ts';

export enum DataTrackHandleErrorReason {
	Reserved = 0,
	TooLarge = 1,
}

export class DataTrackHandleError<
	Reason extends DataTrackHandleErrorReason = DataTrackHandleErrorReason,
> extends LivekitReasonedError<DataTrackHandleErrorReason> {
	override readonly name = 'DataTrackHandleError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason) {
		super(19, message);
		this.reason = reason;
		this.reasonName = DataTrackHandleErrorReason[reason];
	}

	isReason<R extends DataTrackHandleErrorReason>(reason: R): this is DataTrackHandleError<R> {
		return (this.reason as unknown as R) === reason;
	}

	static tooLarge() {
		return new DataTrackHandleError('Value too large to be a valid track handle', DataTrackHandleErrorReason.TooLarge);
	}

	static reserved(value: number) {
		return new DataTrackHandleError(
			`0x${value.toString(16)} is a reserved value.`,
			DataTrackHandleErrorReason.Reserved,
		);
	}
}

export type DataTrackHandle = number;
export const DataTrackHandle = {
	fromNumber(raw: number): Throws<DataTrackHandle, DataTrackHandleError> {
		if (raw === 0) {
			throw DataTrackHandleError.reserved(raw);
		}
		if (raw > U16_MAX_SIZE) {
			throw DataTrackHandleError.tooLarge();
		}
		return raw;
	},
};

export class DataTrackHandleAllocator {
	value = 0;

	get(): DataTrackHandle | null {
		this.value += 1;
		if (this.value > U16_MAX_SIZE) {
			return null;
		}
		return this.value;
	}

	reset() {
		this.value = 0;
	}
}
