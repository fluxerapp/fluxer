// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import type {Throws} from '../../../utils/throws.ts';
import {coerceToDataView} from '../utils.ts';
import {EXT_TAG_PADDING, U8_LENGTH_BYTES, U64_LENGTH_BYTES} from './constants.ts';
import {DataTrackDeserializeError, type DataTrackDeserializeErrorReason} from './errors.ts';
import Serializable from './serializable.ts';

export enum DataTrackExtensionTag {
	UserTimestamp = 2,
	E2ee = 1,
}

abstract class DataTrackExtension extends Serializable {
	static tag: DataTrackExtensionTag;

	static lengthBytes: number;
}

export class DataTrackUserTimestampExtension extends DataTrackExtension {
	static override tag = DataTrackExtensionTag.UserTimestamp;

	static override lengthBytes = 8;

	timestamp: bigint;

	constructor(timestamp: bigint) {
		super();
		this.timestamp = timestamp;
	}

	toBinaryLengthBytes(): number {
		return U8_LENGTH_BYTES + U8_LENGTH_BYTES + DataTrackUserTimestampExtension.lengthBytes;
	}

	toBinaryInto(dataView: DataView): Throws<number, never> {
		let byteIndex = 0;

		dataView.setUint8(byteIndex, DataTrackUserTimestampExtension.tag);
		byteIndex += U8_LENGTH_BYTES;

		dataView.setUint8(byteIndex, DataTrackUserTimestampExtension.lengthBytes);
		byteIndex += U8_LENGTH_BYTES;

		dataView.setBigUint64(byteIndex, this.timestamp);
		byteIndex += U64_LENGTH_BYTES;

		const totalLengthBytes = this.toBinaryLengthBytes();
		if (byteIndex !== totalLengthBytes) {
			throw new Error(
				`DataTrackUserTimestampExtension.toBinaryInto: Wrote ${byteIndex} bytes but expected length was ${totalLengthBytes} bytes`,
			);
		}

		return byteIndex;
	}

	toJSON() {
		return {
			tag: DataTrackUserTimestampExtension.tag as number,
			lengthBytes: DataTrackUserTimestampExtension.lengthBytes,

			timestamp: this.timestamp,
		};
	}
}

export class DataTrackE2eeExtension extends DataTrackExtension {
	static override tag = DataTrackExtensionTag.E2ee;

	static override lengthBytes = 13;

	keyIndex: number;

	iv: NonSharedUint8Array;

	constructor(keyIndex: number, iv: NonSharedUint8Array) {
		super();
		this.keyIndex = keyIndex;
		this.iv = iv;
	}

	toBinaryLengthBytes(): number {
		return U8_LENGTH_BYTES + U8_LENGTH_BYTES + DataTrackE2eeExtension.lengthBytes;
	}

	toBinaryInto(dataView: DataView): Throws<number, never> {
		let byteIndex = 0;

		dataView.setUint8(byteIndex, DataTrackE2eeExtension.tag);
		byteIndex += U8_LENGTH_BYTES;

		dataView.setUint8(byteIndex, DataTrackE2eeExtension.lengthBytes);
		byteIndex += U8_LENGTH_BYTES;

		dataView.setUint8(byteIndex, this.keyIndex);
		byteIndex += U8_LENGTH_BYTES;

		for (let i = 0; i < this.iv.length; i += 1) {
			dataView.setUint8(byteIndex, this.iv[i]);
			byteIndex += U8_LENGTH_BYTES;
		}

		const totalLengthBytes = this.toBinaryLengthBytes();
		if (byteIndex !== totalLengthBytes) {
			throw new Error(
				`DataTrackE2eeExtension.toBinaryInto: Wrote ${byteIndex} bytes but expected length was ${totalLengthBytes} bytes`,
			);
		}

		return byteIndex;
	}

	toJSON() {
		return {
			tag: DataTrackE2eeExtension.tag as number,
			lengthBytes: DataTrackE2eeExtension.lengthBytes,

			keyIndex: this.keyIndex,
			iv: this.iv,
		};
	}
}

export class DataTrackExtensions extends Serializable {
	userTimestamp?: DataTrackUserTimestampExtension;

	e2ee?: DataTrackE2eeExtension;

	constructor(opts: {userTimestamp?: DataTrackUserTimestampExtension; e2ee?: DataTrackE2eeExtension} = {}) {
		super();
		this.userTimestamp = opts.userTimestamp;
		this.e2ee = opts.e2ee;
	}

	toBinaryLengthBytes() {
		let lengthBytes = 0;
		if (this.userTimestamp) {
			lengthBytes += this.userTimestamp.toBinaryLengthBytes();
		}
		if (this.e2ee) {
			lengthBytes += this.e2ee.toBinaryLengthBytes();
		}
		return lengthBytes;
	}

	toBinaryInto(dataView: DataView): Throws<number, never> {
		let byteIndex = 0;

		if (this.e2ee) {
			const e2eeBytes = this.e2ee.toBinaryInto(dataView);
			byteIndex += e2eeBytes;
		}

		if (this.userTimestamp) {
			const userTimestampBytes = this.userTimestamp.toBinaryInto(
				new DataView(dataView.buffer, dataView.byteOffset + byteIndex),
			);
			byteIndex += userTimestampBytes;
		}

		const totalLengthBytes = this.toBinaryLengthBytes();
		if (byteIndex !== totalLengthBytes) {
			throw new Error(
				`DataTrackExtensions.toBinaryInto: Wrote ${byteIndex} bytes but expected length was ${totalLengthBytes} bytes`,
			);
		}

		return byteIndex;
	}

	static fromBinary<Input extends DataView | ArrayBuffer | Uint8Array>(
		input: Input,
	): Throws<
		[extensions: DataTrackExtensions, byteLength: number],
		DataTrackDeserializeError<DataTrackDeserializeErrorReason.MalformedExt>
	> {
		const dataView = coerceToDataView(input);

		let userTimestamp: DataTrackUserTimestampExtension | undefined;
		let e2ee: DataTrackE2eeExtension | undefined;

		let byteIndex = 0;
		while (dataView.byteLength - byteIndex >= U8_LENGTH_BYTES + U8_LENGTH_BYTES) {
			const tag = dataView.getUint8(byteIndex);
			byteIndex += U8_LENGTH_BYTES;

			const lengthBytes = dataView.getUint8(byteIndex);
			byteIndex += U8_LENGTH_BYTES;

			if (tag === EXT_TAG_PADDING) {
				continue;
			}

			switch (tag) {
				case DataTrackExtensionTag.UserTimestamp:
					if (dataView.byteLength - byteIndex < DataTrackUserTimestampExtension.lengthBytes) {
						throw DataTrackDeserializeError.malformedExt(tag);
					}
					userTimestamp = new DataTrackUserTimestampExtension(dataView.getBigUint64(byteIndex));
					byteIndex += lengthBytes;
					break;

				case DataTrackExtensionTag.E2ee: {
					if (dataView.byteLength - byteIndex < DataTrackE2eeExtension.lengthBytes) {
						throw DataTrackDeserializeError.malformedExt(tag);
					}

					const keyIndex = dataView.getUint8(byteIndex);

					const iv = new Uint8Array(12);
					for (let i = 0; i < iv.length; i += 1) {
						let byteIndexForIv = byteIndex;
						byteIndexForIv += U8_LENGTH_BYTES;
						byteIndexForIv += i * U8_LENGTH_BYTES;
						iv[i] = dataView.getUint8(byteIndexForIv);
					}

					e2ee = new DataTrackE2eeExtension(keyIndex, iv);
					byteIndex += lengthBytes;
					break;
				}

				default:
					if (dataView.byteLength - byteIndex < lengthBytes) {
						throw DataTrackDeserializeError.malformedExt(tag);
					}
					byteIndex += lengthBytes;
					break;
			}
		}

		return [new DataTrackExtensions({userTimestamp, e2ee}), dataView.byteLength];
	}

	toJSON() {
		return {
			userTimestamp: this.userTimestamp?.toJSON() ?? null,
			e2ee: this.e2ee?.toJSON() ?? null,
		};
	}
}
