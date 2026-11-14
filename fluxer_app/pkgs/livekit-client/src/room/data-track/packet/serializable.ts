// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0

import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import type {Throws} from '../../../utils/throws.ts';
import type {DataTrackSerializeError} from './errors.ts';

export default abstract class Serializable {
	abstract toBinaryLengthBytes(): number;

	abstract toBinaryInto(dataView: DataView): Throws<number, DataTrackSerializeError>;

	toBinary(): Throws<NonSharedUint8Array, DataTrackSerializeError> {
		const lengthBytes = this.toBinaryLengthBytes();
		const output = new ArrayBuffer(lengthBytes);
		const view = new DataView(output);

		const writtenBytes = this.toBinaryInto(view);

		if (lengthBytes !== writtenBytes) {
			throw new Error(
				`${this.constructor.name}.toBinary: written bytes (${writtenBytes} bytes) not equal to allocated array buffer length (${lengthBytes} bytes).`,
			);
		}

		return new Uint8Array(output);
	}
}
