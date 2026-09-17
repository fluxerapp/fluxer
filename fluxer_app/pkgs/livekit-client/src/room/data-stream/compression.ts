// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {DataStreamError, DataStreamErrorReason} from '../errors.ts';

export function deflateRawTransform(): ReadableWritablePair<NonSharedUint8Array, NonSharedUint8Array> {
	return new CompressionStream('deflate-raw') as unknown as ReadableWritablePair<
		NonSharedUint8Array,
		NonSharedUint8Array
	>;
}

export function inflateRawTransform(): ReadableWritablePair<NonSharedUint8Array, NonSharedUint8Array> {
	return new DecompressionStream('deflate-raw') as unknown as ReadableWritablePair<
		NonSharedUint8Array,
		NonSharedUint8Array
	>;
}

export async function deflateRawCompress(data: NonSharedUint8Array): Promise<NonSharedUint8Array> {
	const cs = new CompressionStream('deflate-raw');
	const writer = cs.writable.getWriter();
	writer.write(data as NonSharedUint8Array);
	writer.close();
	return collect(cs.readable);
}

export async function deflateRawDecompress(
	data: NonSharedUint8Array,
	maxByteLength?: number,
): Promise<NonSharedUint8Array> {
	const ds = new DecompressionStream('deflate-raw');
	const writer = ds.writable.getWriter();
	writer.write(data as NonSharedUint8Array).catch(() => {});
	writer.close().catch(() => {});
	return collect(ds.readable, maxByteLength);
}

export async function collect(
	stream: ReadableStream<NonSharedUint8Array>,
	maxByteLength?: number,
): Promise<NonSharedUint8Array> {
	const reader = stream.getReader();
	const chunks: Array<NonSharedUint8Array> = [];
	let total = 0;
	while (true) {
		const {done, value} = await reader.read();
		if (done) {
			break;
		}
		chunks.push(value);
		total += value.byteLength;
		if (typeof maxByteLength === 'number' && total > maxByteLength) {
			await reader.cancel();
			throw new DataStreamError(
				`Decompressed payload exceeds the maximum payload size of ${maxByteLength} bytes`,
				DataStreamErrorReason.PayloadTooLarge,
			);
		}
	}
	const result = new Uint8Array(total);
	let offset = 0;
	for (const chunk of chunks) {
		result.set(chunk, offset);
		offset += chunk.byteLength;
	}
	return result;
}
