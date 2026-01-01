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

import {Readable, type Stream} from 'node:stream';
import type {ReadableStream as WebReadableStream} from 'node:stream/web';

type BinaryLike = ArrayBufferView | ArrayBuffer;

export const toBodyData = (value: BinaryLike): Uint8Array<ArrayBuffer> => {
	if (value instanceof ArrayBuffer) {
		return new Uint8Array(value);
	}

	if (value.buffer instanceof ArrayBuffer) {
		return new Uint8Array(value.buffer, value.byteOffset, value.byteLength);
	}

	const copyBuffer = new ArrayBuffer(value.byteLength);
	const view = new Uint8Array(copyBuffer);
	view.set(new Uint8Array(value.buffer, value.byteOffset, value.byteLength));
	return new Uint8Array(copyBuffer);
};

export const toWebReadableStream = (stream: Stream): WebReadableStream<Uint8Array> => {
	return Readable.toWeb(stream as Readable);
};
