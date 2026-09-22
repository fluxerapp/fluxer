// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../type-polyfills/non-shared-typed-arrays.ts';

export interface DataPacketItem {
	data: NonSharedUint8Array;
	sequence: number;
	sent: boolean;
}

export class DataPacketBuffer {
	private buffer: Array<DataPacketItem> = [];

	private _sentSize = 0;

	push(item: DataPacketItem) {
		this.buffer.push(item);
		if (item.sent) {
			this._sentSize += item.data.byteLength;
		}
	}

	pop(): DataPacketItem | undefined {
		const item = this.buffer.shift();
		if (item) {
			if (item.sent) {
				this._sentSize -= item.data.byteLength;
			}
		}
		return item;
	}

	getAll(): Array<DataPacketItem> {
		return this.buffer.slice();
	}

	getUnsent(): Array<DataPacketItem> {
		return this.buffer.filter((item) => !item.sent);
	}

	markSent(item: DataPacketItem) {
		if (!item.sent) {
			item.sent = true;
			this._sentSize += item.data.byteLength;
		}
	}

	markAllUnsent() {
		for (const item of this.buffer) {
			item.sent = false;
		}
		this._sentSize = 0;
	}

	popToSequence(sequence: number) {
		while (this.buffer.length > 0) {
			const first = this.buffer[0];
			if (first.sequence <= sequence) {
				this.pop();
			} else {
				break;
			}
		}
	}

	alignBufferedAmount(bufferedAmount: number) {
		while (this.buffer.length > 0) {
			const first = this.buffer[0];
			if (!first.sent) {
				break;
			}
			if (this._sentSize - first.data.byteLength <= bufferedAmount) {
				break;
			}
			this.pop();
		}
	}

	get length(): number {
		return this.buffer.length;
	}
}
