// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {DataPacketBuffer} from '../../utils/dataPacketBuffer.ts';
import {FlowControlledDataChannel, type FlowControlledDataChannelOptions} from './FlowControlledDataChannel.ts';

export interface ReliableDataChannelOptions extends FlowControlledDataChannelOptions {
	isDeferringSends: () => boolean;
}

export class ReliableDataChannel extends FlowControlledDataChannel {
	private messageBuffer = new DataPacketBuffer();

	private sequence = 1;

	private isDeferringSends: () => boolean;

	constructor(opts: ReliableDataChannelOptions) {
		super(opts);
		this.isDeferringSends = opts.isDeferringSends;
	}

	nextSequence(): number {
		const sequence = this.sequence;
		this.sequence += 1;
		return sequence;
	}

	async send(msg: NonSharedUint8Array, sequence: number) {
		if (this.isDeferringSends()) {
			this.messageBuffer.push({data: msg, sequence, sent: false});
			return;
		}

		const dc = this.getChannel();
		if (!dc) {
			return;
		}

		try {
			await this.waitForHeadroomWithLock();
		} catch (error) {
			if (this.isEngineClosed()) {
				throw error;
			}
			this.messageBuffer.push({data: msg, sequence, sent: false});
			return;
		}

		if (this.isDeferringSends()) {
			this.messageBuffer.push({data: msg, sequence, sent: false});
			return;
		}

		this.messageBuffer.push({data: msg, sequence, sent: true});
		dc.send(msg);
		this.refreshBufferStatus();
	}

	async replay(lastMessageSeq: number) {
		const dc = this.getChannel();
		if (!dc) {
			return;
		}
		this.messageBuffer.popToSequence(lastMessageSeq);
		const unlock = await this.lockHeadroom();
		try {
			this.messageBuffer.markAllUnsent();
			for (let batch = this.messageBuffer.getUnsent(); batch.length > 0; batch = this.messageBuffer.getUnsent()) {
				for (const item of batch) {
					await this.waitForHeadroomWithoutLock();
					dc.send(item.data);
					this.messageBuffer.markSent(item);
				}
			}
		} finally {
			unlock();
		}
		this.refreshBufferStatus();
	}

	override refreshBufferStatus() {
		const dc = this.channelHandle;
		if (dc) {
			this.messageBuffer.alignBufferedAmount(dc.bufferedAmount);
		}
		super.refreshBufferStatus();
	}

	reset() {
		this.messageBuffer = new DataPacketBuffer();
		this.sequence = 1;
	}
}
