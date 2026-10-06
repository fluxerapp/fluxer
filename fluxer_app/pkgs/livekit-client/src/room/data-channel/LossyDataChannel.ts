// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import log from '../../logger.ts';
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import CriticalTimers, {type TimerHandle} from '../timers.ts';
import {FlowControlledDataChannel, type FlowControlledDataChannelOptions} from './FlowControlledDataChannel.ts';

export interface LossyDataChannelOptions extends FlowControlledDataChannelOptions {
	bufferFullBehavior: 'drop' | 'wait';
	shouldSkipSends: () => boolean;
}

export class LossyDataChannel extends FlowControlledDataChannel {
	private bufferFullBehavior: 'drop' | 'wait';

	private shouldSkipSends: () => boolean;

	private statCurrentBytes = 0;

	private statByterate = 0;

	private statInterval: TimerHandle | undefined;

	private dropCount = 0;

	constructor(opts: LossyDataChannelOptions) {
		super(opts);
		this.bufferFullBehavior = opts.bufferFullBehavior;
		this.shouldSkipSends = opts.shouldSkipSends;
	}

	async send(msg: NonSharedUint8Array) {
		const dc = this.getChannel();
		if (!dc) {
			return;
		}
		switch (this.bufferFullBehavior) {
			case 'wait':
				if (!this.isBelowHighWaterMark(dc)) {
					await this.waitForHeadroomWithLock();
				}
				break;
			case 'drop':
				if (!this.isBelowLowWaterMark(dc)) {
					this.dropCount += 1;
					if (this.dropCount % 100 === 0) {
						log.warn(`dropping lossy data channel messages, total dropped: ${this.dropCount}`);
					}
					return;
				}
		}
		this.statCurrentBytes += msg.byteLength;

		if (this.shouldSkipSends()) {
			return;
		}

		try {
			dc.send(msg);
			this.refreshBufferStatus();
		} catch (error: unknown) {
			if (error instanceof TypeError) {
				log.error(error);
			} else {
				throw error;
			}
		}
	}

	startThresholdTuning() {
		this.stopThresholdTuning();
		this.statInterval = CriticalTimers.setInterval(() => {
			this.statByterate = this.statCurrentBytes;
			this.statCurrentBytes = 0;

			const dc = this.getChannel();
			if (dc) {
				const threshold = this.statByterate / 10;
				dc.bufferedAmountLowThreshold = Math.min(Math.max(threshold, this.lowWaterMark), this.highWaterMark);
			}
		}, 1000);
	}

	stopThresholdTuning() {
		this.statByterate = 0;
		this.statCurrentBytes = 0;
		if (this.statInterval) {
			CriticalTimers.clearInterval(this.statInterval);
			this.statInterval = undefined;
		}
		this.dropCount = 0;
	}
}
