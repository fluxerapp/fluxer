// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {Mutex} from '@livekit/mutex';
import TypedPromise from '../../utils/TypedPromise.ts';
import {UnexpectedConnectionState} from '../errors.ts';
import type {DataChannelKind} from './types.ts';

export interface FlowControlledDataChannelOptions {
	kind: DataChannelKind;
	lowWaterMark: number;
	highWaterMark: number;
	isEngineClosed: () => boolean;
	onBufferStatusChanged?: (isLow: boolean) => void;
}

export class FlowControlledDataChannel {
	readonly kind: DataChannelKind;

	readonly lowWaterMark: number;

	readonly highWaterMark: number;

	protected isEngineClosed: () => boolean;

	private onBufferStatusChanged?: (isLow: boolean) => void;

	private bufferStatusLow = true;

	private handle?: RTCDataChannel;

	private headroomLock = new Mutex();

	private waiterAbortController = new AbortController();

	constructor(opts: FlowControlledDataChannelOptions) {
		this.kind = opts.kind;
		this.lowWaterMark = opts.lowWaterMark;
		this.highWaterMark = opts.highWaterMark;
		this.isEngineClosed = opts.isEngineClosed;
		this.onBufferStatusChanged = opts.onBufferStatusChanged;
	}

	get channelHandle(): RTCDataChannel | undefined {
		return this.handle;
	}

	attach(dc: RTCDataChannel) {
		if (this.handle && this.handle !== dc) {
			this.invalidateWaiters('data channel replaced');
		}
		this.handle = dc;
	}

	detach(reason: string = 'data channel torn down') {
		if (this.handle) {
			this.invalidateWaiters(reason);
		}
		this.handle = undefined;
	}

	protected getChannel(): RTCDataChannel | undefined {
		return this.handle;
	}

	isBelowHighWaterMark(dc: RTCDataChannel): boolean {
		return dc.bufferedAmount <= this.highWaterMark;
	}

	isBelowLowWaterMark(dc: RTCDataChannel): boolean {
		return dc.bufferedAmount <= dc.bufferedAmountLowThreshold;
	}

	lockHeadroom(): Promise<() => void> {
		return this.headroomLock.lock();
	}

	async waitForHeadroomWithLock() {
		const unlock = await this.lockHeadroom();
		try {
			await this.waitForHeadroomWithoutLock();
		} finally {
			unlock();
		}
	}

	async waitForHeadroomWithoutLock() {
		if (this.isEngineClosed()) {
			throw new UnexpectedConnectionState('engine closed');
		}
		const dc = this.getChannel();
		if (!dc) {
			throw new UnexpectedConnectionState(`DataChannel not found, kind: ${this.kind}`);
		}
		if (this.isBelowHighWaterMark(dc)) {
			return;
		}
		const abortSignal = this.waiterAbortController.signal;
		await new TypedPromise<void, UnexpectedConnectionState>((resolve, reject) => {
			const onBufferedAmountLow = () => {
				cleanup();
				resolve();
			};
			const onDCClose = () => {
				cleanup();
				reject(new UnexpectedConnectionState(`DataChannel ${this.kind} closed while draining the buffer`));
			};
			const onAbort = () => {
				cleanup();
				reject(
					new UnexpectedConnectionState(
						`DataChannel ${this.kind} was replaced or torn down while waiting for headroom`,
					),
				);
			};
			const cleanup = () => {
				dc.removeEventListener('bufferedamountlow', onBufferedAmountLow);
				dc.removeEventListener('close', onDCClose);
				abortSignal.removeEventListener('abort', onAbort);
			};
			if (abortSignal.aborted) {
				onAbort();
				return;
			}
			dc.addEventListener('bufferedamountlow', onBufferedAmountLow);
			dc.addEventListener('close', onDCClose);
			abortSignal.addEventListener('abort', onAbort);
		});
	}

	invalidateWaiters(reason: string) {
		this.waiterAbortController.abort(reason);
		this.waiterAbortController = new AbortController();
	}

	refreshBufferStatus() {
		const dc = this.getChannel();
		if (!dc) {
			return;
		}
		const isLow = this.isBelowLowWaterMark(dc);
		if (isLow !== this.bufferStatusLow) {
			this.bufferStatusLow = isLow;
			this.onBufferStatusChanged?.(isLow);
		}
	}
}
