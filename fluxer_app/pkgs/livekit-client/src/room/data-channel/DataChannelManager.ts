// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {PCTransportManager} from '../PCTransportManager.ts';
import type {FlowControlledDataChannel} from './FlowControlledDataChannel.ts';
import {LossyDataChannel} from './LossyDataChannel.ts';
import {ReliableDataChannel} from './ReliableDataChannel.ts';
import {DataChannelKind, dataChannelHighWaterMark, dataChannelLowWaterMark} from './types.ts';

const lossyDataChannelLabel = '_lossy';
const reliableDataChannelLabel = '_reliable';
const dataTrackDataChannelLabel = '_data_track';

export interface DataChannelManagerOptions {
	isEngineClosed: () => boolean;
	isReconnecting: () => boolean;
	onDataMessage: (message: MessageEvent) => void;
	onDataTrackMessage: (message: MessageEvent) => void;
	onDataError: (event: Event) => void;
	onChannelClose: (kind: DataChannelKind) => void;
	onBufferStatusChanged: (kind: DataChannelKind, isLow: boolean) => void;
}

export class DataChannelManager {
	readonly reliable: ReliableDataChannel;

	readonly lossy: LossyDataChannel;

	readonly dataTrack: LossyDataChannel;

	private reliableSub?: RTCDataChannel;

	private lossySub?: RTCDataChannel;

	private dataTrackSub?: RTCDataChannel;

	private opts: DataChannelManagerOptions;

	constructor(opts: DataChannelManagerOptions) {
		this.opts = opts;
		const flowControlOptions = (kind: DataChannelKind) => ({
			kind,
			lowWaterMark: dataChannelLowWaterMark(kind),
			highWaterMark: dataChannelHighWaterMark(kind),
			isEngineClosed: opts.isEngineClosed,
			onBufferStatusChanged: (isLow: boolean) => opts.onBufferStatusChanged(kind, isLow),
		});
		this.reliable = new ReliableDataChannel({
			...flowControlOptions(DataChannelKind.RELIABLE),
			isDeferringSends: opts.isReconnecting,
		});
		this.lossy = new LossyDataChannel({
			...flowControlOptions(DataChannelKind.LOSSY),
			bufferFullBehavior: 'drop',
			shouldSkipSends: opts.isReconnecting,
		});
		this.dataTrack = new LossyDataChannel({
			...flowControlOptions(DataChannelKind.DATA_TRACK_LOSSY),
			bufferFullBehavior: 'wait',
			shouldSkipSends: opts.isReconnecting,
		});
	}

	channelFor(kind: DataChannelKind): FlowControlledDataChannel {
		switch (kind) {
			case DataChannelKind.RELIABLE:
				return this.reliable;
			case DataChannelKind.LOSSY:
				return this.lossy;
			case DataChannelKind.DATA_TRACK_LOSSY:
				return this.dataTrack;
		}
	}

	getHandle(kind: DataChannelKind, subscriber: boolean = false): RTCDataChannel | undefined {
		if (!subscriber) {
			return this.channelFor(kind).channelHandle;
		}
		switch (kind) {
			case DataChannelKind.RELIABLE:
				return this.reliableSub;
			case DataChannelKind.LOSSY:
				return this.lossySub;
			case DataChannelKind.DATA_TRACK_LOSSY:
				return this.dataTrackSub;
		}
	}

	get hasPublisherChannels(): boolean {
		return Boolean(this.reliable.channelHandle || this.lossy.channelHandle || this.dataTrack.channelHandle);
	}

	createPublisherChannels(pcManager: PCTransportManager) {
		for (const channel of [this.lossy, this.reliable, this.dataTrack]) {
			const old = channel.channelHandle;
			if (old) {
				old.onmessage = null;
				old.onerror = null;
				old.onclose = null;
			}
		}

		const wire = (
			channel: FlowControlledDataChannel,
			dc: RTCDataChannel,
			onMessage: (message: MessageEvent) => void,
		) => {
			dc.onmessage = onMessage;
			dc.onerror = this.opts.onDataError;
			dc.onclose = () => this.opts.onChannelClose(channel.kind);
			dc.bufferedAmountLowThreshold = channel.lowWaterMark;
			dc.onbufferedamountlow = () => channel.refreshBufferStatus();
			channel.attach(dc);
		};

		wire(
			this.lossy,
			pcManager.createPublisherDataChannel(lossyDataChannelLabel, {
				ordered: false,
				maxRetransmits: 0,
			}),
			this.opts.onDataMessage,
		);
		wire(
			this.reliable,
			pcManager.createPublisherDataChannel(reliableDataChannelLabel, {
				ordered: true,
			}),
			this.opts.onDataMessage,
		);
		wire(
			this.dataTrack,
			pcManager.createPublisherDataChannel(dataTrackDataChannelLabel, {
				ordered: false,
				maxRetransmits: 0,
			}),
			this.opts.onDataTrackMessage,
		);

		this.lossy.startThresholdTuning();
	}

	adoptSubscriberChannel(channel: RTCDataChannel): boolean {
		let handler: (message: MessageEvent) => void;
		if (channel.label === reliableDataChannelLabel) {
			this.reliableSub = channel;
			handler = this.opts.onDataMessage;
		} else if (channel.label === lossyDataChannelLabel) {
			this.lossySub = channel;
			handler = this.opts.onDataMessage;
		} else if (channel.label === dataTrackDataChannelLabel) {
			this.dataTrackSub = channel;
			handler = this.opts.onDataTrackMessage;
		} else {
			return false;
		}
		channel.onmessage = handler;
		return true;
	}

	teardown() {
		const dcCleanup = (dc: RTCDataChannel | undefined) => {
			if (!dc) {
				return;
			}

			dc.onbufferedamountlow = null;
			dc.onclose = null;
			dc.onclosing = null;
			dc.onerror = null;
			dc.onmessage = null;
			dc.onopen = null;

			dc.close();
		};

		for (const channel of [this.lossy, this.reliable, this.dataTrack]) {
			const dc = channel.channelHandle;
			channel.detach('peer connections cleaned up');
			dcCleanup(dc);
		}
		dcCleanup(this.lossySub);
		dcCleanup(this.reliableSub);
		dcCleanup(this.dataTrackSub);
		this.lossySub = undefined;
		this.reliableSub = undefined;
		this.dataTrackSub = undefined;

		this.reliable.reset();
	}
}
