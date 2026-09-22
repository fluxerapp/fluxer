// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {LivekitReasonedError} from '../../errors.ts';

export enum DataTrackSubscribeErrorReason {
	Unpublished = 0,
	Timeout = 1,
	Disconnected = 2,
	Cancelled = 4,
}

export class DataTrackSubscribeError<
	Reason extends DataTrackSubscribeErrorReason = DataTrackSubscribeErrorReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackSubscribeError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(22, message, options);
		this.reason = reason;
		this.reasonName = DataTrackSubscribeErrorReason[reason];
	}

	static unpublished() {
		return new DataTrackSubscribeError(
			'The track has been unpublished and is no longer available',
			DataTrackSubscribeErrorReason.Unpublished,
		);
	}

	static timeout() {
		return new DataTrackSubscribeError(
			'Request to subscribe to data track timed-out',
			DataTrackSubscribeErrorReason.Timeout,
		);
	}

	static disconnected() {
		return new DataTrackSubscribeError(
			'Cannot subscribe to data track when disconnected',
			DataTrackSubscribeErrorReason.Disconnected,
		);
	}

	static cancelled() {
		return new DataTrackSubscribeError(
			'Subscription to data track cancelled by caller',
			DataTrackSubscribeErrorReason.Cancelled,
		);
	}
}
