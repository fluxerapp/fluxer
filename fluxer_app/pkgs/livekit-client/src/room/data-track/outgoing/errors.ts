// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {LivekitReasonedError} from '../../errors.ts';
import type {DataTrackPacketizerError} from '../packetizer.ts';

export enum DataTrackPublishErrorReason {
	NotAllowed = 0,

	DuplicateName = 1,

	Timeout = 2,

	LimitReached = 3,

	Disconnected = 4,

	Cancelled = 5,

	InvalidName = 6,

	Unknown = 7,
}

export class DataTrackPublishError<
	Reason extends DataTrackPublishErrorReason = DataTrackPublishErrorReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackPublishError';

	reason: Reason;

	reasonName: string;

	rawMessage?: string;

	constructor(message: string, reason: Reason, options?: {rawMessage?: string; cause?: unknown}) {
		super(21, message, options);
		this.reason = reason;
		this.reasonName = DataTrackPublishErrorReason[reason];
		this.rawMessage = options?.rawMessage;
	}

	static notAllowed(rawMessage?: string) {
		return new DataTrackPublishError('Data track publishing unauthorized', DataTrackPublishErrorReason.NotAllowed, {
			rawMessage,
		});
	}

	static duplicateName(rawMessage?: string) {
		return new DataTrackPublishError('Track name already taken', DataTrackPublishErrorReason.DuplicateName, {
			rawMessage,
		});
	}

	static invalidName(rawMessage?: string) {
		return new DataTrackPublishError('Track name is invalid', DataTrackPublishErrorReason.InvalidName, {rawMessage});
	}

	static timeout() {
		return new DataTrackPublishError(
			'Publish data track timed-out. Does the LiveKit server support data tracks?',
			DataTrackPublishErrorReason.Timeout,
		);
	}

	static limitReached(rawMessage?: string) {
		return new DataTrackPublishError('Data track publication limit reached', DataTrackPublishErrorReason.LimitReached, {
			rawMessage,
		});
	}

	static unknown(reason: number, message: string) {
		return new DataTrackPublishError(
			`Received RequestResponse for publishDataTrack, but reason was unrecognised (${reason}, ${message})`,
			DataTrackPublishErrorReason.Unknown,
		);
	}

	static disconnected() {
		return new DataTrackPublishError('Room disconnected', DataTrackPublishErrorReason.Disconnected);
	}

	static cancelled() {
		return new DataTrackPublishError('Publish data track cancelled by caller', DataTrackPublishErrorReason.Cancelled);
	}
}

export enum DataTrackPushFrameErrorReason {
	TrackUnpublished = 0,
	Dropped = 1,
}

export class DataTrackPushFrameError<
	Reason extends DataTrackPushFrameErrorReason = DataTrackPushFrameErrorReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackPushFrameError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(22, message, options);
		this.reason = reason;
		this.reasonName = DataTrackPushFrameErrorReason[reason];
	}

	static trackUnpublished() {
		return new DataTrackPushFrameError('Track is no longer published', DataTrackPushFrameErrorReason.TrackUnpublished);
	}

	static dropped(cause?: unknown) {
		return new DataTrackPushFrameError('Frame was dropped', DataTrackPushFrameErrorReason.Dropped, {
			cause,
		});
	}
}

export enum DataTrackOutgoingPipelineErrorReason {
	Packetizer = 0,
	Encryption = 1,
}

export class DataTrackOutgoingPipelineError<
	Reason extends DataTrackOutgoingPipelineErrorReason = DataTrackOutgoingPipelineErrorReason,
> extends LivekitReasonedError<Reason> {
	override readonly name = 'DataTrackOutgoingPipelineError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(21, message, options);
		this.reason = reason;
		this.reasonName = DataTrackOutgoingPipelineErrorReason[reason];
	}

	static packetizer(cause: DataTrackPacketizerError) {
		return new DataTrackOutgoingPipelineError(
			'Error packetizing frame',
			DataTrackOutgoingPipelineErrorReason.Packetizer,
			{cause},
		);
	}

	static encryption(cause: unknown) {
		return new DataTrackOutgoingPipelineError(
			'Error encrypting frame',
			DataTrackOutgoingPipelineErrorReason.Encryption,
			{cause},
		);
	}
}
