// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {LivekitReasonedError} from '../../errors.ts';
import type {DataTrackHandleError} from '../handle.ts';

export enum DataTrackDeserializeErrorReason {
	TooShort = 0,
	HeaderOverrun = 1,
	MissingExtWords = 2,
	UnsupportedVersion = 3,
	InvalidHandle = 4,
	MalformedExt = 5,
}

export class DataTrackDeserializeError<
	Reason extends DataTrackDeserializeErrorReason = DataTrackDeserializeErrorReason,
> extends LivekitReasonedError<DataTrackDeserializeErrorReason> {
	override readonly name = 'DataTrackDeserializeError';

	reason: Reason;

	reasonName: string;

	declare cause?: Reason extends DataTrackDeserializeErrorReason.InvalidHandle ? DataTrackHandleError : never;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(19, message, options);
		this.reason = reason;
		this.reasonName = DataTrackDeserializeErrorReason[reason];
	}

	static tooShort() {
		return new DataTrackDeserializeError(
			'Too short to contain a valid header',
			DataTrackDeserializeErrorReason.TooShort,
		);
	}

	static headerOverrun() {
		return new DataTrackDeserializeError(
			'Header exceeds total packet length',
			DataTrackDeserializeErrorReason.HeaderOverrun,
		);
	}

	static missingExtWords() {
		return new DataTrackDeserializeError(
			'Extension word indicator is missing',
			DataTrackDeserializeErrorReason.MissingExtWords,
		);
	}

	static unsupportedVersion(version: number) {
		return new DataTrackDeserializeError(
			`Unsupported version ${version}`,
			DataTrackDeserializeErrorReason.UnsupportedVersion,
		);
	}

	static invalidHandle(cause: DataTrackHandleError) {
		return new DataTrackDeserializeError(
			`invalid track handle: ${cause.message}`,
			DataTrackDeserializeErrorReason.InvalidHandle,
			{cause},
		);
	}

	static malformedExt(tag: number) {
		return new DataTrackDeserializeError(
			`Extension with tag ${tag} is malformed`,
			DataTrackDeserializeErrorReason.MalformedExt,
		);
	}
}

export enum DataTrackSerializeErrorReason {
	TooSmallForHeader = 0,
	TooSmallForPayload = 1,
}

export class DataTrackSerializeError<
	Reason extends DataTrackSerializeErrorReason = DataTrackSerializeErrorReason,
> extends LivekitReasonedError<DataTrackSerializeErrorReason> {
	override readonly name = 'DataTrackSerializeError';

	reason: Reason;

	reasonName: string;

	constructor(message: string, reason: Reason, options?: {cause?: unknown}) {
		super(19, message, options);
		this.reason = reason;
		this.reasonName = DataTrackSerializeErrorReason[reason];
	}

	static tooSmallForHeader() {
		return new DataTrackSerializeError('Buffer cannot fit header', DataTrackSerializeErrorReason.TooSmallForHeader);
	}

	static tooSmallForPayload() {
		return new DataTrackSerializeError('Buffer cannot fit payload', DataTrackSerializeErrorReason.TooSmallForPayload);
	}
}
