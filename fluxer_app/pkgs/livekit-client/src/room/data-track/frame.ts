// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../type-polyfills/non-shared-typed-arrays.ts';
import {DataTrackExtensions, DataTrackUserTimestampExtension} from './packet/extensions.ts';

export type DataTrackFrame = {
	payload: NonSharedUint8Array;
	userTimestamp?: bigint;
};

export type DataTrackFrameInternal = {
	payload: NonSharedUint8Array;
	extensions: DataTrackExtensions;
};

export const DataTrackFrameInternal = {
	from(frame: DataTrackFrame) {
		return {
			payload: frame.payload,
			extensions: new DataTrackExtensions({
				userTimestamp: frame.userTimestamp ? new DataTrackUserTimestampExtension(frame.userTimestamp) : undefined,
			}),
		};
	},
	lossyIntoFrame(frame: DataTrackFrameInternal): DataTrackFrame {
		return {
			payload: frame.payload,
			userTimestamp: frame.extensions.userTimestamp?.timestamp,
		};
	},
};
