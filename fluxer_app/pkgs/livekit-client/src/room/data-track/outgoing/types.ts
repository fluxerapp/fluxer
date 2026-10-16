// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {NonSharedUint8Array} from '../../../type-polyfills/non-shared-typed-arrays.ts';
import type {DataTrackHandle} from '../handle.ts';
import type LocalDataTrack from '../LocalDataTrack.ts';
import type {DataTrackInfo, DataTrackSid} from '../types.ts';
import type {DataTrackPublishError, DataTrackPublishErrorReason} from './errors.ts';

export type DataTrackOptions = {
	name: string;
};

export type SfuPublishResponseResult =
	| {type: 'ok'; data: DataTrackInfo}
	| {
			type: 'error';
			error:
				| DataTrackPublishError<DataTrackPublishErrorReason.NotAllowed>
				| DataTrackPublishError<DataTrackPublishErrorReason.DuplicateName>
				| DataTrackPublishError<DataTrackPublishErrorReason.InvalidName>
				| DataTrackPublishError<DataTrackPublishErrorReason.LimitReached>
				| DataTrackPublishError<DataTrackPublishErrorReason.Unknown>;
	  };

export type EventSfuPublishRequest = {
	handle: DataTrackHandle;
	name: string;
	usesE2ee: boolean;
};

export type EventSfuUnpublishRequest = {
	handle: DataTrackHandle;
};

export type EventPacketAvailable = {
	handle: DataTrackHandle;
	bytes: NonSharedUint8Array;
};

export type EventTrackPublished = {track: LocalDataTrack};

export type EventTrackUnpublished = {sid: DataTrackSid};

export type EventPacketsFlushedChange = {handle: DataTrackHandle; isFlushed: boolean};
