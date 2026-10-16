// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type Participant from '../../participant/Participant.ts';
import type RemoteDataTrack from '../RemoteDataTrack.ts';
import type {DataTrackSid} from '../types.ts';

export type EventSfuUpdateSubscription = {
	sid: DataTrackSid;
	subscribe: boolean;
};

export type EventTrackAvailable = {
	track: RemoteDataTrack;
};

export type EventTrackUnavailable = {
	sid: DataTrackSid;
	publisherIdentity: Participant['identity'];
};
