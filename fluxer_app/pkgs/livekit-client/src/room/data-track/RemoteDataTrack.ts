// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type Participant from '../participant/Participant.ts';
import type {DataTrackFrame} from './frame.ts';
import type IncomingDataTrackManager from './incoming/IncomingDataTrackManager.ts';
import {DataTrackSymbol, type IDataTrack, type IRemoteTrack, TrackSymbol} from './track-interfaces.ts';
import type {DataTrackInfo, RemoteDataTrackPipelineOptions} from './types.ts';

type RemoteDataTrackOptions = {
	publisherIdentity: Participant['identity'];
};

export type DataTrackSubscribeOptions = {
	signal?: AbortSignal;

	bufferSize?: number;
};

export default class RemoteDataTrack implements IRemoteTrack, IDataTrack {
	readonly trackSymbol = TrackSymbol;

	readonly isLocal = false;

	readonly typeSymbol = DataTrackSymbol;

	info: DataTrackInfo;

	publisherIdentity: Participant['identity'];

	protected manager: IncomingDataTrackManager;

	constructor(info: DataTrackInfo, manager: IncomingDataTrackManager, options: RemoteDataTrackOptions) {
		this.info = info;
		this.manager = manager;
		this.publisherIdentity = options.publisherIdentity;
	}

	subscribe(options?: DataTrackSubscribeOptions): ReadableStream<DataTrackFrame> {
		const [stream, sfuSubscriptionComplete] = this.manager.openSubscriptionStream(
			this.info.sid,
			options?.signal,
			options?.bufferSize,
		);
		sfuSubscriptionComplete.catch(() => {});
		return stream;
	}

	setPipelineOptions(options: RemoteDataTrackPipelineOptions): void {
		this.manager.setPipelineOptions(this.info.sid, options);
	}
}
