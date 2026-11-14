// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {TrackInfo} from '@livekit/protocol';
import log from '../logger.ts';
import {RoomEvent} from '../room/events.ts';
import type Room from '../room/Room.ts';
import {FrameMetadataExtractor} from '../room/track/FrameMetadataExtractor.ts';
import type RemoteTrack from '../room/track/RemoteTrack.ts';
import RemoteVideoTrack from '../room/track/RemoteVideoTrack.ts';
import type {PTDecodeMessage, PTUpdateTrackIdMessage, PTWorkerMessage} from './types.ts';
import {isFrameMetadataSupported, shouldUseFrameMetadataScriptTransform} from './utils.ts';

export interface FrameMetadataOptions {
	worker: Worker;
}

export class FrameMetadataManager {
	private worker?: Worker;

	private room?: Room;

	private extractors = new Map<string, FrameMetadataExtractor>();

	private workerPipelines = new Map<RTCRtpReceiver, string>();

	constructor(options?: FrameMetadataOptions) {
		this.worker = options?.worker;
	}

	setup(room: Room) {
		if (room === this.room) {
			return;
		}
		this.room = room;

		if (this.worker) {
			this.worker.onmessage = this.onWorkerMessage;
			this.worker.onerror = this.onWorkerError;
			this.worker.postMessage({kind: 'init'});
		}

		room
			.on(RoomEvent.TrackSubscribed, (track, pub, _participant) => {
				if (track.kind !== 'video') {
					return;
				}
				this.setupReceiver(track as unknown as RemoteVideoTrack, pub.trackInfo);
			})
			.on(RoomEvent.TrackUnsubscribed, (track) => {
				this.teardownTrack(track);
			})
			.on(RoomEvent.Disconnected, () => {
				this.cleanup();
			});
	}

	private setupReceiver(track: RemoteVideoTrack, trackInfo?: TrackInfo) {
		const receiver = track.receiver;
		if (!receiver) {
			return;
		}

		const hasFeatures = !!trackInfo?.packetTrailerFeatures && trackInfo.packetTrailerFeatures.length > 0;
		if (!hasFeatures) {
			if (!this.room?.hasE2EESetup) {
				this.setupPassthroughReceiver(receiver, track.mediaStreamID);
			}
			return;
		}

		if (!isFrameMetadataSupported(this.worker ? {worker: this.worker} : undefined) && !this.room?.hasE2EESetup) {
			log.warn('frame metadata transform not supported; skipping extraction');
			return;
		}

		const extractor = new FrameMetadataExtractor();
		const trackId = track.mediaStreamID;

		this.extractors.set(trackId, extractor);
		track.frameMetadataExtractor = extractor;

		if (this.room?.hasE2EESetup) {
			return;
		}

		this.setupWorkerReceiver(receiver, trackId, true);
	}

	private setupPassthroughReceiver(receiver: RTCRtpReceiver, trackId: string) {
		if (shouldUseFrameMetadataScriptTransform()) {
			if ('transform' in receiver) {
				receiver.transform = null;
			}
			return;
		}

		if (this.worker && isFrameMetadataSupported({worker: this.worker}) && !this.workerPipelines.has(receiver)) {
			this.setupWorkerReceiver(receiver, trackId, false);
			return;
		}

		if (this.worker && this.workerPipelines.has(receiver)) {
			this.setupWorkerReceiver(receiver, trackId, false);
		}
	}

	private setupWorkerReceiver(receiver: RTCRtpReceiver, newTrackId: string, hasPacketTrailer = true) {
		const worker = this.worker;
		if (!worker) {
			return;
		}

		if (shouldUseFrameMetadataScriptTransform()) {
			receiver.transform = new RTCRtpScriptTransform(worker, {
				kind: 'decode',
				trackId: newTrackId,
			});
			return;
		}

		const existingTrackId = this.workerPipelines.get(receiver);

		if (existingTrackId) {
			const msg: PTUpdateTrackIdMessage = {
				kind: 'updateTrackId',
				data: {oldTrackId: existingTrackId, newTrackId, hasPacketTrailer},
			};
			worker.postMessage(msg);
			this.workerPipelines.set(receiver, newTrackId);
			return;
		}

		if (!('createEncodedStreams' in receiver)) {
			log.warn('createEncodedStreams not supported');
			return;
		}

		let streams: {readable: ReadableStream; writable: WritableStream};
		try {
			// @ts-expect-error — createEncodedStreams is not in standard typings
			streams = receiver.createEncodedStreams();
		} catch (err) {
			log.warn('failed to create encoded streams', {error: err});
			return;
		}

		const msg: PTDecodeMessage = {
			kind: 'decode',
			data: {
				readableStream: streams.readable,
				writableStream: streams.writable,
				trackId: newTrackId,
				hasPacketTrailer,
			},
		};
		worker.postMessage(msg, [streams.readable, streams.writable]);
		this.workerPipelines.set(receiver, newTrackId);
	}

	private teardownTrack(track: RemoteTrack) {
		const trackId = track.mediaStreamID;
		const extractor = this.extractors.get(trackId);
		if (extractor) {
			extractor.dispose();
			this.extractors.delete(trackId);
		}

		if (track instanceof RemoteVideoTrack) {
			track.frameMetadataExtractor = undefined;
		}
	}

	private cleanup() {
		for (const extractor of this.extractors.values()) {
			extractor.dispose();
		}
		this.extractors.clear();
		this.workerPipelines.clear();
		this.worker?.terminate();
	}

	private onWorkerMessage = (ev: MessageEvent<PTWorkerMessage>) => {
		const msg = ev.data;
		if (msg.kind === 'metadata') {
			const extractor = this.extractors.get(msg.data.trackId);
			if (extractor) {
				extractor.storeMetadata(msg.data.rtpTimestamp, msg.data.ssrc, msg.data.metadata);
			}
		}
	};

	private onWorkerError = (ev: ErrorEvent) => {
		log.error('frame metadata worker encountered an error:', {error: ev.error});
	};
}

export const PacketTrailerManager = FrameMetadataManager;

export type PacketTrailerOptions = FrameMetadataOptions;
