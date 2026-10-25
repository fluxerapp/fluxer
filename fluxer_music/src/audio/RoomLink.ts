// SPDX-License-Identifier: AGPL-3.0-or-later

import {EventEmitter} from 'node:events';
import {parseParticipantUserId} from '@fluxer/music_protocol/src/Identity';
import {
	decodeRpcPayload,
	encodeRpcResult,
	RPC_METHODS,
	type RpcMethod,
	type RpcPayloads,
	type RpcResponses,
} from '@fluxer/music_protocol/src/Rpc';
import {encodeState} from '@fluxer/music_protocol/src/State';
import {MUSIC_STATE_TOPIC, type MusicErrorCode, type MusicState} from '@fluxer/music_protocol/src/Types';
import {
	AudioFrame,
	AudioSource,
	LocalAudioTrack,
	Room,
	RoomEvent,
	type RpcInvocationData,
	TrackPublishOptions,
	TrackSource,
} from '@livekit/rtc-node';
import {MusicError} from '../Errors';
import type {VoiceGrant} from '../fluxer/GatewayClient';
import {type AudioSink, FRAME_SAMPLES_PER_CHANNEL} from './Player';

const SAMPLE_RATE = 48000;
const CHANNELS = 2;
const TRACK_NAME = 'music';

export type RpcHandlers = {
	[M in RpcMethod]: (callerUserId: string, payload: RpcPayloads[M]) => Promise<RpcResponses[M]>;
};

export interface RoomLinkOptions {
	/** Opus max bitrate in bits per second (the voice channel's bitrate). A function is read each time the room is joined. */
	bitrate: number | (() => number);
}

/** Joins a LiveKit room, publishes the audio track, broadcasts state and serves RPC. Emits 'disconnected'. */
export class RoomLink extends EventEmitter {
	readonly sink: AudioSink;

	private readonly source = new AudioSource(SAMPLE_RATE, CHANNELS);
	private readonly bitrate: () => number;
	private room: Room | null = null;
	private handlers: RpcHandlers | null = null;

	constructor(opts: RoomLinkOptions) {
		super();
		const {bitrate} = opts;
		this.bitrate = typeof bitrate === 'function' ? bitrate : () => bitrate;
		this.sink = {
			captureFrame: (samples) =>
				this.source.captureFrame(new AudioFrame(samples, SAMPLE_RATE, CHANNELS, FRAME_SAMPLES_PER_CHANNEL)),
		};
	}

	async connect(grant: VoiceGrant, handlers: RpcHandlers): Promise<void> {
		this.handlers = handlers;
		await this.join(grant, handlers);
	}

	async reconnect(grant: VoiceGrant): Promise<void> {
		if (!this.handlers) throw new Error('RoomLink.reconnect called before connect');
		await this.leave();
		await this.join(grant, this.handlers);
	}

	async broadcast(state: MusicState): Promise<void> {
		const room = this.room;
		if (!room?.localParticipant) return;
		await room.localParticipant.publishData(encodeState(state), {reliable: true, topic: MUSIC_STATE_TOPIC});
	}

	async disconnect(): Promise<void> {
		this.handlers = null;
		await this.leave();
	}

	private async join(grant: VoiceGrant, handlers: RpcHandlers): Promise<void> {
		const room = new Room();
		try {
			await room.connect(grant.endpoint, grant.token, {autoSubscribe: false, dynacast: false});
			const local = room.localParticipant;
			if (!local) throw new Error('LiveKit connected without a local participant');
			for (const method of RPC_METHODS) {
				local.registerRpcMethod(method, (data) => this.serve(method, data, handlers));
			}
			const track = LocalAudioTrack.createAudioTrack(TRACK_NAME, this.source);
			await local.publishTrack(
				track,
				new TrackPublishOptions({
					source: TrackSource.SOURCE_MICROPHONE,
					audioEncoding: {maxBitrate: BigInt(this.bitrate())},
				}),
			);
		} catch (err) {
			await room.disconnect().catch(() => {});
			throw err;
		}
		this.room = room;
		room.on(RoomEvent.Disconnected, () => {
			if (this.room !== room) return;
			this.room = null;
			this.emit('disconnected');
		});
	}

	private async leave(): Promise<void> {
		const room = this.room;
		this.room = null;
		if (room) await room.disconnect().catch(() => {});
	}

	private async serve<M extends RpcMethod>(method: M, data: RpcInvocationData, handlers: RpcHandlers): Promise<string> {
		const fail = (error: MusicErrorCode) => encodeRpcResult({ok: false, error});
		const callerId = parseParticipantUserId(data.callerIdentity);
		const payload = decodeRpcPayload(method, data.payload);
		if (callerId === null || payload === null) return fail('BAD_REQUEST');
		try {
			const handler = handlers[method] as (id: string, p: RpcPayloads[M]) => Promise<RpcResponses[M]>;
			return encodeRpcResult({ok: true, value: await handler(callerId, payload)});
		} catch (err) {
			if (err instanceof MusicError) return fail(err.code);
			console.error(`rpc ${method} failed: ${err instanceof Error ? err.message : String(err)}`);
			return fail('SOURCE_FAILED');
		}
	}
}
