// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {type AddTrackRequest, AudioTrackFeature, ParticipantPermission, TrackInfo} from '@livekit/protocol';
import {EventEmitter} from 'events';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {SignalConnectionState} from '../../api/SignalClient.ts';
import {publishDefaults, roomOptionDefaults} from '../defaults.ts';
import {TrackEvent} from '../events.ts';
import type RTCEngine from '../RTCEngine.ts';
import LocalAudioTrack from '../track/LocalAudioTrack.ts';
import type {AudioProcessorOptions, TrackProcessor} from '../track/processor/types.ts';
import {Track} from '../track/Track.ts';
import LocalParticipant from './LocalParticipant.ts';

class TestMediaStreamTrack extends EventTarget {
	readonly id = 'microphone-input';
	readonly kind = 'audio';
	enabled = true;
	readyState: MediaStreamTrackState = 'live';

	constructor(private readonly channelCount: number) {
		super();
	}

	getSettings(): MediaTrackSettings {
		return {channelCount: this.channelCount, echoCancellation: true};
	}

	getConstraints(): MediaTrackConstraints {
		return {};
	}

	stop() {
		this.readyState = 'ended';
	}
}

class TestMediaStream {
	constructor(private readonly tracks: Array<MediaStreamTrack>) {}

	getTracks() {
		return this.tracks;
	}
}

function createPublisher() {
	const setTrackCodecBitrate = vi.fn();
	const sendUpdateLocalAudioTrack = vi.fn();
	const sender = {replaceTrack: vi.fn(async () => undefined)} as unknown as RTCRtpSender;
	const transceiver = {sender} as RTCRtpTransceiver;
	const addTrack = vi.fn(
		async (request: AddTrackRequest) =>
			new TrackInfo({
				sid: 'TR_microphone',
				name: request.name,
				type: request.type,
				source: request.source,
				audioFeatures: request.audioFeatures,
			}),
	);
	const engine = Object.assign(new EventEmitter(), {
		isClosed: false,
		logContext: {},
		client: {currentState: SignalConnectionState.CONNECTED, sendUpdateLocalAudioTrack},
		pcManager: {publisher: {getTransceivers: () => [transceiver], setTrackCodecBitrate}},
		createSender: vi.fn(async () => sender),
		negotiate: vi.fn(async () => undefined),
		addTrack,
	});
	type ParticipantArgs = ConstructorParameters<typeof LocalParticipant>;
	const participant = new LocalParticipant(
		'PA_publisher',
		'publisher',
		engine as unknown as RTCEngine,
		{...roomOptionDefaults, publishDefaults},
		{} as ParticipantArgs[4],
		{} as ParticipantArgs[5],
		{} as ParticipantArgs[6],
		{} as ParticipantArgs[7],
	);
	participant.permissions = new ParticipantPermission({canPublish: true});
	return {participant, addTrack, setTrackCodecBitrate, sendUpdateLocalAudioTrack};
}

describe('microphone publication stereo metadata', () => {
	beforeEach(() => {
		vi.stubGlobal('MediaStreamTrack', TestMediaStreamTrack);
		vi.stubGlobal('MediaStream', TestMediaStream);
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it.each([
		{name: 'mono policy on a two-channel device', channelCount: 2, forceStereo: false, stereo: false},
		{name: 'automatic stereo on a two-channel device', channelCount: 2, forceStereo: undefined, stereo: true},
		{name: 'forced stereo on a one-channel device', channelCount: 1, forceStereo: true, stereo: true},
		{name: 'automatic mono on a one-channel device', channelCount: 1, forceStereo: undefined, stereo: false},
	])(
		'keeps the request, SDP policy, and feature updates consistent for $name',
		async ({channelCount, forceStereo, stereo}) => {
			const {participant, addTrack, setTrackCodecBitrate, sendUpdateLocalAudioTrack} = createPublisher();
			const source = new TestMediaStreamTrack(channelCount);
			const track = new LocalAudioTrack(source as unknown as MediaStreamTrack);
			track.source = Track.Source.Microphone;
			await track.runWithTrackChangeLock(async () => undefined);
			const publication = await participant.publishTrack(track, {
				audioPreset: {maxBitrate: 64000},
				forceStereo,
			});

			expect(addTrack).toHaveBeenCalledTimes(1);
			const request = addTrack.mock.calls[0]![0];
			expect(request.stereo).toBe(stereo);
			expect(request.audioFeatures.includes(AudioTrackFeature.TF_STEREO)).toBe(stereo);
			expect(request.audioFeatures).toContain(AudioTrackFeature.TF_ECHO_CANCELLATION);
			expect(setTrackCodecBitrate).toHaveBeenCalledWith(expect.objectContaining({codec: 'opus', maxbr: 64, stereo}));

			expect(publication.getTrackFeatures().includes(AudioTrackFeature.TF_STEREO)).toBe(stereo);
			track.emit(TrackEvent.AudioTrackFeatureUpdate, track, AudioTrackFeature.TF_ECHO_CANCELLATION, true);
			expect(sendUpdateLocalAudioTrack).toHaveBeenCalledTimes(1);
			const [sid, features] = sendUpdateLocalAudioTrack.mock.calls[0]!;
			expect(sid).toBe('TR_microphone');
			expect(features.includes(AudioTrackFeature.TF_STEREO)).toBe(stereo);
			expect(features).toContain(AudioTrackFeature.TF_ECHO_CANCELLATION);
			track.stop();
		},
	);
});

let nextTrackId = 0;

class FakeMediaStreamTrack {
	readonly id = `track-${nextTrackId++}`;
	readonly kind = 'audio';
	enabled = true;
	muted = false;
	readyState: MediaStreamTrackState = 'live';
	appliedConstraints: MediaTrackConstraints = {};

	constructor(private readonly settings: MediaTrackSettings = {}) {}

	getConstraints(): MediaTrackConstraints {
		return this.appliedConstraints;
	}

	getSettings(): MediaTrackSettings {
		return this.settings;
	}

	async applyConstraints(): Promise<void> {}

	addEventListener(): void {}

	removeEventListener(): void {}

	stop(): void {
		this.readyState = 'ended';
	}
}

class FakeProcessor implements TrackProcessor<Track.Kind.Audio, AudioProcessorOptions> {
	name = 'fake-processor';
	processedTrack?: MediaStreamTrack;
	readonly contexts: Array<AudioContext | undefined> = [];
	destroyed = 0;

	constructor(private readonly failInit = false) {}

	async init(opts: AudioProcessorOptions): Promise<void> {
		this.contexts.push(opts.audioContext);
		if (this.failInit) throw new Error('processor init failed');
		this.processedTrack = new FakeMediaStreamTrack() as unknown as MediaStreamTrack;
	}

	async restart(): Promise<void> {}

	async destroy(): Promise<void> {
		this.destroyed++;
	}
}

const audioContext = {state: 'running'} as unknown as AudioContext;
const getUserMediaCalls: Array<MediaStreamConstraints> = [];
let getUserMediaResult: (constraints: MediaStreamConstraints) => Promise<{getTracks: () => Array<MediaStreamTrack>}>;

function createParticipant(): LocalParticipant {
	const engine = {
		on() {
			return engine;
		},
		logContext: {},
	};
	const participant = new LocalParticipant(
		'participant-sid',
		'participant-identity',
		engine as never,
		{audioCaptureDefaults: {}, videoCaptureDefaults: {}} as never,
		{} as never,
		{} as never,
		{} as never,
		{} as never,
	);
	participant.setAudioContext(audioContext);
	return participant;
}

function nextCapture(settings: MediaTrackSettings = {}): FakeMediaStreamTrack {
	const track = new FakeMediaStreamTrack(settings);
	getUserMediaResult = async (constraints) => {
		track.appliedConstraints = typeof constraints.audio === 'object' ? constraints.audio : {};
		return {getTracks: () => [track as unknown as MediaStreamTrack]};
	};
	return track;
}

describe('LocalParticipant microphone processor and device loss', () => {
	beforeEach(() => {
		getUserMediaCalls.length = 0;
		vi.stubGlobal(
			'MediaStream',
			class {
				constructor(readonly tracks: Array<MediaStreamTrack>) {}
			},
		);
		vi.stubGlobal('MediaStreamTrack', FakeMediaStreamTrack);
		vi.stubGlobal('navigator', {
			mediaDevices: {
				getUserMedia: (constraints: MediaStreamConstraints) => {
					getUserMediaCalls.push(constraints);
					return getUserMediaResult(constraints);
				},
			},
		});
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('gives the audio processor the participant audio context when creating a microphone track', async () => {
		nextCapture();
		const processor = new FakeProcessor();
		const [track] = await createParticipant().createTracks({audio: {processor}});
		expect(processor.contexts).toEqual([audioContext]);
		expect((track as LocalAudioTrack).getProcessor()).toBe(processor);
		expect(track?.mediaStreamTrack).toBe(processor.processedTrack);
	});

	it('returns the unprocessed microphone track when the processor cannot start', async () => {
		const raw = nextCapture();
		const processor = new FakeProcessor(true);
		const [track] = await createParticipant().createTracks({audio: {processor}});
		expect((track as LocalAudioTrack).getProcessor()).toBeUndefined();
		expect(track?.mediaStreamTrack).toBe(raw as unknown as MediaStreamTrack);
		expect(processor.destroyed).toBeGreaterThan(0);
	});

	it('destroys the audio processor when the microphone cannot be captured', async () => {
		getUserMediaResult = async () => {
			throw new Error('NotReadableError');
		};
		const processor = new FakeProcessor();
		await expect(createParticipant().createTracks({audio: {processor}})).rejects.toThrow('NotReadableError');
		expect(processor.contexts).toEqual([]);
		expect(processor.destroyed).toBe(1);
	});

	it('keeps echo cancellation, noise suppression, gain control and channels when the device is lost', async () => {
		nextCapture({deviceId: 'usb-mic'});
		const participant = createParticipant();
		const [track] = await participant.createTracks({
			audio: {
				deviceId: 'usb-mic',
				echoCancellation: false,
				noiseSuppression: false,
				autoGainControl: false,
				channelCount: {ideal: 2},
			},
		});
		getUserMediaCalls.length = 0;
		nextCapture({deviceId: 'default'});
		await (participant as unknown as {handleTrackEnded: (track: unknown) => Promise<void>}).handleTrackEnded(track);
		expect(getUserMediaCalls).toHaveLength(1);
		expect(getUserMediaCalls[0]?.audio).toMatchObject({
			deviceId: 'default',
			echoCancellation: false,
			noiseSuppression: false,
			autoGainControl: false,
			channelCount: {ideal: 2},
		});
	});
});
