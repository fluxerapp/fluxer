import {type AddTrackRequest, AudioTrackFeature, ParticipantPermission, TrackInfo} from '@livekit/protocol';
import {EventEmitter} from 'events';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import {SignalConnectionState} from '../../api/SignalClient.ts';
import {publishDefaults, roomOptionDefaults} from '../defaults.ts';
import {TrackEvent} from '../events.ts';
import type RTCEngine from '../RTCEngine.ts';
import LocalAudioTrack from '../track/LocalAudioTrack.ts';
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
