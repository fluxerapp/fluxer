// SPDX-License-Identifier: AGPL-3.0-or-later

import type {VoiceEngineV2AppSourceLifecycleBridge} from '@app/features/voice/engine/v2/VoiceEngineV2AppSourceLifecycleBridge';
import {
	armNativeSystemAudioForNextCapture,
	captureNativeAudioTrackForLinuxRouting,
	getNativeAudioCaptureDiagnosticState,
	resetNativeAudioCaptureBridgeForTests,
	setNativeAudioCaptureBridgeLifecycleBridge,
} from '@app/features/voice/utils/NativeAudioCaptureBridge';
import {recordBridgeFrame, startBridgeStats} from '@app/features/voice/utils/native_audio_capture_bridge/bridgeStats';
import type {
	NativeAudioBridgeEndedCapture,
	NativeAudioBridgeStats,
} from '@app/features/voice/utils/native_audio_capture_bridge/shared';
import {beforeEach, describe, expect, it, vi} from 'vitest';

const electronApi = {current: null as Record<string, unknown> | null};

vi.mock('@app/features/ui/utils/NativeUtils', () => ({
	getElectronAPI: () => electronApi.current,
}));

vi.mock('@app/features/voice/utils/SelfWindowScreenShareAudioMix', () => ({
	mixTrackWithSelfWindowScreenShareAudio: async (primaryTrack: MediaStreamTrack) => ({
		track: primaryTrack,
		cleanup: async () => undefined,
	}),
}));

const CAPTURE_ID = 'native-audio:stop-cause';

class FakeMediaStreamTrack extends EventTarget {
	readyState: 'live' | 'ended' = 'live';

	constructor(readonly kind: 'audio' | 'video') {
		super();
	}

	stop(): void {
		this.readyState = 'ended';
	}
}

class FakeGeneratorTrack extends FakeMediaStreamTrack {
	readonly writable = new WritableStream();

	constructor(_options: {kind: 'audio'}) {
		super('audio');
	}
}

class FakeMediaStream {
	private readonly tracks: Array<FakeMediaStreamTrack>;

	constructor(tracks: Array<FakeMediaStreamTrack> = []) {
		this.tracks = [...tracks];
	}

	getTracks(): Array<FakeMediaStreamTrack> {
		return [...this.tracks];
	}

	getVideoTracks(): Array<FakeMediaStreamTrack> {
		return this.tracks.filter((track) => track.kind === 'video');
	}

	getAudioTracks(): Array<FakeMediaStreamTrack> {
		return this.tracks.filter((track) => track.kind === 'audio');
	}

	addTrack(track: FakeMediaStreamTrack): void {
		this.tracks.push(track);
	}

	removeTrack(track: FakeMediaStreamTrack): void {
		const index = this.tracks.indexOf(track);
		if (index >= 0) this.tracks.splice(index, 1);
	}
}

const stopNativeCapture = vi.fn(async () => undefined);
const reportLifecycle = vi.fn(() => true);

function createNativeAudioApi() {
	return {
		getAvailability: async () => ({
			available: true,
			backend: 'windows-wasapi-loopback',
			capabilities: {process: true, system: true, systemExcludesSelf: true},
		}),
		start: async () => ({captureId: CAPTURE_ID, sampleRate: 48000, channels: 2}),
		stop: stopNativeCapture,
		onFrame: () => () => undefined,
		onEnd: () => () => undefined,
	};
}

function installLifecycleBridge(): void {
	setNativeAudioCaptureBridgeLifecycleBridge({
		bind: () => true,
		unbind: () => undefined,
		reportLifecycle,
	} as unknown as VoiceEngineV2AppSourceLifecycleBridge);
}

async function flushCleanup(): Promise<void> {
	await new Promise((resolve) => setTimeout(resolve, 0));
	await new Promise((resolve) => setTimeout(resolve, 0));
}

async function startDisplayShareWithNativeAudio(): Promise<{
	videoTrack: FakeMediaStreamTrack;
	audioTrack: FakeMediaStreamTrack;
}> {
	const videoTrack = new FakeMediaStreamTrack('video');
	const capturedStream = new FakeMediaStream([videoTrack]);
	Object.defineProperty(globalThis, 'navigator', {
		configurable: true,
		value: {mediaDevices: {getDisplayMedia: async () => capturedStream}},
	});
	expect(await armNativeSystemAudioForNextCapture()).toBe(true);
	const stream = (await navigator.mediaDevices.getDisplayMedia({video: true})) as unknown as FakeMediaStream;
	const audioTrack = stream.getAudioTracks()[0];
	expect(audioTrack).toBeInstanceOf(FakeGeneratorTrack);
	return {videoTrack, audioTrack};
}

function currentBridgeStats(): NativeAudioBridgeStats {
	return getNativeAudioCaptureDiagnosticState().bridgeStats as NativeAudioBridgeStats;
}

function currentLifecycleFaults(): Array<{message: string}> {
	return getNativeAudioCaptureDiagnosticState().lifecycleFaults as Array<{message: string}>;
}

describe('getNativeAudioCaptureDiagnosticState', () => {
	it('keeps the six fields real dumps already carry and appends the retained histories', () => {
		expect(Object.keys(getNativeAudioCaptureDiagnosticState())).toEqual([
			'armedCapture',
			'activeBridge',
			'supersededBridge',
			'lastStartedCapture',
			'lastArmFailure',
			'bridgeStats',
			'endedBridgeCaptures',
			'lifecycleFaults',
		]);
	});

	it('emits the superseded capture next to the live one instead of only the replacement', () => {
		startBridgeStats('generator', 'native-audio:diag-failed', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});
		recordBridgeFrame('native-audio:diag-failed', {timestampUs: 0, durationUs: 10_000, peak: 0.31, rms: 0.08});
		startBridgeStats('generator', 'native-audio:diag-live', {prebufferTargetUs: 60_000, frameDurationUs: 10_000});

		const state = getNativeAudioCaptureDiagnosticState();

		expect((state.bridgeStats as NativeAudioBridgeStats).captureId).toBe('native-audio:diag-live');
		const ended = state.endedBridgeCaptures as Array<NativeAudioBridgeEndedCapture>;
		const retained = ended.find((capture) => capture.captureId === 'native-audio:diag-failed');
		expect(retained?.framesReceived).toBe(1);
		expect(retained?.nonSilentFrameCount).toBe(1);
		expect(retained?.endReason).toBe('superseded');
	});

	it('exposes a native-audio fault history that outlives the lifecycle unbind', () => {
		expect(Array.isArray(getNativeAudioCaptureDiagnosticState().lifecycleFaults)).toBe(true);
	});
});

describe('native audio capture teardown cause', () => {
	beforeEach(() => {
		resetNativeAudioCaptureBridgeForTests();
		Object.defineProperty(globalThis, 'window', {
			configurable: true,
			value: {MediaStreamTrackGenerator: FakeGeneratorTrack, AudioData: class {}},
		});
		Object.defineProperty(globalThis, 'MediaStream', {configurable: true, value: FakeMediaStream});
		electronApi.current = {platform: 'win32', nativeAudio: createNativeAudioApi()};
		stopNativeCapture.mockClear();
		reportLifecycle.mockClear();
		installLifecycleBridge();
	});

	it('records caller-stopped with no fault when the share is stopped by hand', async () => {
		const {audioTrack} = await startDisplayShareWithNativeAudio();

		audioTrack.stop();
		await flushCleanup();

		expect(currentLifecycleFaults()).toEqual([]);
		expect(reportLifecycle).not.toHaveBeenCalled();
		expect(currentBridgeStats().endDetail).toBe('caller-stopped');
		expect(stopNativeCapture).toHaveBeenCalledTimes(1);
	});

	it('names the audio track when the capture track ends on its own', async () => {
		const {audioTrack} = await startDisplayShareWithNativeAudio();

		audioTrack.dispatchEvent(new Event('ended'));
		await flushCleanup();

		expect(currentLifecycleFaults().map((fault) => fault.message)).toEqual(['native-audio-tap-audio-track-ended']);
		expect(reportLifecycle).toHaveBeenCalledWith({
			captureId: CAPTURE_ID,
			kind: 'error',
			message: 'native-audio-tap-audio-track-ended',
		});
		expect(currentBridgeStats().endDetail).toBe('audio-track-ended');
	});

	it('names the video track when the shared screen stops sending frames', async () => {
		const {videoTrack} = await startDisplayShareWithNativeAudio();

		videoTrack.dispatchEvent(new Event('ended'));
		await flushCleanup();

		expect(currentLifecycleFaults().map((fault) => fault.message)).toEqual(['native-audio-tap-video-track-ended']);
		expect(currentBridgeStats().endDetail).toBe('video-track-ended');
	});

	it('tears down once and keeps the track-ended cause when a stop follows it', async () => {
		const {audioTrack} = await startDisplayShareWithNativeAudio();

		audioTrack.dispatchEvent(new Event('ended'));
		audioTrack.stop();
		await flushCleanup();

		expect(currentLifecycleFaults()).toHaveLength(1);
		expect(currentBridgeStats().endDetail).toBe('audio-track-ended');
		expect(stopNativeCapture).toHaveBeenCalledTimes(1);
	});

	it('names the audio track when a capture mixed with self-window audio ends on its own', async () => {
		electronApi.current = {platform: 'linux', nativeAudio: createNativeAudioApi()};
		const track = (await captureNativeAudioTrackForLinuxRouting(
			{include: []},
			{includeSelfWindowAudio: true},
		)) as unknown as FakeMediaStreamTrack | null;
		expect(track).toBeInstanceOf(FakeGeneratorTrack);

		track?.dispatchEvent(new Event('ended'));
		await flushCleanup();

		expect(currentBridgeStats().endDetail).toBe('audio-track-ended');
	});

	it('tears down once and keeps caller-stopped when the track ends after the stop', async () => {
		const {audioTrack} = await startDisplayShareWithNativeAudio();

		audioTrack.stop();
		audioTrack.dispatchEvent(new Event('ended'));
		await flushCleanup();

		expect(currentLifecycleFaults()).toEqual([]);
		expect(currentBridgeStats().endDetail).toBe('caller-stopped');
		expect(stopNativeCapture).toHaveBeenCalledTimes(1);
	});
});
