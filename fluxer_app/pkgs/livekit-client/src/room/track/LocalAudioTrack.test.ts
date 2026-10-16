// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';
import LocalAudioTrack from './LocalAudioTrack.ts';
import type {AudioProcessorOptions, TrackProcessor} from './processor/types.ts';
import type {Track} from './Track.ts';

let nextTrackId = 0;

class FakeMediaStreamTrack {
	readonly id = `track-${nextTrackId++}`;
	readonly kind = 'audio';
	enabled = true;
	muted = false;
	readyState: MediaStreamTrackState = 'live';

	getConstraints(): MediaTrackConstraints {
		return {};
	}

	getSettings(): MediaTrackSettings {
		return {};
	}

	async applyConstraints(): Promise<void> {}

	addEventListener(): void {}

	removeEventListener(): void {}

	stop(): void {
		this.readyState = 'ended';
	}
}

interface SenderHandoff {
	track: MediaStreamTrack | null;
	enabled: boolean | null;
}

class FakeSender {
	track: MediaStreamTrack | null = null;
	readonly handoffs: Array<SenderHandoff> = [];
	duringReplace: (() => Promise<unknown>) | null = null;

	async replaceTrack(track: MediaStreamTrack | null): Promise<void> {
		this.handoffs.push({track, enabled: track?.enabled ?? null});
		const duringReplace = this.duringReplace;
		this.duringReplace = null;
		await duringReplace?.();
		this.track = track;
	}
}

class FakeProcessor implements TrackProcessor<Track.Kind.Audio, AudioProcessorOptions> {
	name = 'fake-processor';
	processedTrack?: MediaStreamTrack;
	readonly inputs: Array<MediaStreamTrack> = [];

	async init(opts: AudioProcessorOptions): Promise<void> {
		this.inputs.push(opts.track);
		this.processedTrack = new FakeMediaStreamTrack() as unknown as MediaStreamTrack;
	}

	async restart(opts: AudioProcessorOptions): Promise<void> {
		this.inputs.push(opts.track);
	}

	async destroy(): Promise<void> {}
}

function asMediaStreamTrack(track: FakeMediaStreamTrack): MediaStreamTrack {
	return track as unknown as MediaStreamTrack;
}

async function createPublishedTrack() {
	const raw = new FakeMediaStreamTrack();
	const track = new LocalAudioTrack(asMediaStreamTrack(raw), undefined, false, {} as AudioContext);
	await track.runWithTrackChangeLock(async () => {});
	const sender = new FakeSender();
	track.sender = sender as unknown as RTCRtpSender;
	await sender.replaceTrack(asMediaStreamTrack(raw));
	sender.handoffs.length = 0;
	return {raw, track, sender};
}

async function installProcessor(track: LocalAudioTrack): Promise<FakeProcessor> {
	const processor = new FakeProcessor();
	await track.setProcessor(processor);
	return processor;
}

describe('LocalAudioTrack mute with a processor', () => {
	beforeEach(() => {
		vi.stubGlobal(
			'MediaStream',
			class {
				constructor(readonly tracks: Array<MediaStreamTrack>) {}
			},
		);
	});

	afterEach(() => {
		vi.unstubAllGlobals();
	});

	it('keeps the raw capture enabled and disables the processed track on mute', async () => {
		const {raw, track} = await createPublishedTrack();
		const processor = await installProcessor(track);
		await track.mute();
		expect(raw.enabled).toBe(true);
		expect(processor.processedTrack?.enabled).toBe(false);
	});

	it('re-enables the processed track on unmute', async () => {
		const {raw, track} = await createPublishedTrack();
		const processor = await installProcessor(track);
		await track.mute();
		await track.unmute();
		expect(raw.enabled).toBe(true);
		expect(processor.processedTrack?.enabled).toBe(true);
	});

	it('disables the raw capture on mute without a processor', async () => {
		const {raw, track} = await createPublishedTrack();
		await track.mute();
		expect(raw.enabled).toBe(false);
	});

	it('hands the sender a disabled processed track when the processor is installed while muted', async () => {
		const {raw, track, sender} = await createPublishedTrack();
		await track.mute();
		const processor = await installProcessor(track);
		expect(sender.handoffs).toEqual([{track: processor.processedTrack, enabled: false}]);
		expect(raw.enabled).toBe(true);
		expect(processor.processedTrack?.enabled).toBe(false);
	});

	it('ends muted when a mute lands while the processed track is being handed to the sender', async () => {
		const {raw, track, sender} = await createPublishedTrack();
		sender.duringReplace = async () => {
			await track.mute();
			expect(raw.enabled).toBe(false);
		};
		const processor = await installProcessor(track);
		expect(track.isMuted).toBe(true);
		expect(sender.track).toBe(processor.processedTrack);
		expect(processor.processedTrack?.enabled).toBe(false);
		expect(raw.enabled).toBe(true);
	});

	it('never hands an enabled track to the sender when the processor stops while muted', async () => {
		const {raw, track, sender} = await createPublishedTrack();
		await installProcessor(track);
		await track.mute();
		sender.handoffs.length = 0;
		await track.stopProcessor();
		expect(sender.handoffs.length).toBeGreaterThan(0);
		expect(sender.handoffs.every((handoff) => handoff.enabled !== true)).toBe(true);
		expect(sender.track).toBe(asMediaStreamTrack(raw));
		expect(raw.enabled).toBe(false);
	});

	it('keeps the new raw capture enabled and the sender track disabled when restarting while muted', async () => {
		const {track, sender} = await createPublishedTrack();
		const processor = await installProcessor(track);
		await track.mute();
		sender.handoffs.length = 0;
		const restarted = new FakeMediaStreamTrack();
		vi.stubGlobal('navigator', {
			mediaDevices: {getUserMedia: async () => ({getTracks: () => [restarted]})},
		});
		await track.restartTrack();
		expect(processor.inputs.at(-1)).toBe(asMediaStreamTrack(restarted));
		expect(restarted.enabled).toBe(true);
		expect(sender.handoffs).toEqual([{track: processor.processedTrack, enabled: false}]);
		expect(processor.processedTrack?.enabled).toBe(false);
	});
});
