// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {describe, expect, it} from 'vitest';
import type {InternalRoomOptions} from '../options.ts';
import {EngineEvent} from './events.ts';
import RTCEngine, {selectPublisherCodecPreferences} from './RTCEngine.ts';

function codec(
	mimeType: string,
	sdpFmtpLine?: string,
): RTCRtpCapabilities['codecs'][number] & {
	sdpFmtpLine?: string;
} {
	return {
		mimeType,
		clockRate: 90000,
		...(sdpFmtpLine ? {sdpFmtpLine} : {}),
	};
}

describe('selectPublisherCodecPreferences', () => {
	const constrainedBaselineLine = 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f';
	const baselineLine = 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f';
	const mainLine = 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f';
	const highLine = 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=64001f';

	it('offers High first, then the one profile this server always registers, then the ones it registers nowhere', () => {
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const baseline = codec('video/H264', baselineLine);
		const highProfile = codec('video/H264', highLine);
		const rtx = codec('video/rtx');
		const preferences = selectPublisherCodecPreferences('h264', [constrainedBaseline, rtx, baseline, highProfile]);
		expect(preferences).toEqual([highProfile, constrainedBaseline, baseline, rtx]);
	});

	it('never leads with Main or Baseline, which this server registers nowhere and deletes from the answer', () => {
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const mainProfile = codec('video/H264', mainLine);
		const baseline = codec('video/H264', baselineLine);
		const preferences = selectPublisherCodecPreferences('h264', [mainProfile, baseline, constrainedBaseline]);
		expect(preferences).toEqual([constrainedBaseline, mainProfile, baseline]);
	});

	function chromiumCapabilities() {
		return [
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42001f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42e01f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=4d001f'),
			codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640034'),
		];
	}

	it('offers High first out of the capabilities Chromium reports, so the only hardware profile this server registers wins', () => {
		const preferences = selectPublisherCodecPreferences('h264', chromiumCapabilities());
		expect(preferences.map((entry) => entry.sdpFmtpLine)).toEqual([
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640034',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42e01f',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=4d001f',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42001f',
		]);
	});

	function levelSpread() {
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const mainProfile = codec('video/H264', mainLine);
		const highProfileLevel31 = codec('video/H264', highLine);
		const highProfileLevel51 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033',
		);
		const constrainedHigh = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640c1f',
		);
		return {constrainedBaseline, mainProfile, highProfileLevel31, highProfileLevel51, constrainedHigh};
	}

	it('ranks High and Constrained High above Constrained Baseline, whatever level each one reports', () => {
		const {constrainedBaseline, mainProfile, highProfileLevel31, highProfileLevel51, constrainedHigh} = levelSpread();
		const preferences = selectPublisherCodecPreferences('h264', [
			mainProfile,
			highProfileLevel31,
			highProfileLevel51,
			constrainedHigh,
			constrainedBaseline,
		]);
		expect(preferences).toEqual([
			highProfileLevel31,
			highProfileLevel51,
			constrainedHigh,
			constrainedBaseline,
			mainProfile,
		]);
	});

	it('ranks packetization-mode=1 above packetization-mode=0, which no hardware encoder takes', () => {
		const constrainedBaselineMode0 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42e01f',
		);
		const constrainedBaselineMode1 = codec('video/H264', constrainedBaselineLine);
		const highProfileMode0 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=640033',
		);
		const highProfileMode1 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033',
		);
		const capabilities = [highProfileMode0, constrainedBaselineMode0, constrainedBaselineMode1, highProfileMode1];
		const expected = [highProfileMode1, constrainedBaselineMode1, highProfileMode0, constrainedBaselineMode0];
		expect(selectPublisherCodecPreferences('h264', capabilities)).toEqual(expected);
	});

	it('puts the chosen codec first and keeps every other codec in browser capability order', () => {
		const vp9 = codec('video/VP9');
		const vp8 = codec('video/VP8');
		const rtx = codec('video/rtx');
		expect(selectPublisherCodecPreferences('vp9', [vp8, rtx, vp9])).toEqual([vp9, vp8, rtx]);
	});

	it('keeps the other codecs so a later publication on the same connection can negotiate them', () => {
		const vp9 = codec('video/VP9');
		const av1 = codec('video/AV1');
		const h264 = codec('video/H264', constrainedBaselineLine);
		const preferences = selectPublisherCodecPreferences('vp9', [av1, h264, vp9]);
		expect(preferences.map((entry) => entry.mimeType)).toEqual(['video/VP9', 'video/AV1', 'video/H264']);
	});

	it('ranks the H.264 profiles it leaves behind the chosen codec', () => {
		const vp8 = codec('video/VP8');
		const highProfile = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033');
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const capabilities = [vp8, highProfile, constrainedBaseline];
		expect(selectPublisherCodecPreferences('vp8', capabilities)).toEqual([vp8, highProfile, constrainedBaseline]);
		expect(selectPublisherCodecPreferences('vp8', capabilities, new Set(['42e0']))).toEqual([
			vp8,
			constrainedBaseline,
			highProfile,
		]);
	});

	it('returns nothing when the sender cannot encode the chosen codec', () => {
		expect(selectPublisherCodecPreferences('av1', [codec('video/VP8'), codec('video/rtx')])).toEqual([]);
	});

	it('only lets a profile this host encodes in hardware outrank Constrained Baseline', () => {
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const highProfile = codec('video/H264', highLine);
		const capabilities = [highProfile, constrainedBaseline];
		expect(selectPublisherCodecPreferences('h264', capabilities, new Set(['42e0']))).toEqual([
			constrainedBaseline,
			highProfile,
		]);
		expect(selectPublisherCodecPreferences('h264', capabilities, new Set(['6400', '42e0']))).toEqual([
			highProfile,
			constrainedBaseline,
		]);
	});

	it('falls back to the static order when nothing was measured or nothing is hardware', () => {
		const constrainedBaseline = codec('video/H264', constrainedBaselineLine);
		const highProfile = codec('video/H264', highLine);
		const capabilities = [constrainedBaseline, highProfile];
		expect(selectPublisherCodecPreferences('h264', capabilities)).toEqual([highProfile, constrainedBaseline]);
		expect(selectPublisherCodecPreferences('h264', capabilities, new Set())).toEqual([
			highProfile,
			constrainedBaseline,
		]);
	});

	it('keeps packetization-mode=1 ahead of a hardware packetization-mode=0 profile', () => {
		const constrainedBaselineMode1 = codec('video/H264', constrainedBaselineLine);
		const highProfileMode0 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=64001f',
		);
		expect(
			selectPublisherCodecPreferences('h264', [highProfileMode0, constrainedBaselineMode1], new Set(['6400'])),
		).toEqual([constrainedBaselineMode1, highProfileMode0]);
	});
});

describe('publisher data channels before negotiation', () => {
	function engineWithPublisherChannels(hasPublisherChannels: boolean) {
		const engine = new RTCEngine({} as InternalRoomOptions);
		const created: Array<string> = [];
		const internals = engine as unknown as {
			_isClosed: boolean;
			pcManager: unknown;
			dataChannels: {hasPublisherChannels: boolean; createPublisherChannels: () => void};
		};
		internals._isClosed = false;
		internals.dataChannels = {
			hasPublisherChannels,
			createPublisherChannels: () => created.push('publisher'),
		};
		internals.pcManager = {
			requirePublisher: () => {},
			negotiate: async () => {},
			publisher: {
				off: () => {},
				once: () => {},
				getTransceivers: () => [{} as RTCRtpTransceiver],
			},
		};
		return {engine, created};
	}

	it('creates them on a renegotiation that already carries transceivers', async () => {
		const {engine, created} = engineWithPublisherChannels(false);
		await engine.negotiate();
		expect(created).toEqual(['publisher']);
	});

	it('leaves the existing channels alone', async () => {
		const {engine, created} = engineWithPublisherChannels(true);
		await engine.negotiate();
		expect(created).toEqual([]);
	});
});

describe('negotiation aborts', () => {
	function negotiableEngine() {
		const engine = new RTCEngine({} as InternalRoomOptions);
		const controllers: Array<AbortController> = [];
		const internals = engine as unknown as {
			_isClosed: boolean;
			pcManager: unknown;
			dataChannels: {hasPublisherChannels: boolean};
			pendingNegotiationAborts: Set<() => void>;
		};
		internals._isClosed = false;
		internals.dataChannels = {hasPublisherChannels: true};
		internals.pcManager = {
			requirePublisher: () => {},
			publisher: {
				off: () => {},
				once: () => {},
				getTransceivers: () => [{} as RTCRtpTransceiver],
			},
			negotiate: (abortController: AbortController) =>
				new Promise<void>((_resolve, reject) => {
					controllers.push(abortController);
					abortController.signal.addEventListener('abort', () => reject(new Error('negotiation aborted')), {
						once: true,
					});
				}),
		};
		return {engine, controllers, pendingAborts: internals.pendingNegotiationAborts};
	}

	it('keeps one listener pair however many negotiations are in flight, and aborts every one', async () => {
		const {engine, controllers, pendingAborts} = negotiableEngine();
		const closingListeners = engine.listenerCount(EngineEvent.Closing);
		const restartingListeners = engine.listenerCount(EngineEvent.Restarting);

		const pending = Array.from({length: 20}, () => engine.negotiate());
		expect(engine.listenerCount(EngineEvent.Closing)).toBe(closingListeners);
		expect(engine.listenerCount(EngineEvent.Restarting)).toBe(restartingListeners);
		expect(pendingAborts.size).toBe(20);

		engine.emit(EngineEvent.Closing);
		await expect(Promise.all(pending)).resolves.toHaveLength(20);
		expect(controllers.every((controller) => controller.signal.aborted)).toBe(true);
		expect(pendingAborts.size).toBe(0);
	});

	it('re-arms after a restart so a later negotiation is not aborted on arrival', async () => {
		const {engine, controllers, pendingAborts} = negotiableEngine();
		const firstBatch = [engine.negotiate(), engine.negotiate()];
		engine.emit(EngineEvent.Restarting);
		await expect(Promise.all(firstBatch)).resolves.toHaveLength(2);

		const afterRestart = engine.negotiate();
		expect(controllers).toHaveLength(3);
		expect(controllers[2].signal.aborted).toBe(false);
		expect(pendingAborts.size).toBe(1);

		engine.emit(EngineEvent.Restarting);
		await expect(afterRestart).resolves.toBeUndefined();
		expect(pendingAborts.size).toBe(0);
	});
});
