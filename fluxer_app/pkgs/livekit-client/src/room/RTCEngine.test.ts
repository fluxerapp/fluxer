// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {describe, expect, it} from 'vitest';
import type {InternalRoomOptions} from '../options.ts';
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
	it('prefers H.264 profiles that use Chromium external encoders before OpenH264', () => {
		const openH264 = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f');
		const externalBaseline = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f',
		);
		const highProfile = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=64001f');
		const rtx = codec('video/rtx');
		const preferences = selectPublisherCodecPreferences('h264', [openH264, rtx, externalBaseline, highProfile]);
		expect(preferences).toEqual([externalBaseline, openH264, highProfile, rtx]);
	});

	it('ranks Constrained Baseline above Main, High and Constrained High', () => {
		const constrainedBaseline = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		);
		const mainProfile = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f');
		const highProfileLevel31 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=64001f',
		);
		const highProfileLevel51 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033',
		);
		const constrainedHigh = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640c1f',
		);
		const preferences = selectPublisherCodecPreferences('h264', [
			mainProfile,
			highProfileLevel31,
			highProfileLevel51,
			constrainedHigh,
			constrainedBaseline,
		]);
		expect(preferences).toEqual([
			constrainedBaseline,
			mainProfile,
			highProfileLevel31,
			highProfileLevel51,
			constrainedHigh,
		]);
	});

	it('keeps Constrained Baseline packetization-mode=1 ahead of Constrained Baseline packetization-mode=0 and High', () => {
		const constrainedBaselineMode0 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=0;profile-level-id=42e01f',
		);
		const constrainedBaselineMode1 = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		);
		const highProfile = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033');
		const preferences = selectPublisherCodecPreferences('h264', [
			highProfile,
			constrainedBaselineMode0,
			constrainedBaselineMode1,
		]);
		expect(preferences).toEqual([constrainedBaselineMode1, constrainedBaselineMode0, highProfile]);
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
		const h264 = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f');
		const preferences = selectPublisherCodecPreferences('vp9', [av1, h264, vp9]);
		expect(preferences.map((entry) => entry.mimeType)).toEqual(['video/VP9', 'video/AV1', 'video/H264']);
	});

	it('ranks the H.264 profiles it leaves behind the chosen codec', () => {
		const vp8 = codec('video/VP8');
		const highProfile = codec('video/H264', 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=640033');
		const constrainedBaseline = codec(
			'video/H264',
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		);
		const preferences = selectPublisherCodecPreferences('vp8', [vp8, highProfile, constrainedBaseline]);
		expect(preferences).toEqual([vp8, constrainedBaseline, highProfile]);
	});

	it('returns nothing when the sender cannot encode the chosen codec', () => {
		expect(selectPublisherCodecPreferences('av1', [codec('video/VP8'), codec('video/rtx')])).toEqual([]);
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
