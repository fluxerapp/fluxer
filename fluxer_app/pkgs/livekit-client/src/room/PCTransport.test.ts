// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {MediaDescription} from 'sdp-transform';
import {describe, expect, it} from 'vitest';
import type {TrackBitrateInfo} from './PCTransport.ts';
import {appendStartBitrateToFmtp, collectStereoMids, ensureAudioNackAndStereo, ensureOpusFmtp} from './PCTransport.ts';

function mediaWithFmtp(entries: Array<{payload: number; config: string}>): MediaDescription {
	return {fmtp: entries} as unknown as MediaDescription;
}

function opusMedia(config: string, mid = '0'): MediaDescription {
	return {
		type: 'audio',
		mid,
		port: 9,
		protocol: 'UDP/TLS/RTP/SAVPF',
		rtp: [{payload: 109, codec: 'opus', rate: 48000, encoding: 2}],
		fmtp: [{payload: 109, config}],
	} as unknown as MediaDescription;
}

function offerMedia(mid: string, trackId: string): MediaDescription {
	const media = opusMedia('useinbandfec=1', mid);
	media.msid = `- ${trackId}`;
	return media;
}

function audioBitrateInfo(mid: string | null, trackId: string, stereo: boolean): TrackBitrateInfo {
	return {
		transceiver: {mid, sender: {track: {id: trackId}}} as unknown as RTCRtpTransceiver,
		codec: 'opus',
		maxbr: 320,
		stereo,
	};
}

function opusConfig(media: MediaDescription): string {
	return media.fmtp.find((fmtp) => fmtp.payload === 109)?.config ?? '';
}

describe('appendStartBitrateToFmtp', () => {
	it('appends the start bitrate to a non-SVC codec fmtp line', () => {
		const media = mediaWithFmtp([
			{payload: 96, config: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f'},
		]);
		appendStartBitrateToFmtp(media, 96, 2100);
		expect(media.fmtp[0]?.config).toBe(
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f;x-google-start-bitrate=2100',
		);
	});

	it('only touches the fmtp line for the matching payload', () => {
		const media = mediaWithFmtp([
			{payload: 96, config: 'profile-level-id=42e01f'},
			{payload: 98, config: 'profile-id=0'},
		]);
		appendStartBitrateToFmtp(media, 98, 1400);
		expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f');
		expect(media.fmtp[1]?.config).toBe('profile-id=0;x-google-start-bitrate=1400');
	});

	it('never appends a second start bitrate', () => {
		const media = mediaWithFmtp([{payload: 96, config: 'profile-level-id=42e01f;x-google-start-bitrate=900'}]);
		appendStartBitrateToFmtp(media, 96, 2100);
		expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=900');
	});

	it('does nothing without a usable bitrate or a matching payload', () => {
		const media = mediaWithFmtp([{payload: 96, config: 'profile-level-id=42e01f'}]);
		appendStartBitrateToFmtp(media, 96, 0);
		appendStartBitrateToFmtp(media, 96, -1);
		appendStartBitrateToFmtp(media, 111, 2100);
		expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f');
	});
});

describe('ensureOpusFmtp', () => {
	it('does not force stereo on a mono publication', () => {
		const media = opusMedia('maxplaybackrate=48000;stereo=0;useinbandfec=1');
		ensureOpusFmtp(media, 48000, false);
		const config = opusConfig(media);
		expect(config).toContain('minptime=10');
		expect(config).toContain('useinbandfec=1');
		expect(config).toContain('usedtx=0');
		expect(config).toContain('maxaveragebitrate=48000');
		expect(config).not.toContain('stereo=1');
	});

	it('keeps stereo for a stereo publication', () => {
		const media = opusMedia('maxplaybackrate=48000;useinbandfec=1');
		ensureOpusFmtp(media, 320000, true);
		const config = opusConfig(media);
		expect(config).toContain('stereo=1');
		expect(config).toContain('sprop-stereo=1');
		expect(config).toContain('maxaveragebitrate=320000');
	});

	it('preserves a stereo parameter the server negotiated', () => {
		const media = opusMedia('minptime=10;stereo=1');
		ensureOpusFmtp(media, 48000, false);
		expect(opusConfig(media)).toContain('stereo=1');
	});
});

describe('ensureAudioNackAndStereo', () => {
	it('only stamps stereo on the listed mids', () => {
		const mono = opusMedia('useinbandfec=1', '0');
		ensureAudioNackAndStereo(mono as never, ['1'], []);
		expect(opusConfig(mono)).not.toContain('stereo=1');

		const stereo = opusMedia('useinbandfec=1', '1');
		ensureAudioNackAndStereo(stereo as never, ['1'], []);
		expect(opusConfig(stereo)).toContain('stereo=1');
		expect(opusConfig(stereo)).toContain('sprop-stereo=1');
	});
});

describe('collectStereoMids', () => {
	it('matches the offer media section by msid before the transceiver has a mid', () => {
		const media = [offerMedia('0', 'mic-track'), offerMedia('1', 'screenshare-track')];
		expect(collectStereoMids([audioBitrateInfo(null, 'screenshare-track', true)], media)).toEqual(['1']);
	});

	it('stamps stereo on the first offer for a new stereo publication', () => {
		const media = [offerMedia('0', 'mic-track'), offerMedia('1', 'screenshare-track')];
		const stereoMids = collectStereoMids([audioBitrateInfo(null, 'screenshare-track', true)], media);
		for (const m of media) {
			ensureAudioNackAndStereo(m as never, stereoMids, []);
		}
		expect(opusConfig(media[0]!)).not.toContain('stereo=1');
		expect(opusConfig(media[1]!)).toContain('stereo=1');
		expect(opusConfig(media[1]!)).toContain('sprop-stereo=1');
	});

	it('uses the assigned mid once renegotiation has one', () => {
		const media = [offerMedia('0', 'mic-track'), offerMedia('1', 'screenshare-track')];
		expect(collectStereoMids([audioBitrateInfo('1', 'screenshare-track', true)], media)).toEqual(['1']);
	});

	it('leaves mono publications out', () => {
		const media = [offerMedia('0', 'mic-track')];
		expect(collectStereoMids([audioBitrateInfo(null, 'mic-track', false)], media)).toEqual([]);
	});
});
