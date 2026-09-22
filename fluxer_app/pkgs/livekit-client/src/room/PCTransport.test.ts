// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {MediaDescription} from 'sdp-transform';
import {parse} from 'sdp-transform';
import {describe, expect, it} from 'vitest';
import type {TrackBitrateInfo} from './PCTransport.ts';
import {
	applyVideoStartBitrate,
	collectStereoMids,
	ensureAudioNackAndStereo,
	ensureOpusFmtp,
	ensureVideoDDExtension,
	placeholderMidsFromTransceivers,
	videoSectionCanReceiveAV1,
} from './PCTransport.ts';
import {ddExtensionURI} from './utils.ts';

function videoMedia(
	trackId: string,
	fmtp: Array<{payload: number; config: string}>,
	rtp: Array<{payload: number; codec: string}> = [{payload: 96, codec: 'H264'}],
): MediaDescription {
	return {type: 'video', msid: `stream ${trackId}`, rtp, fmtp} as unknown as MediaDescription;
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

describe('applyVideoStartBitrate', () => {
	function multiPayloadScreenMedia(): MediaDescription {
		return videoMedia(
			'screen-track',
			[
				{payload: 116, config: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f'},
				{payload: 102, config: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f'},
				{payload: 108, config: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f'},
			],
			[
				{payload: 116, codec: 'H264'},
				{payload: 117, codec: 'rtx'},
				{payload: 102, codec: 'H264'},
				{payload: 108, codec: 'H264'},
			],
		);
	}

	it('adds a start bitrate to a non-SVC codec section', () => {
		for (const screenShareDelivery of [false, true]) {
			const media = videoMedia('camera-track', [
				{payload: 96, config: 'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f'},
			]);
			expect(applyVideoStartBitrate(media, 'camera-track', 'H264', 1000, false, screenShareDelivery)).toBe(96);
			expect(media.fmtp[0]?.config).toBe(
				'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f;x-google-start-bitrate=900',
			);
		}
	});

	it('caps camera start bitrates but not screen share start bitrates while screen share delivery is off', () => {
		const camera = videoMedia('camera-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(camera, 'camera-track', 'H264', 3000);
		expect(camera.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=1000');

		const screen = videoMedia('screen-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(screen, 'screen-track', 'H264', 6000, true);
		expect(screen.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=5400');
	});

	it('caps camera and screen share start bitrates at their own ceilings', () => {
		const camera = videoMedia('camera-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(camera, 'camera-track', 'H264', 3000, false, true);
		expect(camera.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=1000');

		const screen = videoMedia('screen-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(screen, 'screen-track', 'H264', 6000, true, true);
		expect(screen.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=1500');
	});

	it('leaves a small screen share start bitrate on the floor while screen share delivery is off', () => {
		const screen = videoMedia('screen-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(screen, 'screen-track', 'H264', 300, true);
		expect(screen.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=270');
	});

	it('keeps a screen share start bitrate above the frame dropper cliff', () => {
		const screen = videoMedia('screen-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
		applyVideoStartBitrate(screen, 'screen-track', 'H264', 300, true, true);
		expect(screen.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=600');
	});

	it('stamps only the lead payload type while screen share delivery is off', () => {
		const media = multiPayloadScreenMedia();
		expect(applyVideoStartBitrate(media, 'screen-track', 'H264', 6000, true)).toBe(116);
		expect(media.fmtp.find((fmtp) => fmtp.payload === 116)?.config).toBe(
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=4d001f;x-google-start-bitrate=5400',
		);
		expect(media.fmtp.find((fmtp) => fmtp.payload === 102)?.config).toBe(
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42001f',
		);
		expect(media.fmtp.find((fmtp) => fmtp.payload === 108)?.config).toBe(
			'level-asymmetry-allowed=1;packetization-mode=1;profile-level-id=42e01f',
		);
	});

	it('stamps every payload type the codec is offered under, not only the lead one', () => {
		const media = multiPayloadScreenMedia();
		expect(applyVideoStartBitrate(media, 'screen-track', 'H264', 6000, true, true)).toBe(116);
		for (const fmtp of media.fmtp) {
			expect(fmtp.config).toContain('x-google-start-bitrate=1500');
		}
		expect(media.fmtp.some((fmtp) => fmtp.payload === 117)).toBe(false);
	});

	it('only touches the fmtp line for the matching payload', () => {
		for (const screenShareDelivery of [false, true]) {
			const media = videoMedia(
				'screen-track',
				[
					{payload: 96, config: 'profile-level-id=42e01f'},
					{payload: 98, config: 'profile-id=0'},
				],
				[
					{payload: 96, codec: 'H264'},
					{payload: 98, codec: 'VP9'},
				],
			);
			applyVideoStartBitrate(media, 'screen-track', 'VP9', 1500, true, screenShareDelivery);
			expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f');
			expect(media.fmtp[1]?.config).toBe('profile-id=0;x-google-start-bitrate=1350');
		}
	});

	it('never appends a second start bitrate while screen share delivery is off', () => {
		const media = videoMedia('camera-track', [
			{payload: 96, config: 'profile-level-id=42e01f;x-google-start-bitrate=900'},
		]);
		applyVideoStartBitrate(media, 'camera-track', 'H264', 2000);
		expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=900');
	});

	it('replaces a start bitrate an earlier offer wrote instead of keeping it', () => {
		const media = videoMedia('camera-track', [
			{payload: 96, config: 'profile-level-id=42e01f;x-google-start-bitrate=900'},
		]);
		applyVideoStartBitrate(media, 'camera-track', 'H264', 2000, false, true);
		expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f;x-google-start-bitrate=1000');
	});

	it('leaves other tracks and missing codecs alone', () => {
		for (const screenShareDelivery of [false, true]) {
			const media = videoMedia('camera-track', [{payload: 96, config: 'profile-level-id=42e01f'}]);
			expect(applyVideoStartBitrate(media, 'other-track', 'H264', 2000, false, screenShareDelivery)).toBeUndefined();
			expect(applyVideoStartBitrate(media, 'camera-track', 'AV1', 2000, false, screenShareDelivery)).toBe(0);
			expect(media.fmtp[0]?.config).toBe('profile-level-id=42e01f');
		}
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

const singlePcOffer = `v=0
o=- 0 0 IN IP4 127.0.0.1
s=-
t=0 0
a=group:BUNDLE 0 1 2
m=video 9 UDP/TLS/RTP/SAVPF 96 45
c=IN IP4 0.0.0.0
a=mid:0
a=sendonly
a=msid:s cam
a=rtpmap:96 VP8/90000
a=rtpmap:45 AV1/90000
a=extmap:2 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time
a=extmap:11 urn:ietf:params:rtp-hdrext:sdes:repaired-rtp-stream-id
m=video 9 UDP/TLS/RTP/SAVPF 96 45
c=IN IP4 0.0.0.0
a=mid:1
a=recvonly
a=rtpmap:96 VP8/90000
a=rtpmap:45 AV1/90000
a=extmap:2 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time
m=video 9 UDP/TLS/RTP/SAVPF 96
c=IN IP4 0.0.0.0
a=mid:2
a=recvonly
a=rtpmap:96 VP8/90000
a=extmap:2 http://www.webrtc.org/experiments/rtp-hdrext/abs-send-time`;

const sectionOf = (sdp: ReturnType<typeof parse>, mid: string) => sdp.media.find((media) => `${media.mid}` === mid)!;
const ddOf = (sdp: ReturnType<typeof parse>, mid: string) =>
	sectionOf(sdp, mid).ext?.find((ext) => ext.uri === ddExtensionURI)?.value;

describe('videoSectionCanReceiveAV1', () => {
	it('is true for a section we receive on that kept AV1, false otherwise', () => {
		const sdp = parse(singlePcOffer);
		expect(videoSectionCanReceiveAV1(sectionOf(sdp, '1'))).toBe(true);
		expect(videoSectionCanReceiveAV1(sectionOf(sdp, '0'))).toBe(false);
		expect(videoSectionCanReceiveAV1(sectionOf(sdp, '2'))).toBe(false);
	});
});

describe('ensureVideoDDExtension', () => {
	it('assigns an id above every extension in the bundle and reuses it', () => {
		const sdp = parse(singlePcOffer);
		expect(ensureVideoDDExtension(sectionOf(sdp, '1'), sdp, 0)).toBe(12);
		expect(ddOf(sdp, '1')).toBe(12);
		expect(ensureVideoDDExtension(sectionOf(sdp, '2'), sdp, 12)).toBe(12);
		expect(ddOf(sdp, '2')).toBe(12);
	});

	it('adopts an id the bundle already maps the extension to', () => {
		const sdp = parse(singlePcOffer);
		sectionOf(sdp, '0').ext!.push({value: 13, uri: ddExtensionURI});
		expect(ensureVideoDDExtension(sectionOf(sdp, '1'), sdp, 7)).toBe(13);
		expect(ddOf(sdp, '1')).toBe(13);
	});

	it('leaves a section that already carries the extension alone', () => {
		const sdp = parse(`${singlePcOffer}\na=extmap:3 ${ddExtensionURI}`);
		expect(ensureVideoDDExtension(sectionOf(sdp, '2'), sdp, 0)).toBe(3);
		expect(sectionOf(sdp, '2').ext).toHaveLength(2);
	});
});

describe('placeholderMidsFromTransceivers', () => {
	function transceiver(
		mid: string | null,
		track: MediaStreamTrack | null,
		currentDirection: RTCRtpTransceiverDirection | null,
	): RTCRtpTransceiver {
		return {mid, currentDirection, sender: {track}} as unknown as RTCRtpTransceiver;
	}

	it('keeps the trackless recvonly sections that still hold an m-line', () => {
		const mids = placeholderMidsFromTransceivers([
			transceiver('3', null, 'recvonly'),
			transceiver('7', {} as MediaStreamTrack, 'sendonly'),
		]);
		expect(mids).toEqual(new Set(['3']));
	});

	it('drops a transceiver that unpublish stopped so its recycled m-section is not fmtp-conformed', () => {
		expect(placeholderMidsFromTransceivers([transceiver('7', null, 'stopped')])).toEqual(new Set());
	});
});
