// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import type {MediaDescription} from 'sdp-transform';
import {describe, expect, it} from 'vitest';
import {appendStartBitrateToFmtp} from './PCTransport.ts';

function mediaWithFmtp(entries: Array<{payload: number; config: string}>): MediaDescription {
	return {fmtp: entries} as unknown as MediaDescription;
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
