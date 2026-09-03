// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {describe, expect, it} from 'vitest';
import {defaultVideoCodec, publishDefaults, roomOptionDefaults} from './defaults.ts';
import {BackupCodecPolicy} from './track/options.ts';
import {selectPreferredVideoCodec} from './utils.ts';

describe('Fluxer media publish defaults', () => {
	it('defaults new video publishes to an opt-in-safe codec with H.264 backup', () => {
		expect(defaultVideoCodec).toBe('h264');
		expect(publishDefaults).toMatchObject({
			videoCodec: 'h264',
			backupCodec: {codec: 'h264'},
			backupCodecPolicy: BackupCodecPolicy.SIMULCAST,
			degradationPreference: 'maintain-resolution',
			dtx: false,
			red: true,
		});
	});

	it('falls back away from the opt-in codecs before reaching them', () => {
		expect(selectPreferredVideoCodec()).toBe('h264');
		expect(selectPreferredVideoCodec()).not.toBe('av1');
		expect(selectPreferredVideoCodec()).not.toBe('h265');
	});

	it('keeps original screen-share publishing at a 4K60-ready transport ceiling', () => {
		expect(publishDefaults.screenShareEncoding).toMatchObject({
			maxBitrate: 20_000_000,
			maxFramerate: 60,
			priority: 'high',
		});
	});

	it('uses publisher-side stream control defaults for high-fidelity screen sharing', () => {
		expect(roomOptionDefaults).toMatchObject({
			adaptiveStream: false,
			dynacast: true,
			singlePeerConnection: true,
		});
	});
});
