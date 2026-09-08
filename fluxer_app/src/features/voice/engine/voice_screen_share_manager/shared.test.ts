// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {findStalledVideoEncoder} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import {describe, expect, test, vi} from 'vitest';

vi.mock('@app/features/voice/utils/VideoQualityEntitlement', () => ({hasHigherVideoQuality: () => false}));

function makeStats(entries: ReadonlyArray<Record<string, unknown>>): RTCStatsReport {
	return new Map(entries.map((entry) => [entry.id as string, entry])) as unknown as RTCStatsReport;
}

const CODEC_REPORT = {type: 'codec', id: 'codec-h264', mimeType: 'video/H264'};
const SOURCE_REPORT = {type: 'media-source', id: 'source-1', kind: 'video', frames: 240, framesPerSecond: 30};

function outboundReport(overrides: Record<string, unknown>): Record<string, unknown> {
	return {
		type: 'outbound-rtp',
		id: 'outbound-1',
		kind: 'video',
		codecId: 'codec-h264',
		mediaSourceId: 'source-1',
		framesEncoded: 0,
		framesSent: 0,
		...overrides,
	};
}

describe('findStalledVideoEncoder', () => {
	test('reports a stall when the encoding is active and the source is producing frames', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({active: true})]);
		expect(findStalledVideoEncoder(stats, 'h264')).toEqual({
			codec: 'h264',
			framesEncoded: 0,
			framesSent: 0,
			sourceFrames: 240,
			sourceFramesPerSecond: 30,
		});
	});

	test('ignores an encoding the SFU deactivated even though the source is producing frames', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({active: false})]);
		expect(findStalledVideoEncoder(stats, 'h264')).toBeNull();
	});

	test('still reports a stall when active is absent, as on Firefox', () => {
		const stats = makeStats([CODEC_REPORT, SOURCE_REPORT, outboundReport({})]);
		expect(findStalledVideoEncoder(stats, 'h264')?.codec).toBe('h264');
	});

	test('skips a deactivated layer and reports the still-active sibling instead', () => {
		const stats = makeStats([
			CODEC_REPORT,
			SOURCE_REPORT,
			{type: 'media-source', id: 'source-low', kind: 'video', frames: 90, framesPerSecond: 15},
			outboundReport({id: 'outbound-low', mediaSourceId: 'source-low', active: false}),
			outboundReport({id: 'outbound-high', active: true}),
		]);
		expect(findStalledVideoEncoder(stats, 'h264')?.sourceFrames).toBe(240);
	});
});
