// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	type PerTrackStats,
	VoiceEngineV2AppStatsHostAdapter,
	type VoiceEngineV2AppStatsHostAdapterScheduler,
} from '@app/features/voice/engine/v2/VoiceEngineV2AppStatsHostAdapter';
import {classifyVoiceEngineV2TrackStats} from '@fluxer/voice_engine_v2';
import type {Room} from 'livekit-client';
import {describe, expect, it} from 'vitest';

interface FakeScheduler extends VoiceEngineV2AppStatsHostAdapterScheduler {
	runStatsTick(): void;
}

function createScheduler(): FakeScheduler {
	const handlers = new Map<number, () => void>();
	return {
		setInterval(handler, intervalMs) {
			handlers.set(intervalMs, handler);
			return intervalMs;
		},
		clearInterval(handle) {
			handlers.delete(handle as number);
		},
		runStatsTick() {
			handlers.get(2000)?.();
		},
	};
}

function chromiumCameraSimulcastReports(bytesSentByRid: Record<string, number>): Map<string, unknown> {
	return new Map<string, unknown>([
		[
			'SV1',
			{
				type: 'media-source',
				id: 'SV1',
				kind: 'video',
				trackIdentifier: 'cam-track',
				width: 1280,
				height: 720,
				framesPerSecond: 30,
				frames: 1801,
			},
		],
		['C1', {type: 'codec', id: 'C1', mimeType: 'video/H264', payloadType: 108}],
		[
			'OT01V1',
			{
				type: 'outbound-rtp',
				id: 'OT01V1',
				kind: 'video',
				ssrc: 1001,
				rid: 'q',
				mid: '2',
				active: false,
				mediaSourceId: 'SV1',
				codecId: 'C1',
				bytesSent: bytesSentByRid.q,
				framesEncoded: 1,
				targetBitrate: 0,
				qualityLimitationReason: 'none',
			},
		],
		[
			'OT01V2',
			{
				type: 'outbound-rtp',
				id: 'OT01V2',
				kind: 'video',
				ssrc: 1002,
				rid: 'h',
				mid: '2',
				active: false,
				mediaSourceId: 'SV1',
				codecId: 'C1',
				bytesSent: bytesSentByRid.h,
				framesEncoded: 1,
				targetBitrate: 0,
				qualityLimitationReason: 'none',
			},
		],
		[
			'OT01V3',
			{
				type: 'outbound-rtp',
				id: 'OT01V3',
				kind: 'video',
				ssrc: 1003,
				rid: 'f',
				mid: '2',
				active: true,
				mediaSourceId: 'SV1',
				codecId: 'C1',
				bytesSent: bytesSentByRid.f,
				framesEncoded: 1,
				frameWidth: 1280,
				frameHeight: 720,
				targetBitrate: 1133000,
				qualityLimitationReason: 'none',
			},
		],
	]);
}

async function collectChromiumCameraRows(
	samples: ReadonlyArray<{now: number; bytesSentByRid: Record<string, number>}>,
): Promise<Array<PerTrackStats>> {
	const scheduler = createScheduler();
	let sampleIndex = 0;
	let now = samples[0].now;
	const publisher = {
		getStats() {
			return Promise.resolve(chromiumCameraSimulcastReports(samples[sampleIndex].bytesSentByRid));
		},
	};
	const adapter = new VoiceEngineV2AppStatsHostAdapter({now: () => now, scheduler});
	adapter.setRoom({engine: {pcManager: {publisher}}, numParticipants: 2} as unknown as Room);
	adapter.startStatsTracking();
	for (sampleIndex = 0; sampleIndex < samples.length; sampleIndex += 1) {
		now = samples[sampleIndex].now;
		scheduler.runStatsTick();
		await Promise.resolve();
		await Promise.resolve();
		await Promise.resolve();
	}
	const rows = adapter.perTrackStats;
	adapter.cleanup();
	return rows;
}

describe('VoiceEngineV2AppStatsHostAdapter chromium outbound rows', () => {
	it('collects the outbound-rtp active flag for every simulcast layer', async () => {
		const rows = await collectChromiumCameraRows([{now: 1000, bytesSentByRid: {q: 0, h: 0, f: 5_000_000}}]);

		expect(rows.map((row) => row.rid)).toEqual(['q', 'h', 'f']);
		expect(rows.map((row) => row.active)).toEqual([false, false, true]);
	});

	it('omits the bitrate window on a first observation so a zero bitrate is not read as measured', async () => {
		const rows = await collectChromiumCameraRows([{now: 1000, bytesSentByRid: {q: 0, h: 0, f: 5_000_000}}]);

		expect(rows.map((row) => row.bitrateKbps)).toEqual([0, 0, 0]);
		expect(rows.map((row) => row.bitrateWindowMs)).toEqual([undefined, undefined, undefined]);
	});

	it('reports the bitrate window once a delta was measured so a zero bitrate is falsifiable', async () => {
		const rows = await collectChromiumCameraRows([
			{now: 1000, bytesSentByRid: {q: 0, h: 0, f: 5_000_000}},
			{now: 3001, bytesSentByRid: {q: 0, h: 0, f: 5_000_000}},
		]);

		expect(rows.map((row) => row.bitrateKbps)).toEqual([0, 0, 0]);
		expect(rows.map((row) => row.bitrateWindowMs)).toEqual([2001, 2001, 2001]);
	});

	it('keeps every pre-existing chromium row field byte-identical', async () => {
		const rows = await collectChromiumCameraRows([
			{now: 1000, bytesSentByRid: {q: 0, h: 0, f: 5_000_000}},
			{now: 3000, bytesSentByRid: {q: 0, h: 0, f: 5_500_000}},
		]);
		const {active, bitrateWindowMs, ...legacy} = rows[2];

		expect(legacy).toEqual({
			direction: 'send',
			kind: 'video',
			ssrc: 1003,
			rid: 'f',
			mid: '2',
			trackIdentifier: 'cam-track',
			mediaSourceId: 'SV1',
			codec: 'video/H264',
			payloadType: 108,
			bitrateKbps: 2000,
			framesEncoded: 1,
			frameWidth: 1280,
			frameHeight: 720,
			sourceFrameWidth: 1280,
			sourceFrameHeight: 720,
			sourceFramesPerSecond: 30,
			sourceFrames: 1801,
			targetBitrateKbps: 1133,
			qualityLimitationReason: 'none',
			encoderAcceleration: 'unknown',
		});
	});
});

function firefoxCameraAndScreenShareReports(): Map<string, unknown> {
	return new Map<string, unknown>([
		['C1', {type: 'codec', id: 'C1', mimeType: 'video/H264', payloadType: 108}],
		['C2', {type: 'codec', id: 'C2', mimeType: 'audio/opus', payloadType: 109}],
		[
			'outbound_rtp_audio_1',
			{
				type: 'outbound-rtp',
				id: 'outbound_rtp_audio_1',
				kind: 'audio',
				ssrc: 724915693,
				mid: '1',
				codecId: 'C2',
				bytesSent: 100_000,
			},
		],
		[
			'outbound_rtp_video_3',
			{
				type: 'outbound-rtp',
				id: 'outbound_rtp_video_3',
				kind: 'video',
				ssrc: 4089824766,
				mid: '3',
				codecId: 'C1',
				bytesSent: 5_000_000,
				framesEncoded: 900,
			},
		],
		[
			'outbound_rtp_video_4',
			{
				type: 'outbound-rtp',
				id: 'outbound_rtp_video_4',
				kind: 'video',
				ssrc: 1477030229,
				mid: '4',
				codecId: 'C1',
				bytesSent: 2_000_000,
				framesEncoded: 600,
			},
		],
		[
			'inbound_rtp_audio_2',
			{
				type: 'inbound-rtp',
				id: 'inbound_rtp_audio_2',
				kind: 'audio',
				ssrc: 1661789229,
				mid: '2',
				codecId: 'C2',
				trackIdentifier: '{8a106956-d202-4916-8e19-0a1672c64b73}',
				bytesReceived: 50_000,
			},
		],
	]);
}

async function collectFirefoxRows(
	transceivers: ReadonlyArray<{mid: string | null; sender?: {track?: {id: string} | null} | null}> | null,
): Promise<Array<PerTrackStats>> {
	const scheduler = createScheduler();
	const publisher = {
		getStats() {
			return Promise.resolve(firefoxCameraAndScreenShareReports());
		},
		...(transceivers === null ? {} : {getTransceivers: () => transceivers}),
	};
	const adapter = new VoiceEngineV2AppStatsHostAdapter({now: () => 1000, scheduler});
	adapter.setRoom({engine: {pcManager: {publisher}}, numParticipants: 2} as unknown as Room);
	adapter.startStatsTracking();
	scheduler.runStatsTick();
	await Promise.resolve();
	await Promise.resolve();
	await Promise.resolve();
	const rows = adapter.perTrackStats;
	adapter.cleanup();
	return rows;
}

describe('VoiceEngineV2AppStatsHostAdapter firefox outbound rows', () => {
	it('backfills the outbound trackIdentifier from the transceiver mid when the report carries neither identity member', async () => {
		const rows = await collectFirefoxRows([
			{mid: '1', sender: {track: {id: '{mic-track}'}}},
			{mid: '2', sender: {track: null}},
			{mid: '3', sender: {track: {id: '{camera-track}'}}},
			{mid: '4', sender: {track: {id: '{screen-track}'}}},
		]);

		expect(rows.map((row) => [row.direction, row.kind, row.mid, row.trackIdentifier])).toEqual([
			['send', 'audio', '1', '{mic-track}'],
			['send', 'video', '3', '{camera-track}'],
			['send', 'video', '4', '{screen-track}'],
			['recv', 'audio', '2', '{8a106956-d202-4916-8e19-0a1672c64b73}'],
		]);
	});

	it('leaves an outbound row unidentified when no transceiver carries its mid', async () => {
		const rows = await collectFirefoxRows([{mid: '3', sender: {track: {id: '{camera-track}'}}}]);

		expect(rows.map((row) => row.trackIdentifier)).toEqual([
			undefined,
			'{camera-track}',
			undefined,
			'{8a106956-d202-4916-8e19-0a1672c64b73}',
		]);
	});

	it('keeps the inbound trackIdentifier when the source exposes no transceivers at all', async () => {
		const rows = await collectFirefoxRows(null);

		expect(rows.map((row) => row.trackIdentifier)).toEqual([
			undefined,
			undefined,
			undefined,
			'{8a106956-d202-4916-8e19-0a1672c64b73}',
		]);
	});

	it('lets the classifier tell a firefox camera apart from a simultaneous screen share', async () => {
		const rows = await collectFirefoxRows([
			{mid: '1', sender: {track: {id: '{mic-track}'}}},
			{mid: '3', sender: {track: {id: '{camera-track}'}}},
			{mid: '4', sender: {track: {id: '{screen-track}'}}},
		]);
		const classification = classifyVoiceEngineV2TrackStats({
			tracks: rows.map((row) => ({
				direction: row.direction,
				kind: row.kind,
				rid: row.rid,
				trackIdentifier: row.trackIdentifier,
				bitrateKbps: row.bitrateKbps,
			})),
			publications: {
				localCameraTrackId: '{camera-track}',
				localMicrophoneTrackId: '{mic-track}',
				localScreenShareTrackId: '{screen-track}',
				localScreenShareAudioTrackId: null,
				remoteMicrophoneTrackIds: [],
				remoteScreenShareTrackIds: [],
				remoteScreenShareAudioTrackIds: [],
			},
		});

		expect(rows[classification.localVideoTrackIndex!].mid).toBe('3');
		expect(rows[classification.localScreenShareTrackIndex!].mid).toBe('4');
	});
});
