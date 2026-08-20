// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceMediaGraphSnapshot,
	transitionVoiceMediaGraph,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {
	buildVoiceMediaGraphStatsView,
	selectVoiceMediaGraphStreamTrackInfo,
	voiceMediaGraphStatsObservationsFromPerTrackStats,
} from '@app/features/voice/engine/VoiceMediaGraphStats';
import type {VoiceMediaGraphStatsTrackObservation} from '@app/features/voice/engine/VoiceMediaGraphStatsObservations';
import type {VoiceEngineV2PerTrackStats} from '@fluxer/voice_engine_v2';
import {describe, expect, it} from 'vitest';

function observation(overrides: Partial<VoiceMediaGraphStatsTrackObservation>): VoiceMediaGraphStatsTrackObservation {
	return {
		trackSid: null,
		trackIdentifier: null,
		mediaSourceId: null,
		mid: null,
		rid: null,
		ssrc: null,
		participantIdentity: null,
		participantSid: null,
		source: null,
		direction: 'recv',
		kind: 'video',
		fps: null,
		width: null,
		height: null,
		sourceFps: null,
		sourceWidth: null,
		sourceHeight: null,
		...overrides,
	};
}

function snapshotWithObservations(
	connectionId: string,
	platform: 'native' | 'web',
	tracks: ReadonlyArray<VoiceMediaGraphStatsTrackObservation>,
): VoiceMediaGraphSnapshot {
	return transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
		type: 'stats.observed',
		at: 1000,
		connectionId,
		platform,
		tracks,
	});
}

describe('voiceMediaGraphStatsObservationsFromPerTrackStats', () => {
	it('normalizes web per-track stats and skips unknown kinds', () => {
		const tracks: Array<VoiceEngineV2PerTrackStats> = [
			{
				direction: 'recv',
				kind: 'video',
				trackIdentifier: 'screen-media-track',
				mediaSourceId: 'media-source-1',
				mid: '4',
				rid: 'f',
				ssrc: 1234,
				bitrateKbps: 4200,
				framesPerSecond: 24.7,
				frameWidth: 3840,
				frameHeight: 2160,
				sourceFramesPerSecond: 60,
			},
			{direction: 'recv', kind: 'unknown', bitrateKbps: 0},
		];

		expect(voiceMediaGraphStatsObservationsFromPerTrackStats(tracks)).toEqual([
			observation({
				trackIdentifier: 'screen-media-track',
				mediaSourceId: 'media-source-1',
				mid: '4',
				rid: 'f',
				ssrc: 1234,
				fps: 24.7,
				width: 3840,
				height: 2160,
				sourceFps: 60,
			}),
		]);
	});
});

describe('selectVoiceMediaGraphStreamTrackInfo', () => {
	it('matches on trackSid before any other key', () => {
		const snapshot = snapshotWithObservations('conn-1', 'native', [
			observation({trackSid: 'TR_a', rid: 'f', width: 1280, height: 720, fps: 15}),
			observation({trackSid: 'TR_b', width: 1920, height: 1080, fps: 30}),
		]);

		expect(selectVoiceMediaGraphStreamTrackInfo(snapshot, {trackSid: 'TR_b', rid: 'f', kind: 'video'})).toEqual({
			width: 1920,
			height: 1080,
			fps: 30,
		});
	});

	it('falls back to rid when no track identifier matches', () => {
		const snapshot = snapshotWithObservations('conn-1', 'web', [
			observation({rid: 'h', width: 1280, height: 720, fps: 20}),
			observation({rid: 'f', width: 1920, height: 1080, fps: 30}),
		]);

		expect(selectVoiceMediaGraphStreamTrackInfo(snapshot, {trackSid: 'TR_missing', rid: 'f', kind: 'video'})).toEqual({
			width: 1920,
			height: 1080,
			fps: 30,
		});
	});

	it('falls back to participantIdentity with strict source matching', () => {
		const snapshot = snapshotWithObservations('conn-1', 'native', [
			observation({
				trackSid: 'TR_camera',
				participantIdentity: 'user_2_conn_2',
				source: 'camera',
				width: 640,
				height: 480,
				fps: 24,
			}),
			observation({
				trackSid: 'TR_screen',
				participantIdentity: 'user_2_conn_2',
				source: 'screen_share',
				width: 3840,
				height: 2160,
				fps: 25,
			}),
		]);

		expect(
			selectVoiceMediaGraphStreamTrackInfo(snapshot, {
				trackSid: 'TR_missing',
				participantIdentity: 'user_2_conn_2',
				source: 'screen_share',
				kind: 'video',
			}),
		).toEqual({width: 3840, height: 2160, fps: 25});
	});

	it('does not match sourceless observations through the participantIdentity fallback', () => {
		const snapshot = snapshotWithObservations('conn-1', 'web', [observation({width: 640, height: 480, fps: 24})]);

		expect(
			selectVoiceMediaGraphStreamTrackInfo(snapshot, {
				participantIdentity: 'user_2_conn_2',
				source: 'screen_share',
				kind: 'video',
			}),
		).toBeNull();
	});

	it('uses source dimensions and fps when encoded values are absent', () => {
		const snapshot = snapshotWithObservations('conn-1', 'web', [
			observation({trackIdentifier: 'screen-media-track', sourceWidth: 2560, sourceHeight: 1440, sourceFps: 60}),
		]);

		expect(
			selectVoiceMediaGraphStreamTrackInfo(snapshot, {trackIdentifier: 'screen-media-track', kind: 'video'}),
		).toEqual({width: 2560, height: 1440, fps: 60});
	});

	it('drops observations for a stale connectionId', () => {
		const snapshot = snapshotWithObservations('conn-1', 'native', [
			observation({trackSid: 'TR_a', width: 1920, height: 1080, fps: 30}),
		]);
		const next = transitionVoiceMediaGraph(snapshot, {
			type: 'stats.observed',
			at: 2000,
			connectionId: 'conn-2',
			platform: 'native',
			tracks: [observation({trackSid: 'TR_b', width: 1280, height: 720, fps: 15})],
		});

		expect(next).toBe(snapshot);
		expect(selectVoiceMediaGraphStreamTrackInfo(next, {trackSid: 'TR_b', kind: 'video'})).toBeNull();
	});

	it('wipes stale entries when the stats connection changes', () => {
		const snapshot = snapshotWithObservations('conn-1', 'native', [
			observation({trackSid: 'TR_a', width: 1920, height: 1080, fps: 30}),
		]);
		const next = transitionVoiceMediaGraph(snapshot, {type: 'stats.connectionChanged', connectionId: 'conn-2'});

		expect(next.statsConnectionId).toBe('conn-2');
		expect(selectVoiceMediaGraphStreamTrackInfo(next, {trackSid: 'TR_a', kind: 'video'})).toBeNull();
	});

	it('resolves from an ad-hoc stats view built from observations', () => {
		const view = buildVoiceMediaGraphStatsView(
			[observation({trackIdentifier: 'screen-media-track', width: 1920, height: 1080, fps: 30})],
			'web',
			0,
			'fallback',
		);

		expect(selectVoiceMediaGraphStreamTrackInfo(view, {trackIdentifier: 'screen-media-track', kind: 'video'})).toEqual({
			width: 1920,
			height: 1080,
			fps: 30,
		});
	});
});
