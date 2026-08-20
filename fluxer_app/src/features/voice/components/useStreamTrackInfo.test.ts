// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	mergeStreamTrackInfo,
	resolveStreamTrackInfoSnapshot,
	resolveStreamTrackStatsInfo,
} from '@app/features/voice/components/useStreamTrackInfo';
import {createVoiceMediaGraphSnapshot, transitionVoiceMediaGraph} from '@app/features/voice/engine/VoiceMediaGraph';
import {
	type VoiceMediaGraphStatsView,
	voiceMediaGraphStatsObservationsFromPerTrackStats,
} from '@app/features/voice/engine/VoiceMediaGraphStats';
import type {VoiceMediaGraphStatsTrackObservation} from '@app/features/voice/engine/VoiceMediaGraphStatsObservations';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
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

function graphViewWithObservations(
	tracks: ReadonlyArray<VoiceMediaGraphStatsTrackObservation>,
): VoiceMediaGraphStatsView {
	return transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
		type: 'stats.observed',
		at: 1000,
		connectionId: 'conn-1',
		platform: 'web',
		tracks,
	});
}

function graphViewWithWebStats(tracks: ReadonlyArray<VoiceEngineV2PerTrackStats>): VoiceMediaGraphStatsView {
	return transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
		type: 'stats.observed',
		at: 1000,
		connectionId: 'conn-1',
		platform: 'web',
		tracks: voiceMediaGraphStatsObservationsFromPerTrackStats(tracks),
	});
}

describe('resolveStreamTrackInfoSnapshot', () => {
	it('prefers attached element dimensions and rounds track settings fps', () => {
		expect(
			resolveStreamTrackInfoSnapshot({
				attachedElements: [{videoWidth: 3840, videoHeight: 2160}],
				settings: {width: 1280, height: 720, frameRate: 29.7},
			}),
		).toEqual({width: 3840, height: 2160, fps: 30});
	});
});

describe('mergeStreamTrackInfo', () => {
	it('fills missing rendered-track fps from matched stats without replacing rendered dimensions', () => {
		const renderedInfo = {width: 3840, height: 2160, fps: 0};
		const statsInfo = {width: 1920, height: 1080, fps: 24.7};

		expect(mergeStreamTrackInfo(renderedInfo, statsInfo)).toEqual({width: 3840, height: 2160, fps: 25});
	});
});

describe('resolveStreamTrackStatsInfo', () => {
	it('resolves screen-share info from the graph by trackSid', () => {
		const view = graphViewWithObservations([
			observation({
				trackSid: 'TR_screen',
				source: 'screen_share',
				direction: 'send',
				width: 2560,
				height: 1440,
				fps: 59.6,
			}),
		]);

		expect(
			resolveStreamTrackStatsInfo(view, {
				trackSid: 'TR_screen',
				source: VoiceTrackSource.ScreenShare,
				kind: 'video',
			}),
		).toEqual({width: 2560, height: 1440, fps: 59.6});
	});

	it('resolves remote screen-share info via the participantIdentity fallback', () => {
		const view = graphViewWithObservations([
			observation({
				participantSid: 'PA_1',
				participantIdentity: 'user_2_connection_2',
				trackSid: 'TR_remote_screen',
				source: 'screen_share',
				width: 3840,
				height: 2160,
				fps: 24.7,
			}),
		]);

		expect(
			resolveStreamTrackStatsInfo(view, {
				participantIdentity: 'user_2_connection_2',
				source: VoiceTrackSource.ScreenShare,
				kind: 'video',
			}),
		).toEqual({width: 3840, height: 2160, fps: 24.7});
	});

	it('resolves web per-track stats dispatched into the graph by trackIdentifier', () => {
		const view = graphViewWithWebStats([
			{
				direction: 'recv',
				kind: 'video',
				trackIdentifier: 'camera-media-track',
				bitrateKbps: 800,
				framesPerSecond: 30,
			},
			{
				direction: 'recv',
				kind: 'video',
				trackIdentifier: 'screen-media-track',
				bitrateKbps: 4200,
				framesPerSecond: 24.7,
				frameWidth: 3840,
				frameHeight: 2160,
			},
		]);

		expect(
			resolveStreamTrackStatsInfo(view, {
				trackIdentifier: 'screen-media-track',
				source: VoiceTrackSource.ScreenShare,
				kind: 'video',
			}),
		).toEqual({width: 3840, height: 2160, fps: 24.7});
	});

	it('can return fps-only web stats so rendered dimensions still get the estimated frame rate', () => {
		const view = graphViewWithWebStats([
			{
				direction: 'recv',
				kind: 'video',
				trackIdentifier: 'screen-media-track',
				bitrateKbps: 4200,
				framesPerSecond: 25,
			},
		]);

		expect(
			resolveStreamTrackStatsInfo(view, {
				trackIdentifier: 'screen-media-track',
				source: VoiceTrackSource.ScreenShare,
				kind: 'video',
			}),
		).toEqual({fps: 25});
	});
});
