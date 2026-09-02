// SPDX-License-Identifier: AGPL-3.0-or-later

import MediaEngine from '@app/features/voice/engine/MediaEngineFacade';
import {
	getAppliedScreenShareFrameRate,
	getDeliberateScreenShareQualityChangeAt,
	parseScreenShareQualityLimitationReason,
	SCREEN_SHARE_UNDERPERFORMANCE_SAMPLE_INTERVAL_MS,
	type ScreenShareUnderperformanceReason,
	ScreenShareUnderperformanceTracker,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {classifyVoiceEngineV2TrackStats, type VoiceEngineV2PerTrackStats} from '@fluxer/voice_engine_v2';
import type {Track} from 'livekit-client';
import {useEffect, useState} from 'react';

const CAMERA_SOURCE = VoiceTrackSource.Camera as Track.Source;
const SCREEN_SHARE_SOURCE = VoiceTrackSource.ScreenShare as Track.Source;

interface LocalScreenShareTrackStats {
	trackId: string;
	stats: VoiceEngineV2PerTrackStats;
	snapshot: ReadonlyArray<VoiceEngineV2PerTrackStats>;
}

function selectLocalScreenShareTrackStats(): LocalScreenShareTrackStats | null {
	const perTrackStats = MediaEngine.perTrackStats;
	if (perTrackStats.length === 0) return null;
	const localParticipant = MediaEngine.room?.localParticipant;
	const localScreenShareTrackId =
		localParticipant?.getTrackPublication(SCREEN_SHARE_SOURCE)?.videoTrack?.mediaStreamTrack?.id ?? null;
	if (!localScreenShareTrackId) return null;
	const classification = classifyVoiceEngineV2TrackStats({
		tracks: perTrackStats.map((track) => ({
			direction: track.direction,
			kind: track.kind,
			rid: track.rid,
			trackIdentifier: track.trackIdentifier,
			bitrateKbps: track.bitrateKbps,
		})),
		publications: {
			localCameraTrackId:
				localParticipant?.getTrackPublication(CAMERA_SOURCE)?.videoTrack?.mediaStreamTrack?.id ?? null,
			localMicrophoneTrackId: null,
			localScreenShareTrackId,
			localScreenShareAudioTrackId: null,
			remoteMicrophoneTrackIds: [],
			remoteScreenShareTrackIds: [],
			remoteScreenShareAudioTrackIds: [],
		},
	});
	const index = classification.localScreenShareTrackIndex;
	if (index == null) return null;
	const stats = perTrackStats[index];
	if (!stats) return null;
	return {trackId: localScreenShareTrackId, stats, snapshot: perTrackStats};
}

function readEncodeFrameRate(stats: VoiceEngineV2PerTrackStats): number {
	const framesPerSecond = stats.framesPerSecond;
	return typeof framesPerSecond === 'number' && Number.isFinite(framesPerSecond) ? framesPerSecond : 0;
}

export function useScreenShareUnderperformance(enabled: boolean): ScreenShareUnderperformanceReason | null {
	const [reason, setReason] = useState<ScreenShareUnderperformanceReason | null>(null);
	useEffect(() => {
		setReason(null);
		if (!enabled) return;
		const tracker = new ScreenShareUnderperformanceTracker();
		let lastSnapshot: ReadonlyArray<VoiceEngineV2PerTrackStats> | null = null;
		const sample = () => {
			const selected = selectLocalScreenShareTrackStats();
			if (!selected) {
				lastSnapshot = null;
				tracker.reset();
				setReason(null);
				return;
			}
			if (selected.snapshot === lastSnapshot) return;
			lastSnapshot = selected.snapshot;
			const requestedFrameRate = getAppliedScreenShareFrameRate(selected.trackId);
			if (requestedFrameRate === null) {
				setReason(null);
				return;
			}
			setReason(
				tracker.observe({
					sample: {
						encodeFrameRate: readEncodeFrameRate(selected.stats),
						limitationReason: parseScreenShareQualityLimitationReason(selected.stats.qualityLimitationReason),
					},
					requestedFrameRate,
					trackId: selected.trackId,
					qualityChangeAt: getDeliberateScreenShareQualityChangeAt(),
					now: Date.now(),
				}),
			);
		};
		sample();
		const timer = setInterval(sample, SCREEN_SHARE_UNDERPERFORMANCE_SAMPLE_INTERVAL_MS);
		return () => {
			clearInterval(timer);
			tracker.reset();
		};
	}, [enabled]);
	return reason;
}
