// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	graphTileStateHoldsWatchIntent,
	selectScreenShareBufferingPresentation,
	selectVoiceParticipantTileCameraActive,
	selectVoiceParticipantTileScreenShareState,
	shouldShowCameraBuffering,
	shouldShowWatchFailed,
	type VoiceParticipantTileCameraActiveSignals,
	type VoiceParticipantTileCameraBufferingSignals,
	type VoiceParticipantTileScreenShareSignals,
} from '@app/features/voice/components/VoiceParticipantTileStateMachine';
import {
	getAppliedScreenShareFrameRate,
	noteAppliedScreenShareFrameRate,
	type ScreenShareQualityLimitationReason,
	type ScreenShareUnderperformanceReason,
	ScreenShareUnderperformanceTracker,
} from '@app/features/voice/engine/ScreenShareUnderperformance';
import type {VoiceMediaGraphStreamTileState} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {describe, expect, it} from 'vitest';

const GRAPH_TILE_STATES: ReadonlyArray<VoiceMediaGraphStreamTileState> = [
	'idle',
	'watchDesired',
	'publicationMissing',
	'attaching',
	'subscribedAwaitingFrame',
	'rendering',
	'failed',
];

const WATCH_INTENT_GRAPH_TILE_STATES: ReadonlyArray<VoiceMediaGraphStreamTileState> = [
	'watchDesired',
	'publicationMissing',
	'attaching',
	'subscribedAwaitingFrame',
	'rendering',
	'failed',
];

function signals(
	overrides: Partial<VoiceParticipantTileScreenShareSignals> = {},
): VoiceParticipantTileScreenShareSignals {
	return {
		graphTileState: 'idle',
		isScreenShare: true,
		isOwnScreenShare: false,
		isFocusedPlaceholderTile: false,
		isFocusPresentationTile: false,
		isTrackReference: true,
		cameraLocallyDisabled: false,
		isRepublishGracePending: false,
		...overrides,
	};
}

function cameraSignals(
	overrides: Partial<VoiceParticipantTileCameraBufferingSignals> = {},
): VoiceParticipantTileCameraBufferingSignals {
	return {
		isScreenShare: false,
		isFocusedPlaceholderTile: false,
		cameraLocallyDisabled: false,
		isOwnCameraHidden: false,
		isCameraActive: true,
		hasVideo: false,
		hasRenderedVideoFrame: false,
		...overrides,
	};
}

function cameraActiveSignals(
	overrides: Partial<VoiceParticipantTileCameraActiveSignals> = {},
): VoiceParticipantTileCameraActiveSignals {
	return {
		isCameraTile: true,
		isOwnContent: false,
		isCameraPublicationActive: false,
		isParticipantCameraActive: false,
		isLocalCameraRequested: false,
		...overrides,
	};
}

describe('VoiceParticipantTileStateMachine camera buffering state', () => {
	it('shows buffering while an active camera publication has no video', () => {
		expect(shouldShowCameraBuffering(cameraSignals())).toBe(true);
	});

	it('keeps buffering until the camera video element has rendered a frame', () => {
		expect(shouldShowCameraBuffering(cameraSignals({hasVideo: true, hasRenderedVideoFrame: false}))).toBe(true);
	});

	it('keeps non-camera, placeholder, hidden, disabled, inactive, and rendered camera tiles out of buffering', () => {
		expect(shouldShowCameraBuffering(cameraSignals({isScreenShare: true}))).toBe(false);
		expect(shouldShowCameraBuffering(cameraSignals({isFocusedPlaceholderTile: true}))).toBe(false);
		expect(shouldShowCameraBuffering(cameraSignals({cameraLocallyDisabled: true}))).toBe(false);
		expect(shouldShowCameraBuffering(cameraSignals({isOwnCameraHidden: true}))).toBe(false);
		expect(shouldShowCameraBuffering(cameraSignals({isCameraActive: false}))).toBe(false);
		expect(shouldShowCameraBuffering(cameraSignals({hasVideo: true, hasRenderedVideoFrame: true}))).toBe(false);
	});
});

describe('VoiceParticipantTileStateMachine camera active state', () => {
	it('preserves participant camera flags for remote camera tiles', () => {
		expect(selectVoiceParticipantTileCameraActive(cameraActiveSignals({isParticipantCameraActive: true}))).toBe(true);
	});

	it('treats a local capture request as an active own camera tile', () => {
		expect(
			selectVoiceParticipantTileCameraActive(cameraActiveSignals({isOwnContent: true, isLocalCameraRequested: true})),
		).toBe(true);
	});

	it('keeps a camera tile inactive without a publication, participant flag, or local request', () => {
		expect(selectVoiceParticipantTileCameraActive(cameraActiveSignals())).toBe(false);
		expect(selectVoiceParticipantTileCameraActive(cameraActiveSignals({isCameraTile: false}))).toBe(false);
	});
});

describe('VoiceParticipantTileStateMachine graph-derived screen share state', () => {
	it('shows the watch prompt when the graph is idle for a published remote stream', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'idle'}))).toBe('watchPrompt');
	});

	it('stays idle when the graph is idle and no track reference exists', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'idle', isTrackReference: false}))).toBe(
			'idle',
		);
	});

	it('shows buffering while watch is desired before a subscription entry exists', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'watchDesired'}))).toBe('buffering');
	});

	it('shows buffering while the publication is missing but a track reference remains', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'publicationMissing'}))).toBe(
			'buffering',
		);
	});

	it('shows stream ended when the publication is missing and the track is gone', () => {
		expect(
			selectVoiceParticipantTileScreenShareState(
				signals({graphTileState: 'publicationMissing', isTrackReference: false}),
			),
		).toBe('streamEnded');
	});

	it('shows buffering instead of stream ended during the republish grace window', () => {
		expect(
			selectVoiceParticipantTileScreenShareState(
				signals({graphTileState: 'publicationMissing', isTrackReference: false, isRepublishGracePending: true}),
			),
		).toBe('buffering');
	});

	it('shows buffering while attaching', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'attaching'}))).toBe('buffering');
	});

	it('shows buffering while subscribed and awaiting the first frame', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'subscribedAwaitingFrame'}))).toBe(
			'buffering',
		);
	});

	it('renders without overlays once the graph reports rendering', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'rendering'}))).toBe('idle');
	});

	it('shows the watch failed overlay when the graph reports a failure', () => {
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'failed'}))).toBe('watchFailed');
		expect(
			selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'failed', isTrackReference: false})),
		).toBe('watchFailed');
	});

	it('never shows the watch prompt while the graph holds watch intent', () => {
		for (const graphTileState of WATCH_INTENT_GRAPH_TILE_STATES) {
			expect(graphTileStateHoldsWatchIntent(graphTileState)).toBe(true);
			expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState}))).not.toBe('watchPrompt');
			expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState, isTrackReference: false}))).not.toBe(
				'watchPrompt',
			);
		}
		expect(graphTileStateHoldsWatchIntent('idle')).toBe(false);
	});

	it('never shows failure UI while the graph reports rendering', () => {
		expect(shouldShowWatchFailed(signals({graphTileState: 'rendering'}))).toBe(false);
		expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'rendering'}))).not.toBe('watchFailed');
		for (const graphTileState of GRAPH_TILE_STATES) {
			if (graphTileState === 'failed') continue;
			expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState}))).not.toBe('watchFailed');
		}
	});

	it('suppresses every overlay for local, focused-placeholder, and non-screen-share tiles', () => {
		for (const graphTileState of GRAPH_TILE_STATES) {
			expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState, isOwnScreenShare: true}))).toBe(
				'idle',
			);
			expect(
				selectVoiceParticipantTileScreenShareState(signals({graphTileState, isFocusedPlaceholderTile: true})),
			).toBe('idle');
			expect(selectVoiceParticipantTileScreenShareState(signals({graphTileState, isScreenShare: false}))).toBe('idle');
		}
	});

	it('suppresses buffering and the watch prompt while video is locally disabled', () => {
		expect(
			selectVoiceParticipantTileScreenShareState(
				signals({graphTileState: 'watchDesired', cameraLocallyDisabled: true}),
			),
		).toBe('idle');
		expect(
			selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'idle', cameraLocallyDisabled: true})),
		).toBe('idle');
	});

	it('keeps the watch prompt and stream ended overlays off focus presentation tiles', () => {
		expect(
			selectVoiceParticipantTileScreenShareState(signals({graphTileState: 'idle', isFocusPresentationTile: true})),
		).toBe('idle');
		expect(
			selectVoiceParticipantTileScreenShareState(
				signals({graphTileState: 'publicationMissing', isTrackReference: false, isFocusPresentationTile: true}),
			),
		).toBe('idle');
	});
});

describe('VoiceParticipantTileStateMachine buffering presentation', () => {
	it('shows the dimmed last frame while buffering with a retained frame', () => {
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'publicationMissing', isRepublishGracePending: true}),
				hasRetainedLastFrame: true,
			}),
		).toBe('last-frame');
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'attaching'}),
				hasRetainedLastFrame: true,
			}),
		).toBe('last-frame');
	});

	it('falls back to the spinner when the stream never produced a frame', () => {
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'watchDesired'}),
				hasRetainedLastFrame: false,
			}),
		).toBe('spinner');
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'subscribedAwaitingFrame'}),
				hasRetainedLastFrame: false,
			}),
		).toBe('spinner');
	});

	it('renders nothing when the tile is not buffering', () => {
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'rendering'}),
				hasRetainedLastFrame: true,
			}),
		).toBeNull();
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'failed'}),
				hasRetainedLastFrame: true,
			}),
		).toBeNull();
		expect(
			selectScreenShareBufferingPresentation({
				...signals({graphTileState: 'attaching', isOwnScreenShare: true}),
				hasRetainedLastFrame: true,
			}),
		).toBeNull();
	});
});

const UNDERPERFORMANCE_START_AT = 1_000_000;

interface UnderperformanceFeed {
	encodeFrameRate: number;
	limitationReason: ScreenShareQualityLimitationReason;
	requestedFrameRate?: number;
	trackId?: string;
	qualityChangeAt?: number;
	startAt?: number;
}

function feedUnderperformance(
	tracker: ScreenShareUnderperformanceTracker,
	count: number,
	feed: UnderperformanceFeed,
): ScreenShareUnderperformanceReason | null {
	let reason: ScreenShareUnderperformanceReason | null = null;
	const startAt = feed.startAt ?? UNDERPERFORMANCE_START_AT;
	for (let index = 0; index < count; index += 1) {
		reason = tracker.observe({
			sample: {encodeFrameRate: feed.encodeFrameRate, limitationReason: feed.limitationReason},
			requestedFrameRate: feed.requestedFrameRate ?? 30,
			trackId: feed.trackId ?? 'track-a',
			qualityChangeAt: feed.qualityChangeAt ?? 0,
			now: startAt + index * 1000,
		});
	}
	return reason;
}

describe('ScreenShareUnderperformanceTracker', () => {
	it('stays quiet until fifteen datapoints exist', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(tracker, 14, {encodeFrameRate: 5, limitationReason: 'cpu'})).toBeNull();
		expect(
			feedUnderperformance(tracker, 1, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				startAt: UNDERPERFORMANCE_START_AT + 14_000,
			}),
		).toBe('cpu');
	});

	it('does not fire while the encoder holds at least half the requested rate', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(tracker, 30, {encodeFrameRate: 15, limitationReason: 'cpu'})).toBeNull();
	});

	it('uses an eight frame floor for low requested rates', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(
			feedUnderperformance(tracker, 15, {encodeFrameRate: 7, limitationReason: 'bandwidth', requestedFrameRate: 15}),
		).toBe('bandwidth');
		const steady = new ScreenShareUnderperformanceTracker();
		expect(
			feedUnderperformance(steady, 15, {encodeFrameRate: 8, limitationReason: 'bandwidth', requestedFrameRate: 15}),
		).toBeNull();
	});

	it('does not fire when nothing reports an active limitation', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(tracker, 30, {encodeFrameRate: 2, limitationReason: 'none'})).toBeNull();
	});

	it('needs a limitation on at least half of the window', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		feedUnderperformance(tracker, 8, {encodeFrameRate: 5, limitationReason: 'none'});
		expect(
			feedUnderperformance(tracker, 7, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				startAt: UNDERPERFORMANCE_START_AT + 8_000,
			}),
		).toBeNull();
		expect(
			feedUnderperformance(tracker, 1, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				startAt: UNDERPERFORMANCE_START_AT + 15_000,
			}),
		).toBe('cpu');
	});

	it('suppresses for twenty seconds after a deliberate quality change', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		const qualityChangeAt = UNDERPERFORMANCE_START_AT;
		expect(
			feedUnderperformance(tracker, 20, {encodeFrameRate: 5, limitationReason: 'cpu', qualityChangeAt}),
		).toBeNull();
		expect(
			feedUnderperformance(tracker, 1, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				qualityChangeAt,
				startAt: UNDERPERFORMANCE_START_AT + 20_000,
			}),
		).toBe('cpu');
	});

	it('adds no grace period of its own when tracking starts long after the last change', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(
			feedUnderperformance(tracker, 15, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				qualityChangeAt: UNDERPERFORMANCE_START_AT - 60_000,
			}),
		).toBe('cpu');
	});

	it('starts over when the requested rate, the track or the change timestamp moves', () => {
		const byRate = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(byRate, 15, {encodeFrameRate: 5, limitationReason: 'cpu'})).toBe('cpu');
		expect(
			feedUnderperformance(byRate, 1, {encodeFrameRate: 5, limitationReason: 'cpu', requestedFrameRate: 60}),
		).toBeNull();
		const byTrack = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(byTrack, 15, {encodeFrameRate: 5, limitationReason: 'cpu'})).toBe('cpu');
		expect(
			feedUnderperformance(byTrack, 1, {encodeFrameRate: 5, limitationReason: 'cpu', trackId: 'track-b'}),
		).toBeNull();
		const byChange = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(byChange, 15, {encodeFrameRate: 5, limitationReason: 'cpu'})).toBe('cpu');
		expect(
			feedUnderperformance(byChange, 1, {
				encodeFrameRate: 5,
				limitationReason: 'cpu',
				qualityChangeAt: UNDERPERFORMANCE_START_AT - 60_000,
			}),
		).toBeNull();
	});

	it('names the limitation reported most often', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		feedUnderperformance(tracker, 7, {encodeFrameRate: 5, limitationReason: 'cpu'});
		expect(
			feedUnderperformance(tracker, 8, {
				encodeFrameRate: 5,
				limitationReason: 'bandwidth',
				startAt: UNDERPERFORMANCE_START_AT + 7_000,
			}),
		).toBe('bandwidth');
		const other = new ScreenShareUnderperformanceTracker();
		feedUnderperformance(other, 7, {encodeFrameRate: 5, limitationReason: 'bandwidth'});
		expect(
			feedUnderperformance(other, 8, {
				encodeFrameRate: 5,
				limitationReason: 'other',
				startAt: UNDERPERFORMANCE_START_AT + 7_000,
			}),
		).toBe('other');
	});

	it('falls back to the long window when the short one is inconclusive', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		feedUnderperformance(tracker, 15, {encodeFrameRate: 2, limitationReason: 'cpu'});
		expect(
			feedUnderperformance(tracker, 15, {
				encodeFrameRate: 16,
				limitationReason: 'cpu',
				startAt: UNDERPERFORMANCE_START_AT + 15_000,
			}),
		).toBe('cpu');
	});

	it('does not latch once the encoder recovers', () => {
		const tracker = new ScreenShareUnderperformanceTracker();
		expect(feedUnderperformance(tracker, 15, {encodeFrameRate: 5, limitationReason: 'cpu'})).toBe('cpu');
		expect(
			feedUnderperformance(tracker, 30, {
				encodeFrameRate: 30,
				limitationReason: 'none',
				startAt: UNDERPERFORMANCE_START_AT + 15_000,
			}),
		).toBeNull();
	});
});

describe('applied screen share frame rate records', () => {
	it('returns the frame rate recorded for a track and nothing for other tracks', () => {
		noteAppliedScreenShareFrameRate('record-a', 60);
		expect(getAppliedScreenShareFrameRate('record-a')).toBe(60);
		expect(getAppliedScreenShareFrameRate('record-missing')).toBeNull();
		noteAppliedScreenShareFrameRate('record-a', 30);
		expect(getAppliedScreenShareFrameRate('record-a')).toBe(30);
	});

	it('keeps only the most recent records', () => {
		for (let index = 0; index < 9; index += 1) {
			noteAppliedScreenShareFrameRate(`record-prune-${index}`, 15 + index);
		}
		expect(getAppliedScreenShareFrameRate('record-prune-0')).toBeNull();
		expect(getAppliedScreenShareFrameRate('record-prune-1')).toBe(16);
		expect(getAppliedScreenShareFrameRate('record-prune-8')).toBe(23);
	});
});
