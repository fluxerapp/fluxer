// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	isScreenShareVideoSubscriptionRecoveryWanted,
	ScreenShareVideoSubscriptionRecoveryCoordinator,
	type ScreenShareVideoSubscriptionRecoveryGraph,
	type ScreenShareVideoSubscriptionRecoveryPublication,
	type ScreenShareVideoSubscriptionRecoveryScheduler,
} from '@app/features/voice/components/voice_participant_tile/ScreenShareVideoSubscriptionRecovery';
import {
	createVoiceMediaGraphSnapshot,
	transitionVoiceMediaGraph,
	VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
	type VoiceMediaGraphFailure,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {ScreenShareWatchErrorCode} from '@app/features/voice/state/ScreenShareWatchFailures';
import {describe, expect, it, vi} from 'vitest';

const STREAM_KEY = 'dm:channel-a:connection-a';
const PARTICIPANT_IDENTITY = 'user_1_connection-a';
const TRACK_SID = 'TR_screen_share';

const FIRST_FRAME_TIMEOUT_FAILURE: Partial<VoiceMediaGraphFailure> = {
	code: ScreenShareWatchErrorCode.FirstFrameTimeout,
	reason: 'first-frame-timeout',
};

const REMOTE_SUBSCRIPTION_FAILURE: Partial<VoiceMediaGraphFailure> = {
	code: ScreenShareWatchErrorCode.RemoteTrackSubscriptionFailed,
	reason: 'remote-track-subscription-failed',
};

function createScheduler() {
	let nextId = 1;
	const callbacks = new Map<number, () => void>();
	const scheduledDelays: Array<number> = [];
	const scheduler: ScreenShareVideoSubscriptionRecoveryScheduler = {
		setTimeout: (callback, delayMs) => {
			const timeoutId = nextId;
			nextId += 1;
			callbacks.set(timeoutId, callback);
			scheduledDelays.push(delayMs);
			return timeoutId;
		},
		clearTimeout: (timeoutId) => {
			callbacks.delete(timeoutId);
		},
	};
	return {
		scheduler,
		pendingCount: () => callbacks.size,
		scheduledDelays: () => [...scheduledDelays],
		fire: () => {
			const next = callbacks.entries().next();
			if (next.done) return;
			callbacks.delete(next.value[0]);
			next.value[1]();
		},
	};
}

function createWatchedSnapshot(failure: Partial<VoiceMediaGraphFailure> | null): VoiceMediaGraphSnapshot {
	let snapshot: VoiceMediaGraphSnapshot = createVoiceMediaGraphSnapshot();
	snapshot = transitionVoiceMediaGraph(snapshot, {type: 'watchIntent.add', key: STREAM_KEY});
	snapshot = transitionVoiceMediaGraph(snapshot, {type: 'watch.started', streamKey: STREAM_KEY});
	if (!failure) return snapshot;
	return transitionVoiceMediaGraph(snapshot, {
		type: 'failure.reported',
		failure: {
			code: -2202,
			reason: 'remote-track-subscription-failed',
			reportedAt: 1000,
			source: VOICE_MEDIA_GRAPH_SCREEN_SHARE_SOURCE,
			streamKey: STREAM_KEY,
			participantIdentity: PARTICIPANT_IDENTITY,
			...failure,
		},
	});
}

function createGraphPort(snapshot: VoiceMediaGraphSnapshot): ScreenShareVideoSubscriptionRecoveryGraph {
	return {
		getGraphSnapshot: () => snapshot,
		nowMs: () => 1000,
		transition: () => undefined,
	};
}

function createReceivablePublication(): ScreenShareVideoSubscriptionRecoveryPublication {
	return {
		trackSid: TRACK_SID,
		isSubscribed: true,
		track: {mediaStreamTrack: {readyState: 'live', muted: false}},
	};
}

function acquireWatchingTile(failure: Partial<VoiceMediaGraphFailure> | null) {
	const snapshot = createWatchedSnapshot(failure);
	const {scheduler, fire, pendingCount, scheduledDelays} = createScheduler();
	const coordinator = new ScreenShareVideoSubscriptionRecoveryCoordinator(scheduler, createGraphPort(snapshot));
	const recover = vi.fn();
	coordinator.acquire({
		key: TRACK_SID,
		publication: createReceivablePublication(),
		streamKey: STREAM_KEY,
		participantIdentity: PARTICIPANT_IDENTITY,
		isStillWanted: () => isScreenShareVideoSubscriptionRecoveryWanted(snapshot, STREAM_KEY),
		recover,
	});
	return {coordinator, fire, pendingCount, scheduledDelays, recover};
}

describe('isScreenShareVideoSubscriptionRecoveryWanted', () => {
	it('keeps the lease armed after a first-frame timeout so recovery can still run', () => {
		expect(
			isScreenShareVideoSubscriptionRecoveryWanted(createWatchedSnapshot(FIRST_FRAME_TIMEOUT_FAILURE), STREAM_KEY),
		).toBe(true);
	});

	it('drops the lease for failures other than a first-frame timeout', () => {
		expect(
			isScreenShareVideoSubscriptionRecoveryWanted(createWatchedSnapshot(REMOTE_SUBSCRIPTION_FAILURE), STREAM_KEY),
		).toBe(false);
	});

	it('drops the lease once the stream is no longer watched', () => {
		expect(isScreenShareVideoSubscriptionRecoveryWanted(createVoiceMediaGraphSnapshot(), STREAM_KEY)).toBe(false);
	});
});

describe('ScreenShareVideoSubscriptionRecoveryCoordinator first-frame recovery', () => {
	it('keeps resubscribing while a first-frame timeout stands, up to the recovery limit', () => {
		const {coordinator, fire, pendingCount, recover} = acquireWatchingTile(FIRST_FRAME_TIMEOUT_FAILURE);

		fire();

		expect(recover).toHaveBeenCalledTimes(1);
		expect(recover).toHaveBeenCalledWith('resubscribe');
		expect(pendingCount()).toBe(1);

		fire();
		fire();

		expect(recover).toHaveBeenCalledTimes(3);
		expect(recover).toHaveBeenNthCalledWith(3, 'resubscribe');
		expect(pendingCount()).toBe(1);

		fire();

		expect(recover).toHaveBeenCalledTimes(3);
		expect(coordinator.getActiveSessionCount()).toBe(0);
	});

	it('gives every rebuilt subscription at least the first-frame budget before resubscribing again', () => {
		const {fire, scheduledDelays} = acquireWatchingTile(FIRST_FRAME_TIMEOUT_FAILURE);

		fire();
		fire();

		expect(scheduledDelays().slice(1)).toEqual([20_000, 20_000]);
	});

	it('keeps polling without recovering while a receivable track has no reported failure', () => {
		const {coordinator, fire, recover} = acquireWatchingTile(null);

		fire();
		fire();
		fire();

		expect(recover).not.toHaveBeenCalled();
		expect(coordinator.getActiveSessionCount()).toBe(1);
	});

	it('closes the session without recovering for a failure other than a first-frame timeout', () => {
		const {coordinator, fire, recover} = acquireWatchingTile(REMOTE_SUBSCRIPTION_FAILURE);

		fire();

		expect(recover).not.toHaveBeenCalled();
		expect(coordinator.getActiveSessionCount()).toBe(0);
	});
});
