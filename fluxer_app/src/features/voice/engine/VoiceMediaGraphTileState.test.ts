// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createVoiceMediaGraphSnapshot,
	transitionVoiceMediaGraph,
	type VoiceMediaGraphSnapshot,
} from '@app/features/voice/engine/VoiceMediaGraph';
import {
	selectVoiceMediaGraphStreamTileState,
	VOICE_MEDIA_GRAPH_FIRST_FRAME_RECOVERY_VISIBLE_MS,
} from '@app/features/voice/engine/VoiceMediaGraphTileState';
import {VoiceTrackSource} from '@app/features/voice/engine/VoiceTrackSource';
import {describe, expect, it} from 'vitest';

const STREAM_KEY = 'dm:channel-a:connection-a';
const PARTICIPANT_IDENTITY = 'user_2_connection-a';
const FAILURE_REPORTED_AT = 300;

const target = {
	streamKey: STREAM_KEY,
	participantIdentity: PARTICIPANT_IDENTITY,
	source: VoiceTrackSource.ScreenShare,
};

function subscribe(graph: VoiceMediaGraphSnapshot, hasPublication: boolean): VoiceMediaGraphSnapshot {
	const next = transitionVoiceMediaGraph(graph, {
		type: 'subscription.subscribe',
		participantIdentity: PARTICIPANT_IDENTITY,
		source: VoiceTrackSource.ScreenShare,
		hasPublication,
		observedElement: null,
		context: 'focused',
	});
	return transitionVoiceMediaGraph(next, {type: 'subscription.clearCommands'});
}

function attach(graph: VoiceMediaGraphSnapshot): VoiceMediaGraphSnapshot {
	return transitionVoiceMediaGraph(graph, {
		type: 'subscription.actualChanged',
		participantIdentity: PARTICIPANT_IDENTITY,
		source: VoiceTrackSource.ScreenShare,
		at: 100,
		subscribed: true,
		enabled: true,
		quality: 'high',
	});
}

function reportFailure(graph: VoiceMediaGraphSnapshot, code: number, reason: string): VoiceMediaGraphSnapshot {
	return transitionVoiceMediaGraph(graph, {
		type: 'failure.reported',
		failure: {
			code,
			reason,
			reportedAt: FAILURE_REPORTED_AT,
			streamKey: STREAM_KEY,
			participantIdentity: PARTICIPANT_IDENTITY,
			source: 'screen_share',
		},
	});
}

function watchedWithFailure(code: number, reason: string): VoiceMediaGraphSnapshot {
	return reportFailure(attach(subscribe(createVoiceMediaGraphSnapshot(), true)), code, reason);
}

describe('selectVoiceMediaGraphStreamTileState', () => {
	it('returns idle when the graph has no state for the tile', () => {
		expect(selectVoiceMediaGraphStreamTileState(createVoiceMediaGraphSnapshot(), target)).toBe('idle');
	});

	it('returns watchDesired when only watch intent exists', () => {
		const graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
			type: 'watchIntent.add',
			key: STREAM_KEY,
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('watchDesired');
	});

	it('returns publicationMissing when a desired subscription has no publication', () => {
		const graph = subscribe(createVoiceMediaGraphSnapshot(), false);
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('publicationMissing');
	});

	it('returns attaching when the publication exists but actual is not yet subscribed', () => {
		const graph = subscribe(createVoiceMediaGraphSnapshot(), true);
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('attaching');
	});

	it('returns subscribedAwaitingFrame once actual reports a subscription', () => {
		const graph = attach(subscribe(createVoiceMediaGraphSnapshot(), true));
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('subscribedAwaitingFrame');
	});

	it('returns rendering once a frame is recorded for the attempt', () => {
		let graph = attach(subscribe(createVoiceMediaGraphSnapshot(), true));
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_KEY,
			attemptKey: 'attempt-1',
			renderedAt: 200,
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('rendering');
	});

	it('returns rendering from entry first frame state without an attempt', () => {
		let graph = attach(subscribe(createVoiceMediaGraphSnapshot(), true));
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_KEY,
			attemptKey: 'attempt-1',
			renderedAt: 200,
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, {...target, streamKey: null})).toBe('rendering');
	});

	it('returns failed when a failure is recorded, beating rendering', () => {
		let graph = attach(subscribe(createVoiceMediaGraphSnapshot(), true));
		graph = transitionVoiceMediaGraph(graph, {
			type: 'watch.renderedFrame',
			streamKey: STREAM_KEY,
			attemptKey: 'attempt-1',
			renderedAt: 200,
		});
		graph = transitionVoiceMediaGraph(graph, {
			type: 'failure.reported',
			failure: {
				code: -2202,
				reason: 'remote-track-subscription-failed',
				reportedAt: 300,
				participantIdentity: PARTICIPANT_IDENTITY,
				source: 'screen_share',
			},
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('failed');
	});

	it('returns failed when the last subscription command failed', () => {
		let graph = subscribe(createVoiceMediaGraphSnapshot(), true);
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.commandFailed',
			participantIdentity: PARTICIPANT_IDENTITY,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			code: -2101,
			reason: 'subscription-set-subscribed-failed',
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('failed');
	});

	it('recovers from failed to subscribedAwaitingFrame after a successful actual change', () => {
		let graph = subscribe(createVoiceMediaGraphSnapshot(), true);
		graph = transitionVoiceMediaGraph(graph, {
			type: 'subscription.commandFailed',
			participantIdentity: PARTICIPANT_IDENTITY,
			source: VoiceTrackSource.ScreenShare,
			at: 100,
			code: -2101,
			reason: 'subscription-set-subscribed-failed',
		});
		graph = attach(graph);
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('subscribedAwaitingFrame');
	});

	it('returns recovering while a first-frame timeout still has recovery attempts left', () => {
		const graph = watchedWithFailure(-2303, 'first-frame-timeout');
		expect(
			selectVoiceMediaGraphStreamTileState(graph, target, {
				hasRecoveryBudget: true,
				nowMs: FAILURE_REPORTED_AT + 1000,
			}),
		).toBe('recovering');
	});

	it('returns failed once the first-frame recovery budget is spent', () => {
		const graph = watchedWithFailure(-2303, 'first-frame-timeout');
		expect(
			selectVoiceMediaGraphStreamTileState(graph, target, {
				hasRecoveryBudget: false,
				nowMs: FAILURE_REPORTED_AT + 1000,
			}),
		).toBe('failed');
	});

	it('returns failed once a first-frame timeout has been on screen for the visible cap', () => {
		const graph = watchedWithFailure(-2303, 'first-frame-timeout');
		expect(
			selectVoiceMediaGraphStreamTileState(graph, target, {
				hasRecoveryBudget: true,
				nowMs: FAILURE_REPORTED_AT + VOICE_MEDIA_GRAPH_FIRST_FRAME_RECOVERY_VISIBLE_MS,
			}),
		).toBe('failed');
	});

	it('returns failed for a first-frame timeout when the caller tracks no recovery', () => {
		const graph = watchedWithFailure(-2303, 'first-frame-timeout');
		expect(selectVoiceMediaGraphStreamTileState(graph, target)).toBe('failed');
	});

	it('keeps every other failure code failed while recovery attempts remain', () => {
		const graph = watchedWithFailure(-2202, 'remote-track-subscription-failed');
		expect(
			selectVoiceMediaGraphStreamTileState(graph, target, {
				hasRecoveryBudget: true,
				nowMs: FAILURE_REPORTED_AT + 1000,
			}),
		).toBe('failed');
	});

	it('returns watchDesired for entry-less streams and idle after the watch ends', () => {
		let graph = transitionVoiceMediaGraph(createVoiceMediaGraphSnapshot(), {
			type: 'watchIntent.add',
			key: STREAM_KEY,
		});
		expect(selectVoiceMediaGraphStreamTileState(graph, {...target, participantIdentity: null})).toBe('watchDesired');
		graph = transitionVoiceMediaGraph(graph, {type: 'watchIntent.remove', key: STREAM_KEY});
		expect(selectVoiceMediaGraphStreamTileState(graph, {...target, participantIdentity: null})).toBe('idle');
	});
});
