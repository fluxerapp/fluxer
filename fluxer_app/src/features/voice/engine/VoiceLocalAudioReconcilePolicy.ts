// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export const VOICE_LOCAL_AUDIO_RECONCILE_REASON_LIMIT = 8;
export const VOICE_LOCAL_AUDIO_RECONCILE_FOLLOW_UP_WARN_LIMIT = 16;

export type VoiceLocalAudioReconcileCoalescerEvent = {type: 'run.requested'; reason: string} | {type: 'run.settled'};

interface VoiceLocalAudioReconcileCoalescerContext {
	generation: number;
	runningReasons: Array<string>;
	pendingReasons: Array<string>;
	coalescedCount: number;
	consecutiveFollowUpRuns: number;
}

function createCoalescerContext(): VoiceLocalAudioReconcileCoalescerContext {
	return {
		generation: 0,
		runningReasons: [],
		pendingReasons: [],
		coalescedCount: 0,
		consecutiveFollowUpRuns: 0,
	};
}

function appendReason(reasons: Array<string>, reason: string): Array<string> {
	if (reasons.includes(reason)) return reasons;
	if (reasons.length >= VOICE_LOCAL_AUDIO_RECONCILE_REASON_LIMIT) return reasons;
	return [...reasons, reason];
}

const voiceLocalAudioReconcileCoalescerMachine = setup({
	types: {} as {
		context: VoiceLocalAudioReconcileCoalescerContext;
		events: VoiceLocalAudioReconcileCoalescerEvent;
	},
	actions: {
		beginRun: assign(({context, event}) =>
			event.type === 'run.requested'
				? {
						generation: context.generation + 1,
						runningReasons: appendReason([], event.reason),
						pendingReasons: [],
						coalescedCount: 0,
						consecutiveFollowUpRuns: 0,
					}
				: context,
		),
		absorbRequest: assign(({context, event}) =>
			event.type === 'run.requested'
				? {
						pendingReasons: appendReason(context.pendingReasons, event.reason),
						coalescedCount: context.coalescedCount + 1,
					}
				: context,
		),
		finishRun: assign(() => ({
			runningReasons: [],
			pendingReasons: [],
			coalescedCount: 0,
			consecutiveFollowUpRuns: 0,
		})),
		beginFollowUpRun: assign(({context}) => ({
			generation: context.generation + 1,
			runningReasons: context.pendingReasons,
			pendingReasons: [],
			coalescedCount: 0,
			consecutiveFollowUpRuns: context.consecutiveFollowUpRuns + 1,
		})),
	},
}).createMachine({
	id: 'voiceLocalAudioReconcileCoalescer',
	context: () => createCoalescerContext(),
	initial: 'idle',
	states: {
		idle: {
			on: {
				'run.requested': {target: 'running', actions: 'beginRun'},
			},
		},
		running: {
			on: {
				'run.requested': {target: 'runningDirty', actions: 'absorbRequest'},
				'run.settled': {target: 'idle', actions: 'finishRun'},
			},
		},
		runningDirty: {
			on: {
				'run.requested': {actions: 'absorbRequest'},
				'run.settled': {target: 'running', actions: 'beginFollowUpRun'},
			},
		},
	},
});

export type VoiceLocalAudioReconcileCoalescerSnapshot = SnapshotFrom<typeof voiceLocalAudioReconcileCoalescerMachine>;

export function createVoiceLocalAudioReconcileCoalescerSnapshot(): VoiceLocalAudioReconcileCoalescerSnapshot {
	return getInitialSnapshot(voiceLocalAudioReconcileCoalescerMachine);
}

export function transitionVoiceLocalAudioReconcileCoalescerSnapshot(
	snapshot: VoiceLocalAudioReconcileCoalescerSnapshot,
	event: VoiceLocalAudioReconcileCoalescerEvent,
): VoiceLocalAudioReconcileCoalescerSnapshot {
	return transition(
		voiceLocalAudioReconcileCoalescerMachine,
		snapshot,
		event,
	)[0] as VoiceLocalAudioReconcileCoalescerSnapshot;
}

export function shouldStartVoiceLocalAudioReconcileRun(
	previous: VoiceLocalAudioReconcileCoalescerSnapshot,
	next: VoiceLocalAudioReconcileCoalescerSnapshot,
): boolean {
	return next.context.generation !== previous.context.generation;
}

export function selectVoiceLocalAudioReconcileRunReasons(
	snapshot: VoiceLocalAudioReconcileCoalescerSnapshot,
): ReadonlyArray<string> {
	return snapshot.context.runningReasons;
}

export function selectVoiceLocalAudioReconcileCoalescedCount(
	snapshot: VoiceLocalAudioReconcileCoalescerSnapshot,
): number {
	return snapshot.context.coalescedCount;
}

export function selectVoiceLocalAudioReconcileStartedRuns(snapshot: VoiceLocalAudioReconcileCoalescerSnapshot): number {
	return snapshot.context.generation;
}

export function shouldWarnAboutVoiceLocalAudioReconcileFollowUpRuns(
	snapshot: VoiceLocalAudioReconcileCoalescerSnapshot,
): boolean {
	return snapshot.context.consecutiveFollowUpRuns >= VOICE_LOCAL_AUDIO_RECONCILE_FOLLOW_UP_WARN_LIMIT;
}

export type VoiceMicrophoneFailureLatchEvent =
	| {type: 'microphone.enableFailed'; channelId: string | null; inputDeviceId: string | null}
	| {type: 'microphone.enableSucceeded'}
	| {type: 'mute.userIntentChanged'}
	| {type: 'scope.observed'; channelId: string | null; inputDeviceId: string | null}
	| {type: 'latch.reset'};

interface VoiceMicrophoneFailureLatchContext {
	channelId: string | null;
	inputDeviceId: string | null;
	failureCount: number;
}

function createLatchContext(): VoiceMicrophoneFailureLatchContext {
	return {
		channelId: null,
		inputDeviceId: null,
		failureCount: 0,
	};
}

const voiceMicrophoneFailureLatchMachine = setup({
	types: {} as {
		context: VoiceMicrophoneFailureLatchContext;
		events: VoiceMicrophoneFailureLatchEvent;
	},
	actions: {
		recordFailure: assign(({context, event}) =>
			event.type === 'microphone.enableFailed'
				? {
						channelId: event.channelId,
						inputDeviceId: event.inputDeviceId,
						failureCount: context.failureCount + 1,
					}
				: context,
		),
		clearLatch: assign(() => createLatchContext()),
	},
	guards: {
		isOutOfScope: ({context, event}) =>
			event.type === 'scope.observed' &&
			(event.channelId !== context.channelId || event.inputDeviceId !== context.inputDeviceId),
	},
}).createMachine({
	id: 'voiceMicrophoneFailureLatch',
	context: () => createLatchContext(),
	initial: 'clear',
	states: {
		clear: {
			on: {
				'microphone.enableFailed': {target: 'latched', actions: 'recordFailure'},
			},
		},
		latched: {
			on: {
				'microphone.enableFailed': {actions: 'recordFailure'},
				'microphone.enableSucceeded': {target: 'clear', actions: 'clearLatch'},
				'mute.userIntentChanged': {target: 'clear', actions: 'clearLatch'},
				'latch.reset': {target: 'clear', actions: 'clearLatch'},
				'scope.observed': {guard: 'isOutOfScope', target: 'clear', actions: 'clearLatch'},
			},
		},
	},
});

export type VoiceMicrophoneFailureLatchSnapshot = SnapshotFrom<typeof voiceMicrophoneFailureLatchMachine>;

export function createVoiceMicrophoneFailureLatchSnapshot(): VoiceMicrophoneFailureLatchSnapshot {
	return getInitialSnapshot(voiceMicrophoneFailureLatchMachine);
}

export function transitionVoiceMicrophoneFailureLatchSnapshot(
	snapshot: VoiceMicrophoneFailureLatchSnapshot,
	event: VoiceMicrophoneFailureLatchEvent,
): VoiceMicrophoneFailureLatchSnapshot {
	return transition(voiceMicrophoneFailureLatchMachine, snapshot, event)[0] as VoiceMicrophoneFailureLatchSnapshot;
}

export function isVoiceMicrophoneFailureLatchActive(snapshot: VoiceMicrophoneFailureLatchSnapshot): boolean {
	return snapshot.value === 'latched';
}

export function selectVoiceMicrophoneFailureCount(snapshot: VoiceMicrophoneFailureLatchSnapshot): number {
	return snapshot.context.failureCount;
}

export function selectVoiceLocalAudioEffectiveSelfMute(input: {
	localSelfMute: boolean;
	microphoneFailureLatched: boolean;
}): boolean {
	return input.localSelfMute || input.microphoneFailureLatched;
}
