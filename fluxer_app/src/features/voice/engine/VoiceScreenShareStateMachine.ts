// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PendingScreenShareStopRequest} from '@app/features/voice/engine/voice_screen_share_manager/shared';
import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export type VoiceScreenShareSourceType = 'display' | 'native-display' | 'native-app' | 'device';

export type VoiceScreenShareOperation = 'starting' | 'stopping' | 'replacing' | 'restoring';

export type VoiceScreenShareCodecReadiness = 'idle' | 'loading' | 'ready' | 'timeout';

export type VoiceScreenShareWatchCommand = {type: 'watch.add'; key: string} | {type: 'watch.remove'; key: string};

interface VoiceScreenShareContext {
	active: boolean;
	sourceType: VoiceScreenShareSourceType | null;
	pendingOperation: VoiceScreenShareOperation | null;
	queuedStopRequest: PendingScreenShareStopRequest | null;
	publicationReplaceInFlight: boolean;
	codecReadiness: VoiceScreenShareCodecReadiness;
	encoderVerificationScheduled: boolean;
	endedTrackStopInFlight: boolean;
	streamingPriorityHeld: boolean;
	watchCommands: ReadonlyArray<VoiceScreenShareWatchCommand>;
}

export type VoiceScreenShareEvent =
	| {type: 'share.start'; sourceType: VoiceScreenShareSourceType}
	| {type: 'share.restore'; sourceType: VoiceScreenShareSourceType}
	| {type: 'share.stop'; request?: PendingScreenShareStopRequest | null}
	| {
			type: 'share.replace';
			sourceType: VoiceScreenShareSourceType;
			publicationReplaceInFlight?: boolean;
	  }
	| {
			type: 'share.resolve';
			active: boolean;
			sourceType?: VoiceScreenShareSourceType | null;
			encoderVerificationScheduled?: boolean;
			streamingPriorityHeld?: boolean;
	  }
	| {
			type: 'share.reject';
			active: boolean;
			sourceType?: VoiceScreenShareSourceType | null;
	  }
	| {
			type: 'share.cancel';
			active: boolean;
			sourceType?: VoiceScreenShareSourceType | null;
	  }
	| {type: 'share.endedStop.start'}
	| {type: 'share.endedStop.finish'}
	| {type: 'share.encoderVerification.scheduled'}
	| {type: 'share.encoderVerification.cleared'}
	| {type: 'share.codecReadiness.loading'}
	| {type: 'share.codecReadiness.ready'}
	| {type: 'share.codecReadiness.timeout'}
	| {type: 'share.codecReadiness.reset'}
	| {type: 'share.streamingPriority.set'; active: boolean}
	| {type: 'share.publicationReplace.set'; inFlight: boolean}
	| {type: 'share.queuedStop.clear'}
	| {
			type: 'share.localWatcher.sync';
			enabled: boolean;
			streamKey: string | null;
			currentViewerStreamKeys: ReadonlyArray<string>;
	  }
	| {type: 'share.clearWatchCommands'}
	| {type: 'share.reset'};

const EMPTY_WATCH_COMMANDS: ReadonlyArray<VoiceScreenShareWatchCommand> = [];

function initialContext(): VoiceScreenShareContext {
	return {
		active: false,
		sourceType: null,
		pendingOperation: null,
		queuedStopRequest: null,
		publicationReplaceInFlight: false,
		codecReadiness: 'idle',
		encoderVerificationScheduled: false,
		endedTrackStopInFlight: false,
		streamingPriorityHeld: false,
		watchCommands: EMPTY_WATCH_COMMANDS,
	};
}

function beginOperation(
	context: VoiceScreenShareContext,
	pendingOperation: VoiceScreenShareOperation,
	sourceType: VoiceScreenShareSourceType | null = context.sourceType,
	publicationReplaceInFlight = false,
): VoiceScreenShareContext {
	if (context.pendingOperation) return context;
	return {
		...context,
		pendingOperation,
		sourceType,
		publicationReplaceInFlight,
		codecReadiness: 'idle',
	};
}

function beginStop(
	context: VoiceScreenShareContext,
	request: PendingScreenShareStopRequest | null | undefined,
): VoiceScreenShareContext {
	const normalizedRequest = request
		? {
				sendUpdate: request.sendUpdate,
				playSound: request.playSound,
			}
		: null;
	if (context.pendingOperation) {
		return {
			...context,
			queuedStopRequest: normalizedRequest ?? context.queuedStopRequest,
		};
	}
	return {
		...context,
		pendingOperation: 'stopping',
		queuedStopRequest: null,
		publicationReplaceInFlight: false,
		codecReadiness: 'idle',
		encoderVerificationScheduled: false,
		streamingPriorityHeld: false,
	};
}

function resolveOperation(
	context: VoiceScreenShareContext,
	active: boolean,
	sourceType: VoiceScreenShareSourceType | null | undefined,
	encoderVerificationScheduled: boolean | undefined,
	streamingPriorityHeld: boolean | undefined,
): VoiceScreenShareContext {
	return {
		...context,
		active,
		sourceType: active ? (sourceType ?? context.sourceType) : null,
		pendingOperation: null,
		publicationReplaceInFlight: false,
		codecReadiness: active ? context.codecReadiness : 'idle',
		encoderVerificationScheduled: encoderVerificationScheduled ?? (active && context.encoderVerificationScheduled),
		streamingPriorityHeld: streamingPriorityHeld ?? (active && context.streamingPriorityHeld),
	};
}

function rejectOperation(
	context: VoiceScreenShareContext,
	active: boolean,
	sourceType: VoiceScreenShareSourceType | null | undefined,
): VoiceScreenShareContext {
	return {
		...context,
		active,
		sourceType: active ? (sourceType ?? context.sourceType) : null,
		pendingOperation: null,
		publicationReplaceInFlight: false,
		codecReadiness: active ? context.codecReadiness : 'idle',
		encoderVerificationScheduled: active && context.encoderVerificationScheduled,
		streamingPriorityHeld: active && context.streamingPriorityHeld,
	};
}

function setCodecReadiness(
	context: VoiceScreenShareContext,
	codecReadiness: VoiceScreenShareCodecReadiness,
): VoiceScreenShareContext {
	return context.codecReadiness === codecReadiness ? context : {...context, codecReadiness};
}

function syncLocalWatcher(
	context: VoiceScreenShareContext,
	enabled: boolean,
	streamKey: string | null,
	currentViewerStreamKeys: ReadonlyArray<string>,
): VoiceScreenShareContext {
	if (!streamKey) return context;
	const isWatched = currentViewerStreamKeys.includes(streamKey);
	if (enabled) {
		if (isWatched) return context;
		return {
			...context,
			watchCommands: [...context.watchCommands, {type: 'watch.add', key: streamKey}],
		};
	}
	if (!isWatched) return context;
	return {
		...context,
		watchCommands: [...context.watchCommands, {type: 'watch.remove', key: streamKey}],
	};
}

export const voiceScreenShareStateMachine = setup({
	types: {} as {
		context: VoiceScreenShareContext;
		events: VoiceScreenShareEvent;
	},
	actions: {
		start: assign(({context, event}) =>
			event.type === 'share.start' ? beginOperation(context, 'starting', event.sourceType) : context,
		),
		restore: assign(({context, event}) =>
			event.type === 'share.restore' ? beginOperation(context, 'restoring', event.sourceType) : context,
		),
		stop: assign(({context, event}) => (event.type === 'share.stop' ? beginStop(context, event.request) : context)),
		replace: assign(({context, event}) =>
			event.type === 'share.replace'
				? beginOperation(context, 'replacing', event.sourceType, event.publicationReplaceInFlight === true)
				: context,
		),
		resolve: assign(({context, event}) =>
			event.type === 'share.resolve'
				? resolveOperation(
						context,
						event.active,
						event.sourceType,
						event.encoderVerificationScheduled,
						event.streamingPriorityHeld,
					)
				: context,
		),
		reject: assign(({context, event}) =>
			event.type === 'share.reject' || event.type === 'share.cancel'
				? rejectOperation(context, event.active, event.sourceType)
				: context,
		),
		startEndedStop: assign(({context}) => ({
			...context,
			endedTrackStopInFlight: true,
		})),
		finishEndedStop: assign(({context}) => ({
			...context,
			endedTrackStopInFlight: false,
		})),
		scheduleEncoderVerification: assign(({context}) => ({
			...context,
			encoderVerificationScheduled: true,
		})),
		clearEncoderVerification: assign(({context}) => ({
			...context,
			encoderVerificationScheduled: false,
		})),
		setCodecReadiness: assign(({context, event}) => {
			switch (event.type) {
				case 'share.codecReadiness.loading':
					return setCodecReadiness(context, 'loading');
				case 'share.codecReadiness.ready':
					return setCodecReadiness(context, 'ready');
				case 'share.codecReadiness.timeout':
					return setCodecReadiness(context, 'timeout');
				case 'share.codecReadiness.reset':
					return setCodecReadiness(context, 'idle');
				default:
					return context;
			}
		}),
		setStreamingPriority: assign(({context, event}) =>
			event.type === 'share.streamingPriority.set' ? {...context, streamingPriorityHeld: event.active} : context,
		),
		setPublicationReplaceInFlight: assign(({context, event}) =>
			event.type === 'share.publicationReplace.set' && context.pendingOperation
				? {...context, publicationReplaceInFlight: event.inFlight}
				: context,
		),
		clearQueuedStop: assign(({context}) =>
			context.queuedStopRequest ? {...context, queuedStopRequest: null} : context,
		),
		syncLocalWatcher: assign(({context, event}) =>
			event.type === 'share.localWatcher.sync'
				? syncLocalWatcher(context, event.enabled, event.streamKey, event.currentViewerStreamKeys)
				: context,
		),
		clearWatchCommands: assign(({context}) =>
			context.watchCommands.length === 0 ? context : {...context, watchCommands: EMPTY_WATCH_COMMANDS},
		),
		reset: assign(() => initialContext()),
	},
	guards: {
		isPending: ({context}) => context.pendingOperation != null,
		isActive: ({context}) => context.active,
	},
}).createMachine({
	id: 'voiceScreenShare',
	context: () => initialContext(),
	initial: 'routing',
	on: {
		'share.codecReadiness.loading': {actions: 'setCodecReadiness'},
		'share.codecReadiness.ready': {actions: 'setCodecReadiness'},
		'share.codecReadiness.timeout': {actions: 'setCodecReadiness'},
		'share.codecReadiness.reset': {actions: 'setCodecReadiness'},
	},
	states: {
		routing: {
			always: [{guard: 'isPending', target: 'pending'}, {guard: 'isActive', target: 'active'}, {target: 'inactive'}],
		},
		inactive: {
			on: {
				'share.start': {target: 'routing', actions: 'start'},
				'share.restore': {target: 'routing', actions: 'restore'},
				'share.stop': {target: 'routing', actions: 'stop'},
				'share.replace': {target: 'routing', actions: 'replace'},
				'share.resolve': {target: 'routing', actions: 'resolve'},
				'share.reject': {target: 'routing', actions: 'reject'},
				'share.cancel': {target: 'routing', actions: 'reject'},
				'share.endedStop.start': {target: 'routing', actions: 'startEndedStop'},
				'share.endedStop.finish': {target: 'routing', actions: 'finishEndedStop'},
				'share.encoderVerification.scheduled': {target: 'routing', actions: 'scheduleEncoderVerification'},
				'share.encoderVerification.cleared': {target: 'routing', actions: 'clearEncoderVerification'},
				'share.streamingPriority.set': {target: 'routing', actions: 'setStreamingPriority'},
				'share.publicationReplace.set': {target: 'routing', actions: 'setPublicationReplaceInFlight'},
				'share.queuedStop.clear': {target: 'routing', actions: 'clearQueuedStop'},
				'share.localWatcher.sync': {target: 'routing', actions: 'syncLocalWatcher'},
				'share.clearWatchCommands': {actions: 'clearWatchCommands'},
				'share.reset': {target: 'routing', actions: 'reset'},
			},
		},
		active: {
			on: {
				'share.start': {target: 'routing', actions: 'start'},
				'share.restore': {target: 'routing', actions: 'restore'},
				'share.stop': {target: 'routing', actions: 'stop'},
				'share.replace': {target: 'routing', actions: 'replace'},
				'share.resolve': {target: 'routing', actions: 'resolve'},
				'share.reject': {target: 'routing', actions: 'reject'},
				'share.cancel': {target: 'routing', actions: 'reject'},
				'share.endedStop.start': {target: 'routing', actions: 'startEndedStop'},
				'share.endedStop.finish': {target: 'routing', actions: 'finishEndedStop'},
				'share.encoderVerification.scheduled': {target: 'routing', actions: 'scheduleEncoderVerification'},
				'share.encoderVerification.cleared': {target: 'routing', actions: 'clearEncoderVerification'},
				'share.streamingPriority.set': {target: 'routing', actions: 'setStreamingPriority'},
				'share.publicationReplace.set': {target: 'routing', actions: 'setPublicationReplaceInFlight'},
				'share.queuedStop.clear': {target: 'routing', actions: 'clearQueuedStop'},
				'share.localWatcher.sync': {target: 'routing', actions: 'syncLocalWatcher'},
				'share.clearWatchCommands': {actions: 'clearWatchCommands'},
				'share.reset': {target: 'routing', actions: 'reset'},
			},
		},
		pending: {
			on: {
				'share.start': {actions: 'start'},
				'share.restore': {actions: 'restore'},
				'share.stop': {actions: 'stop'},
				'share.replace': {actions: 'replace'},
				'share.resolve': {target: 'routing', actions: 'resolve'},
				'share.reject': {target: 'routing', actions: 'reject'},
				'share.cancel': {target: 'routing', actions: 'reject'},
				'share.endedStop.start': {actions: 'startEndedStop'},
				'share.endedStop.finish': {actions: 'finishEndedStop'},
				'share.encoderVerification.scheduled': {actions: 'scheduleEncoderVerification'},
				'share.encoderVerification.cleared': {actions: 'clearEncoderVerification'},
				'share.streamingPriority.set': {actions: 'setStreamingPriority'},
				'share.publicationReplace.set': {actions: 'setPublicationReplaceInFlight'},
				'share.queuedStop.clear': {actions: 'clearQueuedStop'},
				'share.localWatcher.sync': {actions: 'syncLocalWatcher'},
				'share.clearWatchCommands': {actions: 'clearWatchCommands'},
				'share.reset': {target: 'routing', actions: 'reset'},
			},
		},
	},
});

export type VoiceScreenShareSnapshot = SnapshotFrom<typeof voiceScreenShareStateMachine>;
export type VoiceScreenShareStateValue = 'inactive' | 'active' | 'pending';

export function createVoiceScreenShareSnapshot(): VoiceScreenShareSnapshot {
	return getInitialSnapshot(voiceScreenShareStateMachine);
}

export function transitionVoiceScreenShareSnapshot(
	snapshot: VoiceScreenShareSnapshot,
	event: VoiceScreenShareEvent,
): VoiceScreenShareSnapshot {
	return transition(voiceScreenShareStateMachine, snapshot, event)[0] as VoiceScreenShareSnapshot;
}

export function getVoiceScreenShareStateValue(snapshot: VoiceScreenShareSnapshot): VoiceScreenShareStateValue {
	if (snapshot.value === 'pending') return 'pending';
	if (snapshot.value === 'active') return 'active';
	return 'inactive';
}
