// SPDX-License-Identifier: AGPL-3.0-or-later

import {areOrderedStringArraysEqual} from '@app/features/voice/utils/StringArrayUtils';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface IncomingCallManagerSignals {
	incomingCallIds: Array<string>;
	hasRingingCalls: boolean;
	isVoiceConnected: boolean;
	isVoiceConnecting: boolean;
}

export type IncomingCallManagerEvent = {
	type: 'incomingCalls.update';
	signals: IncomingCallManagerSignals;
};

interface IncomingCallManagerContext {
	callQueue: Array<string>;
	shouldPlayIncomingRing: boolean;
}

export interface IncomingCallManagerModel {
	callQueue: Array<string>;
	activeCallId: string | null;
	shouldPlayIncomingRing: boolean;
}

export function resolveIncomingCallQueue(previousQueue: Array<string>, incomingCallIds: Array<string>): Array<string> {
	const incomingSet = new Set(incomingCallIds);
	const retained = previousQueue.filter((channelId) => incomingSet.has(channelId));
	const appended = incomingCallIds.filter((channelId) => !retained.includes(channelId));
	const nextQueue = [...retained, ...appended];
	return areOrderedStringArraysEqual(previousQueue, nextQueue) ? previousQueue : nextQueue;
}

export function shouldPlayIncomingRing(signals: IncomingCallManagerSignals): boolean {
	return signals.hasRingingCalls && !signals.isVoiceConnected && !signals.isVoiceConnecting;
}

export type IncomingRingCommand = 'start' | 'stop' | 'none';

export interface IncomingRingSignals {
	shouldPlayIncomingRing: boolean;
	ringSoundEnabled: boolean;
	ringActive: boolean;
}

export function resolveIncomingRingCommand(signals: IncomingRingSignals): IncomingRingCommand {
	if (signals.shouldPlayIncomingRing && signals.ringSoundEnabled && !signals.ringActive) {
		return 'start';
	}
	if (!signals.shouldPlayIncomingRing && signals.ringActive) {
		return 'stop';
	}
	return 'none';
}

export const incomingCallManagerStateMachine = setup({
	types: {} as {
		context: IncomingCallManagerContext;
		events: IncomingCallManagerEvent;
	},
	actions: {
		applyIncomingCalls: assign(({context, event}) => ({
			callQueue: resolveIncomingCallQueue(context.callQueue, event.signals.incomingCallIds),
			shouldPlayIncomingRing: shouldPlayIncomingRing(event.signals),
		})),
	},
}).createMachine({
	id: 'incomingCallManager',
	context: () => ({
		callQueue: [],
		shouldPlayIncomingRing: false,
	}),
	initial: 'ready',
	on: {
		'incomingCalls.update': {actions: 'applyIncomingCalls'},
	},
	states: {
		ready: {},
	},
});

export type IncomingCallManagerSnapshot = SnapshotFrom<typeof incomingCallManagerStateMachine>;

export function createIncomingCallManagerSnapshot(): IncomingCallManagerSnapshot {
	return initialTransition(incomingCallManagerStateMachine)[0];
}

export function transitionIncomingCallManagerSnapshot(
	snapshot: IncomingCallManagerSnapshot,
	event: IncomingCallManagerEvent,
): IncomingCallManagerSnapshot {
	const [nextSnapshot] = transition(incomingCallManagerStateMachine, snapshot, event);
	return nextSnapshot;
}

export function selectIncomingCallManagerModel(snapshot: IncomingCallManagerSnapshot): IncomingCallManagerModel {
	const callQueue = snapshot.context.callQueue;
	return {
		callQueue,
		activeCallId: callQueue[0] ?? null,
		shouldPlayIncomingRing: snapshot.context.shouldPlayIncomingRing,
	};
}
