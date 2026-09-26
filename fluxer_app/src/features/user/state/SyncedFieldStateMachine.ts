// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type SyncedFieldFailureReason =
	| 'build-local'
	| 'compare-remote'
	| 'read-remote'
	| 'encode-local'
	| 'payload-too-large'
	| 'roundtrip-threw'
	| 'roundtrip-unstable'
	| 'initialize-reactions'
	| 'machine-loop';

export interface SyncedFieldFailure {
	readonly reason: SyncedFieldFailureReason;
	readonly message: string;
	readonly error?: unknown;
}

export type SyncedFieldCommand =
	| {
			readonly type: 'compareRemote';
			readonly snapshot: unknown;
	  }
	| {
			readonly type: 'applyRemote';
			readonly snapshot: unknown;
	  }
	| {
			readonly type: 'preparePush';
			readonly candidate: unknown;
	  }
	| {
			readonly type: 'pushLocal';
			readonly candidate: unknown;
	  };

interface SyncedFieldMachineContext {
	command: SyncedFieldCommand | null;
	pendingRemoteSnapshot: unknown | null;
	pendingLocalCandidate: unknown | null;
	suppressPush: boolean;
	failure: SyncedFieldFailure | null;
}

export type SyncedFieldMachineEvent =
	| {
			type: 'sync.remoteObserved';
			enabled: boolean;
			snapshot: unknown | undefined;
	  }
	| {
			type: 'sync.remoteMatched';
	  }
	| {
			type: 'sync.remoteNeedsApply';
	  }
	| {
			type: 'sync.localObserved';
			enabled: boolean;
			candidate: unknown | null;
	  }
	| {
			type: 'sync.localAlreadySynced';
	  }
	| {
			type: 'sync.localReadyToPush';
	  }
	| {
			type: 'sync.applyFinished';
	  }
	| {
			type: 'sync.commandHandled';
	  }
	| {
			type: 'sync.failed';
			failure: SyncedFieldFailure;
	  }
	| {
			type: 'sync.reset';
	  };

export type SyncedFieldMachineState =
	| 'idle'
	| 'comparingRemote'
	| 'applyingRemote'
	| 'preparingPush'
	| 'pushing'
	| 'suspended';

export interface SyncedFieldMachineModel {
	readonly state: SyncedFieldMachineState;
	readonly isActive: boolean;
	readonly command: SyncedFieldCommand | null;
	readonly suppressPush: boolean;
	readonly failure: SyncedFieldFailure | null;
}

export type SyncedFieldMachineSnapshot = SnapshotFrom<typeof syncedFieldStateMachine>;

function createEmptyContext(): SyncedFieldMachineContext {
	return {
		command: null,
		pendingRemoteSnapshot: null,
		pendingLocalCandidate: null,
		suppressPush: false,
		failure: null,
	};
}

export const syncedFieldStateMachine = setup({
	types: {} as {
		context: SyncedFieldMachineContext;
		events: SyncedFieldMachineEvent;
	},
	actions: {
		clearCommand: assign({
			command: null,
		}),
		reset: assign(() => createEmptyContext()),
		requestRemoteCompare: assign(({event}) => {
			if (event.type !== 'sync.remoteObserved' || event.snapshot === undefined) {
				return {};
			}
			return {
				command: {type: 'compareRemote', snapshot: event.snapshot},
				pendingRemoteSnapshot: event.snapshot,
				failure: null,
			};
		}),
		requestRemoteApply: assign(({context}) => {
			if (context.pendingRemoteSnapshot == null) {
				return {command: null, suppressPush: false};
			}
			return {
				command: {type: 'applyRemote', snapshot: context.pendingRemoteSnapshot},
				suppressPush: true,
				failure: null,
			};
		}),
		finishRemoteApply: assign({
			command: null,
			pendingRemoteSnapshot: null,
			suppressPush: false,
		}),
		requestPushPreparation: assign(({event}) => {
			if (event.type !== 'sync.localObserved' || event.candidate == null) {
				return {};
			}
			return {
				command: {type: 'preparePush', candidate: event.candidate},
				pendingLocalCandidate: event.candidate,
				failure: null,
			};
		}),
		requestPush: assign(({context}) => {
			if (context.pendingLocalCandidate == null) {
				return {command: null};
			}
			return {
				command: {type: 'pushLocal', candidate: context.pendingLocalCandidate},
				failure: null,
			};
		}),
		finishLocalSync: assign({
			command: null,
			pendingLocalCandidate: null,
		}),
		suspend: assign(({event}) => {
			if (event.type !== 'sync.failed') {
				return {};
			}
			return {
				command: null,
				pendingRemoteSnapshot: null,
				pendingLocalCandidate: null,
				suppressPush: false,
				failure: event.failure,
			};
		}),
	},
	guards: {
		remoteUnavailable: ({event}) =>
			event.type === 'sync.remoteObserved' && (!event.enabled || event.snapshot === undefined),
		localUnavailable: ({event}) => event.type === 'sync.localObserved' && (!event.enabled || event.candidate == null),
		pushSuppressed: ({context}) => context.suppressPush,
	},
}).createMachine({
	id: 'syncedField',
	context: createEmptyContext,
	initial: 'active',
	on: {
		'sync.failed': {target: '.suspended', actions: 'suspend'},
		'sync.reset': {target: '.active.idle', actions: 'reset'},
	},
	states: {
		active: {
			initial: 'idle',
			states: {
				idle: {
					on: {
						'sync.remoteObserved': [
							{guard: 'remoteUnavailable', actions: 'clearCommand'},
							{target: 'comparingRemote', actions: 'requestRemoteCompare'},
						],
						'sync.localObserved': [
							{guard: 'localUnavailable', actions: 'clearCommand'},
							{guard: 'pushSuppressed', actions: 'clearCommand'},
							{target: 'preparingPush', actions: 'requestPushPreparation'},
						],
					},
				},
				comparingRemote: {
					on: {
						'sync.remoteMatched': {target: 'idle', actions: 'finishRemoteApply'},
						'sync.remoteNeedsApply': {target: 'applyingRemote', actions: 'requestRemoteApply'},
					},
				},
				applyingRemote: {
					on: {
						'sync.applyFinished': {target: 'idle', actions: 'finishRemoteApply'},
						'sync.localObserved': {},
					},
				},
				preparingPush: {
					on: {
						'sync.localAlreadySynced': {target: 'idle', actions: 'finishLocalSync'},
						'sync.localReadyToPush': {target: 'pushing', actions: 'requestPush'},
					},
				},
				pushing: {
					on: {
						'sync.commandHandled': {target: 'idle', actions: 'finishLocalSync'},
					},
				},
			},
		},
		suspended: {},
	},
});

export function createSyncedFieldMachineSnapshot(): SyncedFieldMachineSnapshot {
	return initialTransition(syncedFieldStateMachine)[0];
}

export function transitionSyncedFieldMachineSnapshot(
	snapshot: SyncedFieldMachineSnapshot,
	event: SyncedFieldMachineEvent,
): SyncedFieldMachineSnapshot {
	return transition(syncedFieldStateMachine, snapshot, event)[0] as SyncedFieldMachineSnapshot;
}

export function selectSyncedFieldMachineModel(snapshot: SyncedFieldMachineSnapshot): SyncedFieldMachineModel {
	const state = getSyncedFieldMachineState(snapshot);
	return {
		state,
		isActive: state !== 'suspended',
		command: snapshot.context.command,
		suppressPush: snapshot.context.suppressPush,
		failure: snapshot.context.failure,
	};
}

function getSyncedFieldMachineState(snapshot: SyncedFieldMachineSnapshot): SyncedFieldMachineState {
	const value = snapshot.value;
	if (value === 'suspended') {
		return 'suspended';
	}
	if (typeof value === 'object' && value != null && 'active' in value) {
		return (value as {active: SyncedFieldMachineState}).active;
	}
	return 'idle';
}
