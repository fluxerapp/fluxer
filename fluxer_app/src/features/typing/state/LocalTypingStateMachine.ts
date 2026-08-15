// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, getInitialSnapshot, type SnapshotFrom, setup, transition} from 'xstate';

export const LOCAL_TYPING_REMOTE_SEND_DELAY_MS = 1500;
export const LOCAL_TYPING_REMOTE_REFRESH_MS = 8000;
export const LOCAL_TYPING_IDLE_RESET_MS = 10000;

export interface LocalTypingMachineInput {
	channelId?: string | null;
	localTyping?: boolean;
	remoteCooldownChannelId?: string | null;
	remoteCooldownUntil?: number | null;
	remotePendingVersion?: number;
}

export interface LocalTypingMachineContext {
	channelId: string | null;
	localTyping: boolean;
	lastChangeAt: number;
	remoteCooldownChannelId: string | null;
	remoteCooldownUntil: number | null;
	lastRemoteSentAt: number | null;
	remoteSendAt: number | null;
	remotePendingVersion: number;
}

export type LocalTypingMachineEvent =
	| {
			type: 'localTyping.started';
			channelId: string;
			now: number;
	  }
	| {
			type: 'localTyping.stopped';
			channelId: string;
	  }
	| {
			type: 'localTyping.remoteSent';
			channelId: string;
			now: number;
			pendingVersion: number;
	  }
	| {
			type: 'localTyping.idleElapsed';
			channelId: string;
			now: number;
	  };

export interface LocalTypingModel {
	channelId: string | null;
	localTyping: boolean;
	remotePending: boolean;
	remotePendingVersion: number;
	remoteCooldownChannelId: string | null;
	remoteCooldownUntil: number | null;
	remoteSendAt: number | null;
	remoteSendDelayMs: number;
	localIdleAt: number | null;
}

export type LocalTypingSnapshot = SnapshotFrom<typeof localTypingStateMachine>;

function initialLocalTypingContext(input: LocalTypingMachineInput = {}): LocalTypingMachineContext {
	return {
		channelId: input.channelId == null ? null : input.channelId,
		localTyping: input.localTyping == null ? false : input.localTyping,
		lastChangeAt: 0,
		remoteCooldownChannelId: input.remoteCooldownChannelId == null ? null : input.remoteCooldownChannelId,
		remoteCooldownUntil: input.remoteCooldownUntil == null ? null : input.remoteCooldownUntil,
		lastRemoteSentAt: null,
		remoteSendAt: null,
		remotePendingVersion: input.remotePendingVersion == null ? 0 : input.remotePendingVersion,
	};
}

function resetLocalTypingContext(context: LocalTypingMachineContext): LocalTypingMachineContext {
	return {
		channelId: null,
		localTyping: false,
		lastChangeAt: 0,
		remoteCooldownChannelId: context.remoteCooldownChannelId,
		remoteCooldownUntil: context.remoteCooldownUntil,
		lastRemoteSentAt: null,
		remoteSendAt: null,
		remotePendingVersion: context.remotePendingVersion,
	};
}

function isRemoteCooldownActive(context: LocalTypingMachineContext, channelId: string, now: number): boolean {
	const cooldownUntil = context.remoteCooldownUntil == null ? 0 : context.remoteCooldownUntil;
	return context.remoteCooldownChannelId === channelId && cooldownUntil > now;
}

function desiredLocalIdleAt(context: LocalTypingMachineContext): number | null {
	if (!context.localTyping || context.channelId == null) {
		return null;
	}
	return context.lastChangeAt + LOCAL_TYPING_IDLE_RESET_MS;
}

export const localTypingStateMachine = setup({
	types: {} as {
		context: LocalTypingMachineContext;
		events: LocalTypingMachineEvent;
		input: LocalTypingMachineInput;
	},
	actions: {
		startLocalTyping: assign(({context, event}) => {
			if (event.type !== 'localTyping.started') {
				return {};
			}
			if (!context.localTyping || context.channelId !== event.channelId) {
				const shouldScheduleRemote = !isRemoteCooldownActive(context, event.channelId, event.now);
				return {
					channelId: event.channelId,
					localTyping: true,
					lastChangeAt: event.now,
					lastRemoteSentAt: null,
					remoteSendAt: shouldScheduleRemote ? event.now + LOCAL_TYPING_REMOTE_SEND_DELAY_MS : null,
					remotePendingVersion: shouldScheduleRemote ? context.remotePendingVersion + 1 : context.remotePendingVersion,
				};
			}
			if (context.remoteSendAt != null) {
				if (context.lastRemoteSentAt == null) {
					return {
						lastChangeAt: event.now,
						remoteSendAt: event.now + LOCAL_TYPING_REMOTE_SEND_DELAY_MS,
						remotePendingVersion: context.remotePendingVersion + 1,
					};
				}
				return {
					lastChangeAt: event.now,
				};
			}
			if (context.lastRemoteSentAt != null && event.now > context.lastRemoteSentAt) {
				return {
					lastChangeAt: event.now,
					remoteSendAt: context.lastRemoteSentAt + LOCAL_TYPING_REMOTE_REFRESH_MS,
					remotePendingVersion: context.remotePendingVersion + 1,
				};
			}
			const shouldScheduleRemote = !isRemoteCooldownActive(context, event.channelId, event.now);
			return {
				lastChangeAt: event.now,
				remoteSendAt: shouldScheduleRemote ? event.now + LOCAL_TYPING_REMOTE_SEND_DELAY_MS : null,
				remotePendingVersion: shouldScheduleRemote ? context.remotePendingVersion + 1 : context.remotePendingVersion,
			};
		}),
		stopLocalTyping: assign(({context, event}) => {
			if (event.type !== 'localTyping.stopped' || !context.localTyping || context.channelId !== event.channelId) {
				return {};
			}
			return resetLocalTypingContext(context);
		}),
		markRemoteSent: assign(({context, event}) => {
			if (
				event.type !== 'localTyping.remoteSent' ||
				!context.localTyping ||
				context.channelId !== event.channelId ||
				context.remotePendingVersion !== event.pendingVersion
			) {
				return {};
			}
			return {
				remoteCooldownChannelId: event.channelId,
				remoteCooldownUntil: event.now + LOCAL_TYPING_REMOTE_REFRESH_MS,
				lastRemoteSentAt: event.now,
				remoteSendAt: null,
			};
		}),
		markIdleElapsed: assign(({context, event}) => {
			if (
				event.type !== 'localTyping.idleElapsed' ||
				!context.localTyping ||
				context.channelId !== event.channelId ||
				event.now - context.lastChangeAt < LOCAL_TYPING_IDLE_RESET_MS
			) {
				return {};
			}
			return resetLocalTypingContext(context);
		}),
	},
}).createMachine({
	id: 'localTyping',
	context: ({input}) => initialLocalTypingContext(input),
	initial: 'ready',
	states: {
		ready: {
			on: {
				'localTyping.started': {actions: 'startLocalTyping'},
				'localTyping.stopped': {actions: 'stopLocalTyping'},
				'localTyping.remoteSent': {actions: 'markRemoteSent'},
				'localTyping.idleElapsed': {actions: 'markIdleElapsed'},
			},
		},
	},
});

export function createLocalTypingSnapshot(input: LocalTypingMachineInput = {}): LocalTypingSnapshot {
	return getInitialSnapshot(localTypingStateMachine, input);
}

export function transitionLocalTypingSnapshot(
	snapshot: LocalTypingSnapshot,
	event: LocalTypingMachineEvent,
): LocalTypingSnapshot {
	return transition(localTypingStateMachine, snapshot, event)[0] as LocalTypingSnapshot;
}

export function selectLocalTypingModel(snapshot: LocalTypingSnapshot): LocalTypingModel {
	return {
		channelId: snapshot.context.channelId,
		localTyping: snapshot.context.localTyping,
		remotePending: snapshot.context.remoteSendAt != null,
		remotePendingVersion: snapshot.context.remotePendingVersion,
		remoteCooldownChannelId: snapshot.context.remoteCooldownChannelId,
		remoteCooldownUntil: snapshot.context.remoteCooldownUntil,
		remoteSendAt: snapshot.context.remoteSendAt,
		remoteSendDelayMs: LOCAL_TYPING_REMOTE_SEND_DELAY_MS,
		localIdleAt: desiredLocalIdleAt(snapshot.context),
	};
}
