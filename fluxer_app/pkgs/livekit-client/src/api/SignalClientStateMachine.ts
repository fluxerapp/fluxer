// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {createFsm, type HandlerArgs, type HandlerFn} from 'machina';

export type SignalLifecycleState =
	| 'new'
	| 'connecting'
	| 'connected'
	| 'offline'
	| 'reconnecting'
	| 'disconnecting'
	| 'closed';

export interface SignalMachineContext {
	attemptId: number;
	lastError?: unknown;
	closeReason?: string;
}

export type SignalMachineInput =
	| {type: 'connect'}
	| {type: 'reconnect'}
	| {type: 'connectComplete'; attemptId: number}
	| {type: 'connectFailed'; error?: unknown}
	| {type: 'reconnectComplete'; attemptId: number}
	| {type: 'reconnectFailed'; error?: unknown; recoverable: boolean}
	| {type: 'transportFailed'; attemptId: number; reason: string}
	| {type: 'close'; reason: string}
	| {type: 'closeComplete'};

type Args = HandlerArgs<SignalMachineContext, SignalLifecycleState>;

type Handler = HandlerFn<SignalMachineContext, SignalLifecycleState>;

function on<T extends SignalMachineInput['type']>(
	handler: (args: Args, event: Extract<SignalMachineInput, {type: T}>) => SignalLifecycleState | undefined,
): Handler {
	return handler as Handler;
}

function isCurrentAttempt(ctx: SignalMachineContext, event: {attemptId: number}) {
	return event.attemptId === ctx.attemptId;
}

const attemptEstablished = on<'connectComplete' | 'reconnectComplete'>(({ctx}, event) => {
	if (!isCurrentAttempt(ctx, event)) {
		return;
	}
	return 'connected';
});

const startConnect = on<'connect'>(({ctx}) => {
	ctx.attemptId += 1;
	ctx.lastError = undefined;
	return 'connecting';
});

const startReconnect = on<'reconnect'>(({ctx}) => {
	ctx.attemptId += 1;
	ctx.lastError = undefined;
	return 'reconnecting';
});

const requestClose = on<'close'>(({ctx}, event) => {
	ctx.closeReason = event.reason;
	return 'disconnecting';
});

const signalStates = {
	new: {
		connect: startConnect,
		close: requestClose,
	},
	connecting: {
		connectComplete: attemptEstablished,
		connectFailed: on<'connectFailed'>(({ctx}, event) => {
			ctx.lastError = event.error;
			return 'closed';
		}),
		close: requestClose,
	},
	connected: {
		reconnect: startReconnect,
		transportFailed: on<'transportFailed'>(({ctx}, event) => {
			if (!isCurrentAttempt(ctx, event)) {
				return;
			}
			ctx.lastError = event.reason;
			return 'offline';
		}),
		close: requestClose,
	},
	offline: {
		connect: startConnect,
		reconnect: startReconnect,
		close: requestClose,
	},
	reconnecting: {
		reconnectComplete: attemptEstablished,
		reconnectFailed: on<'reconnectFailed'>(({ctx}, event) => {
			ctx.lastError = event.error;
			return event.recoverable ? 'offline' : 'closed';
		}),
		close: requestClose,
	},
	disconnecting: {
		closeComplete: 'closed',
	},
	closed: {
		connect: startConnect,
		reconnect: startReconnect,
	},
} as const;

export function createSignalMachine(initialState: SignalLifecycleState = 'new') {
	const context: SignalMachineContext = {attemptId: 0};
	return createFsm({
		id: 'signal',
		initialState: initialState,
		context,
		states: signalStates,
	});
}

export type SignalMachine = ReturnType<typeof createSignalMachine>;

export const signalLifecycleStates = Object.keys(signalStates) as Array<SignalLifecycleState>;
