// SPDX-License-Identifier: AGPL-3.0-or-later

import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export type ReviewStep = 'account' | 'scopes' | 'community' | 'permissions';
export type AuthorizePhase =
	| {kind: 'loading'}
	| {kind: 'session_expired'}
	| {kind: 'invalid_request'; message: string}
	| {kind: 'review'; step: ReviewStep}
	| {kind: 'success'; destinationName: string | null};
export type AuthorizeEvent =
	| {type: 'INIT_OK'; step?: ReviewStep}
	| {type: 'INIT_INVALID'; message: string}
	| {type: 'INIT_SESSION_EXPIRED'}
	| {type: 'SET_REVIEW_STEP'; step: ReviewStep}
	| {type: 'SUBMIT_BOT_INVITE_DONE'; destinationName: string | null};

interface AuthorizeMachineContext {
	reviewStep: ReviewStep;
	invalidMessage: string;
	destinationName: string | null;
}

export const INITIAL_PHASE: AuthorizePhase = {kind: 'loading'};

export const authorizeStateMachine = setup({
	types: {} as {
		context: AuthorizeMachineContext;
		events: AuthorizeEvent;
	},
	actions: {
		applyInitOk: assign({
			reviewStep: ({event}) => (event.type === 'INIT_OK' ? (event.step ?? 'account') : 'account'),
			invalidMessage: () => '',
			destinationName: () => null,
		}),
		applyInvalidRequest: assign({
			invalidMessage: ({event}) => (event.type === 'INIT_INVALID' ? event.message : ''),
			destinationName: () => null,
		}),
		applyReviewStep: assign({
			reviewStep: ({event, context}) => (event.type === 'SET_REVIEW_STEP' ? event.step : context.reviewStep),
		}),
		applyBotInviteSuccess: assign({
			destinationName: ({event}) => (event.type === 'SUBMIT_BOT_INVITE_DONE' ? event.destinationName : null),
		}),
	},
}).createMachine({
	id: 'authorize',
	context: {
		reviewStep: 'account',
		invalidMessage: '',
		destinationName: null,
	},
	initial: 'loading',
	on: {
		INIT_INVALID: {target: '.invalid_request', actions: 'applyInvalidRequest'},
		INIT_SESSION_EXPIRED: {target: '.session_expired'},
	},
	states: {
		loading: {
			on: {
				INIT_OK: {target: 'review', actions: 'applyInitOk'},
			},
		},
		session_expired: {},
		invalid_request: {},
		review: {
			on: {
				SET_REVIEW_STEP: {actions: 'applyReviewStep'},
				SUBMIT_BOT_INVITE_DONE: {target: 'success', actions: 'applyBotInviteSuccess'},
			},
		},
		success: {},
	},
});

export type AuthorizeMachineSnapshot = SnapshotFrom<typeof authorizeStateMachine>;
export type AuthorizeMachineStateValue = 'loading' | 'session_expired' | 'invalid_request' | 'review' | 'success';

export function createAuthorizeSnapshot(): AuthorizeMachineSnapshot {
	return initialTransition(authorizeStateMachine)[0];
}

export function transitionAuthorizeSnapshot(
	snapshot: AuthorizeMachineSnapshot,
	event: AuthorizeEvent,
): AuthorizeMachineSnapshot {
	return transition(authorizeStateMachine, snapshot, event)[0] as AuthorizeMachineSnapshot;
}

export function getAuthorizeStateValue(snapshot: AuthorizeMachineSnapshot): AuthorizeMachineStateValue {
	switch (snapshot.value) {
		case 'session_expired':
		case 'invalid_request':
		case 'review':
		case 'success':
			return snapshot.value;
		default:
			return 'loading';
	}
}

export function selectAuthorizePhase(snapshot: AuthorizeMachineSnapshot): AuthorizePhase {
	switch (getAuthorizeStateValue(snapshot)) {
		case 'session_expired':
			return {kind: 'session_expired'};
		case 'invalid_request':
			return {kind: 'invalid_request', message: snapshot.context.invalidMessage};
		case 'review':
			return {kind: 'review', step: snapshot.context.reviewStep};
		case 'success':
			return {kind: 'success', destinationName: snapshot.context.destinationName};
		case 'loading':
			return INITIAL_PHASE;
	}
}
