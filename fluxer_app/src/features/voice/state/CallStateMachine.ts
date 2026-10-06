// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CallVoiceState} from '@app/features/gateway/types/GatewayVoiceTypes';
import {areOrderedStringArraysEqual} from '@app/features/voice/utils/StringArrayUtils';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export enum CallLayout {
	MINIMUM = 'MINIMUM',
	NORMAL = 'NORMAL',
	FULL_SCREEN = 'FULL_SCREEN',
}

export interface GatewayCallData {
	channel_id: string;
	message_id?: string;
	region?: string;
	ringing?: Array<string>;
	voice_states?: Array<CallVoiceState>;
}

export interface Call {
	channelId: string;
	messageId: string | null;
	region: string | null;
	ringing: Array<string>;
	layout: CallLayout;
	participants: Array<string>;
}

export interface CallStateContext {
	calls: Record<string, Call>;
	pendingRinging: Record<string, Array<string>>;
}

export interface CallStateInput {
	calls?: Record<string, Call>;
	pendingRinging?: Record<string, ReadonlyArray<string>>;
}

export type CallStateEvent =
	| {type: 'call.create'; channelId: string; call?: GatewayCallData}
	| {type: 'call.update'; call: GatewayCallData}
	| {type: 'call.delete'; channelId: string}
	| {type: 'call.layout.update'; channelId: string; layout: CallLayout}
	| {type: 'call.participants.update'; channelId: string; participants: ReadonlyArray<string>}
	| {type: 'ringing.clear'; channelId: string; userIds?: ReadonlyArray<string>};

function callsEqual(left: Call, right: Call): boolean {
	return (
		left.channelId === right.channelId &&
		left.messageId === right.messageId &&
		left.region === right.region &&
		left.layout === right.layout &&
		areOrderedStringArraysEqual(left.ringing, right.ringing) &&
		areOrderedStringArraysEqual(left.participants, right.participants)
	);
}

function withCall(context: CallStateContext, channelId: string, call: Call): CallStateContext {
	const existing = context.calls[channelId];
	if (existing && callsEqual(existing, call)) return context;
	return {
		...context,
		calls: {
			...context.calls,
			[channelId]: call,
		},
	};
}

function withoutCall(context: CallStateContext, channelId: string): CallStateContext {
	if (!context.calls[channelId] && !context.pendingRinging[channelId]) return context;
	const calls = {...context.calls};
	const pendingRinging = {...context.pendingRinging};
	delete calls[channelId];
	delete pendingRinging[channelId];
	return {
		calls,
		pendingRinging,
	};
}

export function normalizeCallUserIds(userIds?: ReadonlyArray<string> | null): Array<string> {
	if (!userIds || userIds.length === 0) return [];
	const normalized: Array<string> = [];
	const seenUserIds = new Set<string>();
	for (const userIdValue of userIds) {
		const userId = String(userIdValue);
		if (!userId || seenUserIds.has(userId)) continue;
		seenUserIds.add(userId);
		normalized.push(userId);
	}
	return normalized.sort();
}

export function extractParticipantsFromVoiceStates(voiceStates?: ReadonlyArray<CallVoiceState> | null): Array<string> {
	if (!voiceStates || voiceStates.length === 0) return [];
	return normalizeCallUserIds(voiceStates.map((state) => state.user_id));
}

function setPendingRinging(
	context: CallStateContext,
	channelId: string,
	userIds: ReadonlyArray<string>,
): CallStateContext {
	const ringing = normalizeCallUserIds(userIds);
	const existingPending = context.pendingRinging[channelId] ?? [];
	const pendingRinging = {...context.pendingRinging};
	if (ringing.length === 0) {
		delete pendingRinging[channelId];
	} else {
		pendingRinging[channelId] = ringing;
	}
	const call = context.calls[channelId];
	const nextCalls =
		call && !areOrderedStringArraysEqual(call.ringing, ringing)
			? {
					...context.calls,
					[channelId]: {
						...call,
						ringing,
					},
				}
			: context.calls;
	if (areOrderedStringArraysEqual(existingPending, ringing) && nextCalls === context.calls) return context;
	return {
		calls: nextCalls,
		pendingRinging,
	};
}

function clearPendingRinging(
	context: CallStateContext,
	channelId: string,
	userIds?: ReadonlyArray<string>,
): CallStateContext {
	const existing = context.pendingRinging[channelId];
	if (!existing) return context;
	if (!userIds || userIds.length === 0) {
		return setPendingRinging(context, channelId, []);
	}
	const removed = new Set(normalizeCallUserIds(userIds));
	if (removed.size === 0) return context;
	const ringing = existing.filter((id) => !removed.has(id));
	if (ringing.length === existing.length) return context;
	return setPendingRinging(context, channelId, ringing);
}

function createCallFromGateway(channelId: string, data: GatewayCallData): Call {
	const ringing = normalizeCallUserIds(data.ringing);
	return {
		channelId,
		messageId: data.message_id ?? null,
		region: data.region ?? null,
		ringing,
		layout: CallLayout.MINIMUM,
		participants: extractParticipantsFromVoiceStates(data.voice_states),
	};
}

export function isDifferentCallInstance(
	existingCall: Call | undefined,
	nextCall: GatewayCallData | undefined,
): boolean {
	return Boolean(existingCall?.messageId && nextCall?.message_id && existingCall.messageId !== nextCall.message_id);
}

function upsertCall(context: CallStateContext, channelId: string, data: GatewayCallData): CallStateContext {
	const existing = context.calls[channelId];
	if (!existing || isDifferentCallInstance(existing, data)) {
		return setPendingRinging(
			withCall(context, channelId, createCallFromGateway(channelId, data)),
			channelId,
			data.ringing ?? [],
		);
	}
	const hasRingingPayload = data.ringing !== undefined;
	const hasVoiceStatesPayload = data.voice_states !== undefined;
	const call: Call = {
		...existing,
		ringing: hasRingingPayload ? normalizeCallUserIds(data.ringing) : existing.ringing,
		messageId: data.message_id !== undefined ? data.message_id : existing.messageId,
		region: data.region !== undefined ? data.region : existing.region,
		participants: hasVoiceStatesPayload ? extractParticipantsFromVoiceStates(data.voice_states) : existing.participants,
	};
	const nextContext = withCall(context, channelId, call);
	return hasRingingPayload ? setPendingRinging(nextContext, channelId, call.ringing) : nextContext;
}

function updateLayout(context: CallStateContext, channelId: string, layout: CallLayout): CallStateContext {
	const call = context.calls[channelId];
	if (!call || call.layout === layout) return context;
	return withCall(context, channelId, {...call, layout});
}

function updateParticipants(
	context: CallStateContext,
	channelId: string,
	participants: ReadonlyArray<string>,
): CallStateContext {
	const call = context.calls[channelId];
	if (!call) return context;
	const nextParticipants = normalizeCallUserIds(participants);
	if (areOrderedStringArraysEqual(call.participants, nextParticipants)) return context;
	return withCall(context, channelId, {...call, participants: nextParticipants});
}

function normalizeInput(input: CallStateInput): CallStateContext {
	const calls: Record<string, Call> = {};
	for (const [channelId, call] of Object.entries(input.calls ?? {})) {
		calls[channelId] = {
			...call,
			channelId,
			messageId: call.messageId ?? null,
			region: call.region ?? null,
			ringing: normalizeCallUserIds(call.ringing),
			participants: normalizeCallUserIds(call.participants),
		};
	}
	const pendingRinging: Record<string, Array<string>> = {};
	for (const [channelId, ringing] of Object.entries(input.pendingRinging ?? {})) {
		const normalized = normalizeCallUserIds(ringing);
		if (normalized.length > 0) pendingRinging[channelId] = normalized;
	}
	return {calls, pendingRinging};
}

export const callStateMachine = setup({
	types: {} as {
		context: CallStateContext;
		events: CallStateEvent;
		input: CallStateInput;
	},
	actions: {
		createCall: assign(({context, event}) =>
			event.type === 'call.create' && event.call ? upsertCall(context, event.channelId, event.call) : context,
		),
		updateCall: assign(({context, event}) =>
			event.type === 'call.update' ? upsertCall(context, event.call.channel_id, event.call) : context,
		),
		deleteCall: assign(({context, event}) =>
			event.type === 'call.delete' ? withoutCall(context, event.channelId) : context,
		),
		updateLayout: assign(({context, event}) =>
			event.type === 'call.layout.update' ? updateLayout(context, event.channelId, event.layout) : context,
		),
		updateParticipants: assign(({context, event}) =>
			event.type === 'call.participants.update'
				? updateParticipants(context, event.channelId, event.participants)
				: context,
		),
		clearRinging: assign(({context, event}) =>
			event.type === 'ringing.clear' ? clearPendingRinging(context, event.channelId, event.userIds) : context,
		),
	},
	guards: {
		hasCalls: ({context}) => Object.keys(context.calls).length > 0,
	},
}).createMachine({
	id: 'callState',
	context: ({input}) => normalizeInput(input),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'hasCalls', target: 'tracking'}, {target: 'idle'}],
		},
		idle: {
			on: {
				'call.create': {target: 'routing', actions: 'createCall'},
				'call.update': {target: 'routing', actions: 'updateCall'},
				'call.delete': {target: 'routing', actions: 'deleteCall'},
				'call.layout.update': {target: 'routing', actions: 'updateLayout'},
				'call.participants.update': {target: 'routing', actions: 'updateParticipants'},
				'ringing.clear': {target: 'routing', actions: 'clearRinging'},
			},
		},
		tracking: {
			on: {
				'call.create': {target: 'routing', actions: 'createCall'},
				'call.update': {target: 'routing', actions: 'updateCall'},
				'call.delete': {target: 'routing', actions: 'deleteCall'},
				'call.layout.update': {target: 'routing', actions: 'updateLayout'},
				'call.participants.update': {target: 'routing', actions: 'updateParticipants'},
				'ringing.clear': {target: 'routing', actions: 'clearRinging'},
			},
		},
	},
});

export type CallStateSnapshot = SnapshotFrom<typeof callStateMachine>;

export function createCallStateSnapshot(input: CallStateInput = {}): CallStateSnapshot {
	return initialTransition(callStateMachine, input)[0];
}

export function transitionCallStateSnapshot(snapshot: CallStateSnapshot, event: CallStateEvent): CallStateSnapshot {
	return transition(callStateMachine, snapshot, event)[0] as CallStateSnapshot;
}

export function getCallFromSnapshot(snapshot: CallStateSnapshot, channelId: string): Call | undefined {
	return snapshot.context.calls[channelId];
}

export function getCallsFromSnapshot(snapshot: CallStateSnapshot): Array<Call> {
	return Object.values(snapshot.context.calls);
}

export function isUserPendingRingingInSnapshot(
	snapshot: CallStateSnapshot,
	channelId: string,
	userId?: string | null,
): boolean {
	if (!userId) return false;
	return Boolean(snapshot.context.pendingRinging[channelId]?.includes(userId));
}

export function hasActiveCallInSnapshot(
	snapshot: CallStateSnapshot,
	channelId: string,
	participants: ReadonlyArray<string> = [],
): boolean {
	const call = getCallFromSnapshot(snapshot, channelId);
	if (!call) return false;
	const participantIds = normalizeCallUserIds([...call.participants, ...participants]);
	return participantIds.length > 0 || call.ringing.length > 0;
}

export function getActiveCallsFromSnapshot(
	snapshot: CallStateSnapshot,
	participantsByChannel: Record<string, ReadonlyArray<string>> = {},
): Array<Call> {
	return getCallsFromSnapshot(snapshot).filter((call) =>
		hasActiveCallInSnapshot(snapshot, call.channelId, participantsByChannel[call.channelId] ?? []),
	);
}
