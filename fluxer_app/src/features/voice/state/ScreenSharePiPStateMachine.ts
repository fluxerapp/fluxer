// SPDX-License-Identifier: AGPL-3.0-or-later

import type {PiPContent} from '@app/features/ui/state/PiP';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface ScreenSharePiPScreenShare {
	participantIdentity: string;
	userId: string;
	connectionId: string;
	source?: ScreenSharePiPScreenShareSource;
}

export type ScreenSharePiPScreenShareSource =
	| 'livekit'
	| 'local-self-state'
	| 'participant-snapshot'
	| 'voice-state'
	| 'watched-stream';

export interface ScreenSharePiPConditions {
	connectedChannelId: string | null;
	connectedGuildId: string | null;
	screenShare: ScreenSharePiPScreenShare | null;
	selectedChannelId: string | null;
	isMobile: boolean;
	disabledBySetting: boolean;
	disabledBySession: boolean;
}

export type ScreenSharePiPCommand = {type: 'open'; content: PiPContent} | {type: 'close'};

export type ScreenSharePiPMode = {kind: 'closed'} | {kind: 'open'; content: PiPContent};

interface ScreenSharePiPContext {
	mode: ScreenSharePiPMode;
	commands: Array<ScreenSharePiPCommand>;
}

export type ScreenSharePiPEvent = {type: 'conditions.changed'; conditions: ScreenSharePiPConditions};

const INITIAL_MODE: ScreenSharePiPMode = {kind: 'closed'};

function buildPiPContent(
	conditions: ScreenSharePiPConditions & {connectedChannelId: string; screenShare: ScreenSharePiPScreenShare},
): PiPContent {
	return {
		type: 'stream',
		participantIdentity: conditions.screenShare.participantIdentity,
		channelId: conditions.connectedChannelId,
		guildId: conditions.connectedGuildId,
		connectionId: conditions.screenShare.connectionId,
		userId: conditions.screenShare.userId,
	};
}

function computeNextMode(conditions: ScreenSharePiPConditions): ScreenSharePiPMode {
	if (!conditions.connectedChannelId) return {kind: 'closed'};
	if (!conditions.screenShare) return {kind: 'closed'};
	if (conditions.isMobile) return {kind: 'closed'};
	if (conditions.disabledBySetting) return {kind: 'closed'};
	if (conditions.disabledBySession) return {kind: 'closed'};
	if (conditions.selectedChannelId === conditions.connectedChannelId) return {kind: 'closed'};
	return {
		kind: 'open',
		content: buildPiPContent({
			...conditions,
			connectedChannelId: conditions.connectedChannelId,
			screenShare: conditions.screenShare,
		}),
	};
}

function isSamePiPContent(a: PiPContent, b: PiPContent): boolean {
	return (
		a.type === b.type &&
		a.participantIdentity === b.participantIdentity &&
		a.channelId === b.channelId &&
		a.guildId === b.guildId &&
		a.connectionId === b.connectionId &&
		a.userId === b.userId
	);
}

function diffModes(previous: ScreenSharePiPMode, next: ScreenSharePiPMode): Array<ScreenSharePiPCommand> {
	if (previous.kind === 'closed' && next.kind === 'closed') return [];
	if (previous.kind === 'closed' && next.kind === 'open') return [{type: 'open', content: next.content}];
	if (previous.kind === 'open' && next.kind === 'closed') return [{type: 'close'}];
	if (previous.kind === 'open' && next.kind === 'open') {
		if (isSamePiPContent(previous.content, next.content)) return [];
		return [{type: 'close'}, {type: 'open', content: next.content}];
	}
	return [];
}

export const screenSharePiPStateMachine = setup({
	types: {} as {
		context: ScreenSharePiPContext;
		events: ScreenSharePiPEvent;
	},
	actions: {
		applyConditions: assign(({context, event}) => {
			if (event.type !== 'conditions.changed') return {};
			const nextMode = computeNextMode(event.conditions);
			return {
				mode: nextMode,
				commands: diffModes(context.mode, nextMode),
			};
		}),
	},
	guards: {
		nextIsOpen: ({context}) => context.mode.kind === 'open',
	},
}).createMachine({
	id: 'screenSharePiP',
	context: {mode: INITIAL_MODE, commands: []},
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'nextIsOpen', target: 'open'}, {target: 'closed'}],
		},
		closed: {
			on: {'conditions.changed': {target: 'routing', actions: 'applyConditions'}},
		},
		open: {
			on: {'conditions.changed': {target: 'routing', actions: 'applyConditions'}},
		},
	},
});

export type ScreenSharePiPSnapshot = SnapshotFrom<typeof screenSharePiPStateMachine>;

export function createScreenSharePiPSnapshot(): ScreenSharePiPSnapshot {
	return initialTransition(screenSharePiPStateMachine, undefined)[0];
}

export function transitionScreenSharePiPSnapshot(
	snapshot: ScreenSharePiPSnapshot,
	event: ScreenSharePiPEvent,
): ScreenSharePiPSnapshot {
	return transition(screenSharePiPStateMachine, snapshot, event)[0] as ScreenSharePiPSnapshot;
}

export function selectScreenSharePiPCommands(snapshot: ScreenSharePiPSnapshot): Array<ScreenSharePiPCommand> {
	return snapshot.context.commands;
}

export function selectScreenSharePiPMode(snapshot: ScreenSharePiPSnapshot): ScreenSharePiPMode {
	return snapshot.context.mode;
}
