// SPDX-License-Identifier: AGPL-3.0-or-later

import {normalizeMemberListRanges} from '@app/features/member/utils/MemberListRangeUtils';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface MemberListViewportMachineInput {
	hasReceivedInitialPayload?: boolean;
	requestedRanges?: Array<[number, number]>;
	subscribedRanges?: Array<[number, number]>;
	totalRows?: number | null;
}

export interface MemberListViewportMachineContext {
	hasReceivedInitialPayload: boolean;
	requestedRanges: Array<[number, number]>;
	subscribedRanges: Array<[number, number]>;
	totalRows: number;
}

export type MemberListViewportMachineEvent =
	| {
			type: 'memberList.storeUpdated';
			hasReceivedInitialPayload: boolean;
			subscribedRanges: Array<[number, number]>;
			totalRows: number | null | undefined;
	  }
	| {
			type: 'memberList.rangesRequested';
			requestedRanges: Array<[number, number]>;
	  }
	| {
			type: 'memberList.reset';
			requestedRanges: Array<[number, number]>;
	  };

export interface MemberListViewportModel {
	isInitialLoading: boolean;
	hasStableVirtualHeight: boolean;
	requestedRanges: Array<[number, number]>;
	renderRanges: Array<[number, number]>;
	totalRows: number;
}

export type MemberListViewportMachineSnapshot = SnapshotFrom<typeof memberListViewportStateMachine>;
export type MemberListViewportStateValue = 'initialLoading' | 'virtualized';

function sanitizeTotalRows(totalRows: number | null | undefined): number {
	if (totalRows == null || !Number.isFinite(totalRows)) {
		return 0;
	}
	return Math.max(0, Math.floor(totalRows));
}

function normalizeRanges(ranges: Array<[number, number]> | null | undefined): Array<[number, number]> {
	return normalizeMemberListRanges(ranges ?? []);
}

export const memberListViewportStateMachine = setup({
	types: {} as {
		context: MemberListViewportMachineContext;
		events: MemberListViewportMachineEvent;
		input: MemberListViewportMachineInput;
	},
	guards: {
		hasInitialPayload: ({context}) => context.hasReceivedInitialPayload,
	},
	actions: {
		applyStoreUpdate: assign({
			hasReceivedInitialPayload: ({context, event}) =>
				event.type === 'memberList.storeUpdated' ? event.hasReceivedInitialPayload : context.hasReceivedInitialPayload,
			subscribedRanges: ({context, event}) =>
				event.type === 'memberList.storeUpdated' ? normalizeRanges(event.subscribedRanges) : context.subscribedRanges,
			totalRows: ({context, event}) =>
				event.type === 'memberList.storeUpdated' ? sanitizeTotalRows(event.totalRows) : context.totalRows,
		}),
		applyRequestedRanges: assign({
			requestedRanges: ({context, event}) =>
				event.type === 'memberList.rangesRequested' ? normalizeRanges(event.requestedRanges) : context.requestedRanges,
		}),
		reset: assign(({event}) => {
			if (event.type !== 'memberList.reset') {
				return {
					hasReceivedInitialPayload: false,
					requestedRanges: [],
					subscribedRanges: [],
					totalRows: 0,
				};
			}
			return {
				hasReceivedInitialPayload: false,
				requestedRanges: normalizeRanges(event.requestedRanges),
				subscribedRanges: [],
				totalRows: 0,
			};
		}),
	},
}).createMachine({
	id: 'memberListViewport',
	context: ({input}) => ({
		hasReceivedInitialPayload: input.hasReceivedInitialPayload ?? false,
		requestedRanges: normalizeRanges(input.requestedRanges),
		subscribedRanges: normalizeRanges(input.subscribedRanges),
		totalRows: sanitizeTotalRows(input.totalRows),
	}),
	initial: 'routing',
	states: {
		routing: {
			always: [{guard: 'hasInitialPayload', target: 'virtualized'}, {target: 'initialLoading'}],
		},
		initialLoading: {
			on: {
				'memberList.storeUpdated': {target: 'routing', actions: 'applyStoreUpdate'},
				'memberList.rangesRequested': {actions: 'applyRequestedRanges'},
				'memberList.reset': {target: 'routing', actions: 'reset'},
			},
		},
		virtualized: {
			on: {
				'memberList.storeUpdated': {target: 'routing', actions: 'applyStoreUpdate'},
				'memberList.rangesRequested': {actions: 'applyRequestedRanges'},
				'memberList.reset': {target: 'routing', actions: 'reset'},
			},
		},
	},
});

export function createMemberListViewportSnapshot(
	input: MemberListViewportMachineInput = {},
): MemberListViewportMachineSnapshot {
	return initialTransition(memberListViewportStateMachine, input)[0];
}

export function transitionMemberListViewportSnapshot(
	snapshot: MemberListViewportMachineSnapshot,
	event: MemberListViewportMachineEvent,
): MemberListViewportMachineSnapshot {
	return transition(memberListViewportStateMachine, snapshot, event)[0] as MemberListViewportMachineSnapshot;
}

export function getMemberListViewportStateValue(
	snapshot: MemberListViewportMachineSnapshot,
): MemberListViewportStateValue {
	return snapshot.value === 'virtualized' ? 'virtualized' : 'initialLoading';
}

export function selectMemberListViewportModel(snapshot: MemberListViewportMachineSnapshot): MemberListViewportModel {
	const stateValue = getMemberListViewportStateValue(snapshot);
	const isInitialLoading = stateValue === 'initialLoading';
	const {requestedRanges, totalRows} = snapshot.context;
	return {
		isInitialLoading,
		hasStableVirtualHeight: !isInitialLoading,
		requestedRanges,
		renderRanges: isInitialLoading ? [] : requestedRanges,
		totalRows,
	};
}

export function resolveMemberListViewportModel(input: MemberListViewportMachineInput): MemberListViewportModel {
	return selectMemberListViewportModel(createMemberListViewportSnapshot(input));
}
