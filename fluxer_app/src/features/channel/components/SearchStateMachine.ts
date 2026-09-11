// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {assign, initialTransition, type SnapshotFrom, setup, transition} from 'xstate';

export interface SearchMachineStateIdle {
	status: 'idle';
}

export interface SearchMachineStateLoading {
	status: 'loading';
}

export interface SearchMachineStateIndexing {
	status: 'indexing';
	pollCount: number;
}

export interface SearchMachineStateSuccess {
	status: 'success';
	results: Array<Message>;
	channels: Array<Channel>;
	total: number;
	hitsPerPage: number;
	page: number;
}

export interface SearchMachineStateError {
	status: 'error';
	error: string;
}

export type SearchMachineState =
	| SearchMachineStateIdle
	| SearchMachineStateLoading
	| SearchMachineStateIndexing
	| SearchMachineStateSuccess
	| SearchMachineStateError;

interface SearchMachineContext {
	results: Array<Message>;
	channels: Array<Channel>;
	total: number;
	hitsPerPage: number;
	page: number;
	pollCount: number;
	error: string;
}

export type SearchMachineEvent =
	| {type: 'channelSearch.loading'}
	| {type: 'channelSearch.indexingStarted'}
	| {type: 'channelSearch.indexingPolled'}
	| {
			type: 'channelSearch.succeeded';
			results: Array<Message>;
			channels: Array<Channel>;
			total: number;
			hitsPerPage: number;
			page: number;
	  }
	| {type: 'channelSearch.failed'; error: string}
	| {type: 'channelSearch.reset'};

const createInitialSearchMachineContext = (): SearchMachineContext => ({
	results: [],
	channels: [],
	total: 0,
	hitsPerPage: 0,
	page: 1,
	pollCount: 0,
	error: '',
});

export const searchStateMachine = setup({
	types: {} as {
		context: SearchMachineContext;
		events: SearchMachineEvent;
	},
	actions: {
		resetContext: assign(() => createInitialSearchMachineContext()),
		clearTransientState: assign(() => ({
			pollCount: 0,
			error: '',
		})),
		startIndexing: assign(() => ({
			pollCount: 0,
			error: '',
		})),
		incrementIndexingPoll: assign(({context}) => ({
			pollCount: context.pollCount + 1,
			error: '',
		})),
		applySuccess: assign(({event}) => {
			if (event.type !== 'channelSearch.succeeded') return {};
			return {
				results: event.results,
				channels: event.channels,
				total: event.total,
				hitsPerPage: event.hitsPerPage,
				page: event.page,
				pollCount: 0,
				error: '',
			};
		}),
		applyError: assign(({event}) => ({
			error: event.type === 'channelSearch.failed' ? event.error : '',
		})),
	},
}).createMachine({
	id: 'channelSearch',
	context: createInitialSearchMachineContext(),
	initial: 'idle',
	on: {
		'channelSearch.loading': {target: '.loading', actions: 'clearTransientState'},
		'channelSearch.indexingStarted': {target: '.indexing', actions: 'startIndexing'},
		'channelSearch.indexingPolled': {target: '.indexing', actions: 'incrementIndexingPoll'},
		'channelSearch.succeeded': {target: '.success', actions: 'applySuccess'},
		'channelSearch.failed': {target: '.error', actions: 'applyError'},
		'channelSearch.reset': {target: '.idle', actions: 'resetContext'},
	},
	states: {
		idle: {},
		loading: {},
		indexing: {},
		success: {},
		error: {},
	},
});

export type SearchMachineSnapshot = SnapshotFrom<typeof searchStateMachine>;

export function createSearchMachineSnapshot(): SearchMachineSnapshot {
	return initialTransition(searchStateMachine)[0];
}

export function transitionSearchMachineSnapshot(
	snapshot: SearchMachineSnapshot,
	event: SearchMachineEvent,
): SearchMachineSnapshot {
	return transition(searchStateMachine, snapshot, event)[0] as SearchMachineSnapshot;
}

export function selectSearchMachineState(snapshot: SearchMachineSnapshot): SearchMachineState {
	switch (snapshot.value) {
		case 'loading':
			return {status: 'loading'};
		case 'indexing':
			return {status: 'indexing', pollCount: snapshot.context.pollCount};
		case 'success':
			return {
				status: 'success',
				results: snapshot.context.results,
				channels: snapshot.context.channels,
				total: snapshot.context.total,
				hitsPerPage: snapshot.context.hitsPerPage,
				page: snapshot.context.page,
			};
		case 'error':
			return {status: 'error', error: snapshot.context.error};
		default:
			return {status: 'idle'};
	}
}
