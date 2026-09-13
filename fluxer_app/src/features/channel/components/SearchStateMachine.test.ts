// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	createSearchMachineSnapshot,
	selectSearchMachineState,
	transitionSearchMachineSnapshot,
} from '@app/features/channel/components/SearchStateMachine';
import {describe, expect, it} from 'vitest';

describe('SearchStateMachine', () => {
	it('starts idle and transitions through loading to success', () => {
		let snapshot = createSearchMachineSnapshot();
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'idle'});

		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.loading'});
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'loading'});

		snapshot = transitionSearchMachineSnapshot(snapshot, {
			type: 'channelSearch.succeeded',
			results: [],
			channels: [],
			total: 0,
			hitsPerPage: 25,
			page: 1,
		});
		expect(selectSearchMachineState(snapshot)).toEqual({
			status: 'success',
			results: [],
			channels: [],
			total: 0,
			hitsPerPage: 25,
			page: 1,
		});
	});

	it('tracks indexing poll attempts in machine context', () => {
		let snapshot = createSearchMachineSnapshot();
		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.indexingStarted'});
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'indexing', pollCount: 0});

		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.indexingPolled'});
		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.indexingPolled'});
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'indexing', pollCount: 2});
	});

	it('clears retained context on reset', () => {
		let snapshot = createSearchMachineSnapshot();
		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.failed', error: 'Search failed'});
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'error', error: 'Search failed'});

		snapshot = transitionSearchMachineSnapshot(snapshot, {type: 'channelSearch.reset'});
		expect(selectSearchMachineState(snapshot)).toEqual({status: 'idle'});
	});
});
