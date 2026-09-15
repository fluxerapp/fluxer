// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	buildForwardDefaultDestinations,
	type ForwardDestination,
	type ForwardRowIdentity,
} from '@app/features/app/components/dialogs/shared/ForwardDefaultDestinations';
import {
	type ForwardPinnedDestinations,
	forwardDestinationKey,
	MAX_FORWARD_DESTINATIONS,
	pinForwardDestinations,
	toggleForwardDestination,
} from '@app/features/app/components/dialogs/shared/ForwardDestinationSelection';
import {describe, expect, it} from 'vitest';

const UNPINNED: ForwardPinnedDestinations = {engineQuery: null, pinned: []};
const HISTORY: ReadonlyArray<string> = ['text-1', 'text-2', 'text-3'];

function channel(id: string): ForwardDestination {
	return {id, type: 'channel'};
}

function user(id: string): ForwardDestination {
	return {id, type: 'user'};
}

function resolveChannel(channelId: string): ForwardRowIdentity {
	return {id: channelId, type: 'text_channel'};
}

function resolveDestination(destination: ForwardDestination): ForwardRowIdentity {
	if (destination.type === 'user') return {id: destination.id, type: 'user'};
	return resolveChannel(destination.id);
}

interface PickerState {
	readonly engineQuery: string;
	readonly pinned: ForwardPinnedDestinations;
	readonly selected: ReadonlyArray<ForwardDestination>;
}

interface PickerRender {
	readonly rows: ReadonlyArray<string>;
	readonly state: PickerState;
}

function renderPicker(state: PickerState): PickerRender {
	const pinned = pinForwardDestinations(state.pinned, state.engineQuery, state.selected);
	const rows = buildForwardDefaultDestinations({
		frequentIds: [],
		history: HISTORY,
		isValid: () => true,
		mode: null,
		origin: null,
		pinned: pinned.pinned,
		resolveChannel,
		resolveDestination,
		selected: state.selected,
	});
	return {rows: rows.map((row) => row.id), state: {...state, pinned}};
}

function togglePicker(state: PickerState, destination: ForwardDestination): PickerState {
	const selected = toggleForwardDestination(state.selected, destination);
	const engineQuery = selected.length > state.selected.length ? '' : state.engineQuery;
	return {...state, engineQuery, selected};
}

describe('toggleForwardDestination', () => {
	it('puts the newest pick first and removes a pick in place', () => {
		const picked = toggleForwardDestination(toggleForwardDestination([], channel('text-1')), user('user-1'));
		expect(picked).toEqual([user('user-1'), channel('text-1')]);
		expect(toggleForwardDestination(picked, channel('text-1'))).toEqual([user('user-1')]);
	});

	it('tells a user apart from a channel with the same id', () => {
		expect(forwardDestinationKey(user('1'))).toBe('user:1');
		expect(forwardDestinationKey(channel('1'))).toBe('channel:1');
		expect(toggleForwardDestination([channel('1')], user('1'))).toEqual([user('1'), channel('1')]);
	});

	it('refuses a new pick at the cap but still removes one', () => {
		const full = Array.from({length: MAX_FORWARD_DESTINATIONS}, (_, index) => channel(`text-${index}`));
		expect(toggleForwardDestination(full, channel('text-extra'))).toBe(full);
		expect(toggleForwardDestination(full, channel('text-0'))).toHaveLength(MAX_FORWARD_DESTINATIONS - 1);
	});
});

describe('pinForwardDestinations', () => {
	it('pins the live selection on the first render', () => {
		const selected = [channel('text-1')];
		expect(pinForwardDestinations(UNPINNED, '', selected)).toEqual({engineQuery: '', pinned: selected});
	});

	it('keeps the snapshot while the engine query stays the same', () => {
		const snapshot = pinForwardDestinations(UNPINNED, '', []);
		expect(pinForwardDestinations(snapshot, '', [channel('text-1')])).toBe(snapshot);
	});

	it('pins the live selection again when the engine query changes', () => {
		const snapshot = pinForwardDestinations(UNPINNED, '', []);
		const selected = [user('user-1'), channel('text-1')];
		expect(pinForwardDestinations(snapshot, 'abc', selected)).toEqual({engineQuery: 'abc', pinned: selected});
	});
});

describe('forward default list while selecting', () => {
	it('keeps rows in place until a search clears, then pins the selection newest first', () => {
		const opened = renderPicker({engineQuery: '', pinned: UNPINNED, selected: []});
		expect(opened.rows).toEqual(['text-1', 'text-2', 'text-3']);

		const pickedWithoutQuery = renderPicker(togglePicker(opened.state, channel('text-3')));
		expect(pickedWithoutQuery.state.selected).toEqual([channel('text-3')]);
		expect(pickedWithoutQuery.rows).toEqual(['text-1', 'text-2', 'text-3']);

		const searching = renderPicker({...pickedWithoutQuery.state, engineQuery: 'ada'});
		const pickedFromSearch = renderPicker(togglePicker(searching.state, user('user-9')));
		expect(pickedFromSearch.state.engineQuery).toBe('');
		expect(pickedFromSearch.rows).toEqual(['user-9', 'text-3', 'text-1', 'text-2']);

		const removed = renderPicker(togglePicker(pickedFromSearch.state, channel('text-3')));
		expect(removed.state.selected).toEqual([user('user-9')]);
		expect(removed.rows).toEqual(['user-9', 'text-3', 'text-1', 'text-2']);
	});

	it('re-pins the live selection when a typed query is cleared by hand', () => {
		const opened = renderPicker({engineQuery: '', pinned: UNPINNED, selected: []});
		const picked = renderPicker(togglePicker(opened.state, channel('text-2')));
		const searching = renderPicker({...picked.state, engineQuery: 'x'});
		const cleared = renderPicker({...searching.state, engineQuery: ''});
		expect(cleared.rows).toEqual(['text-2', 'text-1', 'text-3']);
	});
});
