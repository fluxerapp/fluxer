// @vitest-environment happy-dom
// SPDX-License-Identifier: AGPL-3.0-or-later

import {useGuildMemberAutocompleteSearch} from '@app/features/lexical/composer/useAutocompleteMemberSearch';
import {act} from 'react';
import {createRoot, type Root} from 'react-dom/client';
import {afterEach, beforeEach, describe, expect, it, vi} from 'vitest';

const mocks = vi.hoisted(() => ({
	begin: vi.fn(),
	cancel: vi.fn(),
	destroy: vi.fn(),
	fetch: vi.fn(),
	member: vi.fn((guildId: string, id: string) => ({guildId, id})),
	emit: null as null | ((results: Array<{id: string}>) => void),
	results: vi.fn(),
}));
vi.mock('@app/features/member/state/GuildMembers', () => ({default: {getMember: mocks.member}}));
vi.mock('@app/features/member/state/MemberSearch', () => ({
	default: {
		getSearchContext: (emit: typeof mocks.emit) => {
			mocks.emit = emit;
			return {beginSearch: mocks.begin, cancelSearch: mocks.cancel, destroy: mocks.destroy};
		},
		fetchMembersInBackground: mocks.fetch,
	},
}));
vi.mock('@app/features/messaging/utils/AutocompleteOptionBuilders', () => ({MEMBER_SEARCH_LIMIT: 20}));
let root: Root;
let node: HTMLDivElement;
function Probe({guildId = 'guild-a', query = 'alice'}: {guildId?: string; query?: string | null}) {
	useGuildMemberAutocompleteSearch({guildId, query, setResults: mocks.results});
	return null;
}
beforeEach(async () => {
	vi.clearAllMocks();
	vi.useFakeTimers();
	node = document.createElement('div');
	document.body.append(node);
	root = createRoot(node);
	(globalThis as {IS_REACT_ACT_ENVIRONMENT?: boolean}).IS_REACT_ACT_ENVIRONMENT = true;
	await act(async () => root.render(<Probe data-flx="lexical.composer.use-autocomplete-member-search-test.probe" />));
});
afterEach(async () => {
	await act(async () => root.unmount());
	node.remove();
	vi.useRealTimers();
});
describe('autocomplete member search lifecycle', () => {
	it('fetches only the latest query after rapid edits', async () => {
		await act(async () =>
			root.render(<Probe query="alicia" data-flx="lexical.composer.use-autocomplete-member-search-test.probe--2" />),
		);
		await act(async () => vi.advanceTimersByTime(200));
		expect(mocks.fetch).toHaveBeenCalledExactlyOnceWith('alicia', ['guild-a']);
	});
	it('maps results against the active guild after switching context', async () => {
		await act(async () =>
			root.render(<Probe guildId="guild-b" data-flx="lexical.composer.use-autocomplete-member-search-test.probe--3" />),
		);
		mocks.emit?.([{id: 'user'}]);
		expect(mocks.results).toHaveBeenLastCalledWith([{guildId: 'guild-b', id: 'user'}]);
	});
	it('cancels pending work when the trigger closes and destroys the search context on unmount', async () => {
		await act(async () =>
			root.render(<Probe query={null} data-flx="lexical.composer.use-autocomplete-member-search-test.probe--4" />),
		);
		expect(mocks.cancel).toHaveBeenCalledOnce();
		mocks.emit?.([{id: 'late'}]);
		expect(mocks.results).toHaveBeenLastCalledWith([]);
		await act(async () => vi.advanceTimersByTime(200));
		expect(mocks.fetch).not.toHaveBeenCalled();
		await act(async () => root.unmount());
		expect(mocks.destroy).toHaveBeenCalledOnce();
	});
});
