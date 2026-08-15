// SPDX-License-Identifier: AGPL-3.0-or-later

import {cloneChannelResults, cloneMessageResults} from '@app/features/channel/components/SearchResultsUtils';
import {
	createSearchMachineSnapshot,
	type SearchMachineEvent,
	type SearchMachineSnapshot,
	type SearchMachineState,
	selectSearchMachineState,
	transitionSearchMachineSnapshot,
} from '@app/features/channel/components/SearchStateMachine';
import type {Channel} from '@app/features/channel/models/Channel';
import SelectedGuild from '@app/features/navigation/state/SelectedGuild';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import type {MessageSearchScope} from '@app/features/search/utils/SearchUtils';
import {ME} from '@fluxer/constants/src/AppConstants';
import {makeAutoObservable, observable} from 'mobx';

type ChannelSearchSortMode = 'newest' | 'oldest' | 'relevant';

class ChannelSearchContext {
	searchQuery: string = '';
	searchSegments: Array<SearchSegment> = [];
	activeSearchQuery: string = '';
	activeSearchSegments: Array<SearchSegment> = [];
	isSearchActive = false;
	isInputFocused = false;
	searchRefreshKey = 0;
	machineSnapshot: SearchMachineSnapshot = createSearchMachineSnapshot();
	scrollPosition = 0;
	lastSearchQuery = '';
	lastSearchSegments: Array<SearchSegment> = [];
	lastSearchRefreshKey: number | null = null;
	lastSearchScope: MessageSearchScope | null = null;
	lastSearchSortMode: ChannelSearchSortMode | null = null;
	scope: MessageSearchScope = 'current';

	constructor() {
		makeAutoObservable(this, {
			machineSnapshot: observable.ref,
		});
	}

	get machineState(): SearchMachineState {
		return selectSearchMachineState(this.machineSnapshot);
	}
}

class ChannelSearch {
	private contexts = new Map<string, ChannelSearchContext>();

	constructor() {
		makeAutoObservable<this, 'contexts'>(this, {
			contexts: observable.shallow,
		});
	}

	getContext(contextId: string): ChannelSearchContext {
		let context = this.contexts.get(contextId);
		if (!context) {
			context = new ChannelSearchContext();
			this.contexts.set(contextId, context);
		}
		return context;
	}

	setSearchInput(contextId: string, query: string, segments: Array<SearchSegment>): void {
		const context = this.getContext(contextId);
		context.searchQuery = query;
		context.searchSegments = [...segments];
	}

	setActiveSearch(contextId: string, query: string, segments: Array<SearchSegment>): void {
		const context = this.getContext(contextId);
		context.activeSearchQuery = query;
		context.activeSearchSegments = [...segments];
		context.isSearchActive = true;
		context.searchRefreshKey += 1;
	}

	setIsSearchActive(contextId: string, value: boolean): void {
		const context = this.getContext(contextId);
		context.isSearchActive = value;
	}

	setInputFocused(contextId: string, focused: boolean): void {
		const context = this.getContext(contextId);
		context.isInputFocused = focused;
	}

	closeSearch(contextId: string): void {
		const context = this.getContext(contextId);
		context.searchQuery = '';
		context.searchSegments = [];
		context.activeSearchQuery = '';
		context.activeSearchSegments = [];
		context.isSearchActive = false;
		context.searchRefreshKey = 0;
		context.lastSearchRefreshKey = null;
		context.lastSearchScope = null;
		context.lastSearchSortMode = null;
	}

	sendMachineEvent(
		contextId: string,
		event: SearchMachineEvent,
		query: string,
		segments: Array<SearchSegment>,
		refreshKey: number | null,
		metadata?: {scope?: MessageSearchScope | null; sortMode?: ChannelSearchSortMode | null},
	): void {
		const context = this.getContext(contextId);
		context.machineSnapshot = transitionSearchMachineSnapshot(context.machineSnapshot, cloneSearchMachineEvent(event));
		const machineState = context.machineState;
		if (machineState.status === 'success') {
			context.lastSearchQuery = query;
			context.lastSearchSegments = segments.map((segment) => ({...segment}));
			context.lastSearchRefreshKey = refreshKey;
			context.lastSearchScope = metadata?.scope ?? null;
			context.lastSearchSortMode = metadata?.sortMode ?? null;
		}
	}

	setScrollPosition(contextId: string, position: number): void {
		const context = this.getContext(contextId);
		context.scrollPosition = position;
	}

	setScope(contextId: string, scope: MessageSearchScope): void {
		const context = this.getContext(contextId);
		context.scope = scope;
	}
}

function cloneSearchMachineEvent(event: SearchMachineEvent): SearchMachineEvent {
	if (event.type !== 'channelSearch.succeeded') return event;
	return {
		...event,
		channels: cloneChannelResults(event.channels),
		results: cloneMessageResults(event.results),
	};
}

export function getChannelSearchContextId(channel?: Channel | null, selectedGuildId?: string | null): string | null {
	if (!channel) {
		return null;
	}
	const resolvedGuildId = selectedGuildId ?? SelectedGuild.selectedGuildId;
	const isDmContext = !resolvedGuildId || resolvedGuildId === ME || !channel.guildId || channel.guildId === ME;
	if (isDmContext) {
		return channel.id;
	}
	return channel.guildId ?? resolvedGuildId ?? channel.id;
}

export default new ChannelSearch();
