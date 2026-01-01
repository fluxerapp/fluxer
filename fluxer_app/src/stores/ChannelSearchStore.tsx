/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {makeAutoObservable, observable} from 'mobx';
import {ME} from '~/Constants';
import type {SearchMachineState} from '~/components/channel/SearchResultsUtils';
import {cloneMachineState} from '~/components/channel/SearchResultsUtils';
import type {ChannelRecord} from '~/records/ChannelRecord';
import SelectedGuildStore from '~/stores/SelectedGuildStore';
import type {SearchSegment} from '~/utils/SearchSegmentManager';
import type {MessageSearchScope} from '~/utils/SearchUtils';

class ChannelSearchContext {
	searchQuery: string = '';
	searchSegments: Array<SearchSegment> = [];
	activeSearchQuery: string = '';
	activeSearchSegments: Array<SearchSegment> = [];
	isSearchActive = false;
	searchRefreshKey = 0;
	machineState: SearchMachineState = {status: 'idle'};
	scrollPosition = 0;
	lastSearchQuery = '';
	lastSearchSegments: Array<SearchSegment> = [];
	lastSearchRefreshKey: number | null = null;
	scope: MessageSearchScope = 'current';

	constructor() {
		makeAutoObservable(this);
	}
}

class ChannelSearchStore {
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

	closeSearch(contextId: string): void {
		const context = this.getContext(contextId);
		context.searchQuery = '';
		context.searchSegments = [];
		context.activeSearchQuery = '';
		context.activeSearchSegments = [];
		context.isSearchActive = false;
		context.searchRefreshKey = 0;
		context.lastSearchRefreshKey = null;
	}

	setMachineState(
		contextId: string,
		machineState: SearchMachineState,
		query: string,
		segments: Array<SearchSegment>,
		refreshKey: number | null,
	): void {
		const context = this.getContext(contextId);
		context.machineState = cloneMachineState(machineState);
		if (machineState.status === 'success') {
			context.lastSearchQuery = query;
			context.lastSearchSegments = segments.map((segment) => ({...segment}));
			context.lastSearchRefreshKey = refreshKey;
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

export const getChannelSearchContextId = (
	channel?: ChannelRecord | null,
	selectedGuildId?: string | null,
): string | null => {
	if (!channel) {
		return null;
	}

	const resolvedGuildId = selectedGuildId ?? SelectedGuildStore.selectedGuildId;
	const isDmContext = !resolvedGuildId || resolvedGuildId === ME || !channel.guildId || channel.guildId === ME;

	if (isDmContext) {
		return channel.id;
	}

	return channel.guildId ?? resolvedGuildId ?? channel.id;
};

export default new ChannelSearchStore();
