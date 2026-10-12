// SPDX-License-Identifier: AGPL-3.0-or-later

import {
	applyMatureContentToParamsIfNeeded,
	applySortModeToParams,
	runChannelSearch,
} from '@app/features/channel/components/channel_search_results/ChannelSearchResultsShared';
import {
	DEFAULT_SCOPE_VALUE,
	getScopeOptionsForChannel,
	type ScopeValueOption,
} from '@app/features/channel/components/SearchScopeOptions';
import {
	createSearchMachineSnapshot,
	type SearchMachineEvent,
	type SearchMachineState,
	selectSearchMachineState,
	transitionSearchMachineSnapshot,
} from '@app/features/channel/components/SearchStateMachine';
import type {Channel} from '@app/features/channel/models/Channel';

import {getChannelSearchIndexingPollInterval} from '@app/features/channel/utils/ChannelSearchPolling';

import type {MessageSearchParams, MessageSearchScope} from '@app/features/search/utils/SearchUtils';

import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const DEFAULT_RESULTS_PER_PAGE = 25;

export type ChannelSearchSortMode = 'newest' | 'oldest' | 'relevant';

export interface ChannelSearchFilters {
	content?: string;
	authorIds?: Array<string>;
	channelIds?: Array<string>;
	has?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll'>;
}

export interface UseChannelSearchOptions {
	channel: Channel;
	resultsPerPage?: number;
}

export interface UseChannelSearchReturn {
	machineState: SearchMachineState;
	sortMode: ChannelSearchSortMode;
	scope: MessageSearchScope;
	scopeOptions: Array<ScopeValueOption>;
	hasSearched: boolean;
	performFilterSearch: (
		filters: ChannelSearchFilters,
		page?: number,
		overrides?: ChannelSearchExecutionOverrides,
	) => Promise<void>;
	goToPage: (page: number) => void;
	setSortMode: (mode: ChannelSearchSortMode) => void;
	setScope: (scope: MessageSearchScope) => void;
	reset: () => void;
}

interface ChannelSearchExecutionOverrides {
	scope?: MessageSearchScope;
	sortMode?: ChannelSearchSortMode;
}

const filtersToParams = (filters: ChannelSearchFilters): MessageSearchParams => {
	const params: MessageSearchParams = {};
	if (filters.content?.trim()) {
		params.content = filters.content.trim();
	}
	if (filters.authorIds?.length) {
		params.authorId = filters.authorIds;
	}

	if (filters.channelIds?.length) {
		params.channelId = filters.channelIds;
	}

	if (filters.has?.length) {
		params.has = filters.has;
	}

	return params;
};
export const useChannelSearch = ({
	channel,
	resultsPerPage = DEFAULT_RESULTS_PER_PAGE,
}: UseChannelSearchOptions): UseChannelSearchReturn => {
	const {i18n} = useLingui();
	const [searchSnapshot, setSearchSnapshot] = useState(createSearchMachineSnapshot);
	const machineState = useMemo<SearchMachineState>(() => selectSearchMachineState(searchSnapshot), [searchSnapshot]);
	const dispatchMachine = useCallback((event: SearchMachineEvent) => {
		setSearchSnapshot((snapshot) => transitionSearchMachineSnapshot(snapshot, event));
	}, []);
	const [sortMode, setSortModeState] = useState<ChannelSearchSortMode>('newest');
	const [scope, setScopeState] = useState<MessageSearchScope>(DEFAULT_SCOPE_VALUE);
	const [hasSearched, setHasSearched] = useState(false);
	const mountedRef = useRef(true);
	const pollingTimeoutRef = useRef<NodeJS.Timeout | null>(null);
	const currentFiltersRef = useRef<ChannelSearchFilters | null>(null);
	const scopeOptions = useMemo(
		() => getScopeOptionsForChannel(i18n, channel),
		[i18n.locale, channel?.id, channel?.type, channel?.guildId],
	);
	const stopPolling = useCallback(() => {
		if (pollingTimeoutRef.current) {
			clearTimeout(pollingTimeoutRef.current);
			pollingTimeoutRef.current = null;
		}
	}, []);
	const executeSearch = useCallback(
		async (
			params: MessageSearchParams,
			page: number,
			overrides: ChannelSearchExecutionOverrides = {},
		): Promise<void> => {
			if (!mountedRef.current) return;
			dispatchMachine({type: 'channelSearch.loading'});
			setHasSearched(true);
			const searchParams: MessageSearchParams = {
				...params,
				page,
				hitsPerPage: resultsPerPage,
				scope: overrides.scope ?? scope,
			};
			applySortModeToParams(searchParams, overrides.sortMode ?? sortMode);
			applyMatureContentToParamsIfNeeded(searchParams, channel.id);
			const event = await runChannelSearch(
				i18n,
				{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
				searchParams,
			);
			if (!mountedRef.current) return;
			dispatchMachine(event);
		},
		[channel, resultsPerPage, scope, sortMode, dispatchMachine, i18n],
	);
	const performFilterSearch = useCallback(
		async (filters: ChannelSearchFilters, page = 1, overrides?: ChannelSearchExecutionOverrides): Promise<void> => {
			currentFiltersRef.current = filters;

			const params = filtersToParams(filters);
			await executeSearch(params, page, overrides);
		},
		[executeSearch],
	);
	const goToPage = useCallback(
		(page: number) => {
			if (currentFiltersRef.current) {
				const params = filtersToParams(currentFiltersRef.current);
				executeSearch(params, page);
			}
		},
		[executeSearch],
	);
	const setSortMode = useCallback(
		(mode: ChannelSearchSortMode) => {
			setSortModeState(mode);
			if (hasSearched && machineState.status === 'success') {
				if (currentFiltersRef.current) {
					performFilterSearch(currentFiltersRef.current, 1, {sortMode: mode});
				}
			}
		},
		[hasSearched, machineState.status, performFilterSearch],
	);
	const setScope = useCallback(
		(newScope: MessageSearchScope) => {
			setScopeState(newScope);
			if (hasSearched && machineState.status === 'success') {
				if (currentFiltersRef.current) {
					performFilterSearch(currentFiltersRef.current, 1, {scope: newScope});
				}
			}
		},
		[hasSearched, machineState.status, performFilterSearch],
	);
	const reset = useCallback(() => {
		stopPolling();
		dispatchMachine({type: 'channelSearch.reset'});
		setHasSearched(false);

		currentFiltersRef.current = null;
	}, [stopPolling, dispatchMachine]);
	useEffect(() => {
		if (machineState.status !== 'indexing') {
			stopPolling();
			return;
		}
		const pollInterval = getChannelSearchIndexingPollInterval(machineState.pollCount);
		const poll = async () => {
			if (!mountedRef.current) {
				stopPolling();
				return;
			}
			if (!currentFiltersRef.current) return;
			const params = filtersToParams(currentFiltersRef.current);
			params.page = 1;
			params.hitsPerPage = resultsPerPage;
			params.scope = scope;
			applySortModeToParams(params, sortMode);
			applyMatureContentToParamsIfNeeded(params, channel.id);
			const event = await runChannelSearch(
				i18n,
				{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
				params,
			);
			if (!mountedRef.current) return;
			if (event.type === 'channelSearch.indexingStarted') dispatchMachine({type: 'channelSearch.indexingPolled'});
			else {
				stopPolling();
				dispatchMachine(event);
			}
		};
		stopPolling();
		pollingTimeoutRef.current = setTimeout(() => {
			pollingTimeoutRef.current = null;
			void poll();
		}, pollInterval);
		return stopPolling;
	}, [machineState, channel, resultsPerPage, scope, sortMode, stopPolling, i18n, dispatchMachine]);
	useEffect(() => {
		mountedRef.current = true;
		return () => {
			mountedRef.current = false;
			stopPolling();
		};
	}, [stopPolling]);
	return {
		machineState,
		sortMode,
		scope,
		scopeOptions,
		hasSearched,
		performFilterSearch,
		goToPage,
		setSortMode,
		setScope,
		reset,
	};
};
