// SPDX-License-Identifier: AGPL-3.0-or-later

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
import {getChannelSearchContextId} from '@app/features/channel/state/ChannelSearch';
import Channels from '@app/features/channel/state/Channels';
import {getChannelSearchIndexingPollInterval} from '@app/features/channel/utils/ChannelSearchPolling';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import {
	isIndexing,
	type MessageSearchParams,
	type MessageSearchScope,
	parseSearchQueryWithSegments,
	searchMessages,
} from '@app/features/search/utils/SearchUtils';
import * as FormUtils from '@app/lib/forms';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';

const AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR = msg({
	message: 'An error occurred while searching',
	comment: 'Error message in the use channel search hook.',
});
const DEFAULT_RESULTS_PER_PAGE = 25;

export type ChannelSearchSortMode = 'newest' | 'oldest' | 'relevant';

export interface ChannelSearchFilters {
	content?: string;
	authorIds?: Array<string>;
	excludeAuthorIds?: Array<string>;
	mentionIds?: Array<string>;
	excludeMentionIds?: Array<string>;
	channelIds?: Array<string>;
	excludeChannelIds?: Array<string>;
	has?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll'>;
	excludeHas?: Array<'image' | 'sound' | 'video' | 'file' | 'sticker' | 'embed' | 'link' | 'poll'>;
	pinned?: boolean;
	authorType?: Array<'user' | 'bot' | 'webhook'>;
	before?: string;
	after?: string;
	during?: string;
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
	performSearch: (
		query: string,
		segments?: Array<SearchSegment>,
		page?: number,
		overrides?: ChannelSearchExecutionOverrides,
	) => Promise<void>;
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

const applySortModeToParams = (params: MessageSearchParams, mode: ChannelSearchSortMode): void => {
	switch (mode) {
		case 'newest':
			params.sortBy = 'timestamp';
			params.sortOrder = 'desc';
			break;
		case 'oldest':
			params.sortBy = 'timestamp';
			params.sortOrder = 'asc';
			break;
		case 'relevant':
			params.sortBy = 'relevance';
			params.sortOrder = 'desc';
			break;
	}
};
const filtersToParams = (filters: ChannelSearchFilters): MessageSearchParams => {
	const params: MessageSearchParams = {};
	if (filters.content?.trim()) {
		params.content = filters.content.trim();
	}
	if (filters.authorIds?.length) {
		params.authorId = filters.authorIds;
	}
	if (filters.excludeAuthorIds?.length) {
		params.excludeAuthorId = filters.excludeAuthorIds;
	}
	if (filters.mentionIds?.length) {
		params.mentions = filters.mentionIds;
	}
	if (filters.excludeMentionIds?.length) {
		params.excludeMentions = filters.excludeMentionIds;
	}
	if (filters.channelIds?.length) {
		params.channelId = filters.channelIds;
	}
	if (filters.excludeChannelIds?.length) {
		params.excludeChannelId = filters.excludeChannelIds;
	}
	if (filters.has?.length) {
		params.has = filters.has;
	}
	if (filters.excludeHas?.length) {
		params.excludeHas = filters.excludeHas;
	}
	if (filters.pinned !== undefined) {
		params.pinned = filters.pinned;
	}
	if (filters.authorType?.length) {
		params.authorType = filters.authorType;
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
	const currentQueryRef = useRef<string>('');
	const currentFiltersRef = useRef<ChannelSearchFilters | null>(null);
	const currentSegmentsRef = useRef<Array<SearchSegment>>([]);
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
	const checkMatureContentChannels = useCallback(
		(params: MessageSearchParams): boolean => {
			const searchingMatureContentChannels: Array<string> = [];
			if (
				GuildMatureContentAgree.isGatedContent({channelId: channel.id, guildId: channel.guildId ?? null}) &&
				!GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})
			) {
				searchingMatureContentChannels.push(channel.id);
			}
			if (params.channelId) {
				for (const channelId of params.channelId) {
					const targetChannel = Channels.getChannel(channelId);
					if (
						targetChannel &&
						GuildMatureContentAgree.isGatedContent({channelId, guildId: targetChannel.guildId ?? null}) &&
						!GuildMatureContentAgree.shouldShowGate({channelId, guildId: targetChannel.guildId ?? null})
					) {
						searchingMatureContentChannels.push(channelId);
					}
				}
			}
			if (searchingMatureContentChannels.length > 0) {
				params.includeNsfw = true;
				return true;
			}
			return false;
		},
		[channel],
	);
	const executeSearch = useCallback(
		async (
			params: MessageSearchParams,
			page: number,
			overrides: ChannelSearchExecutionOverrides = {},
		): Promise<void> => {
			if (!mountedRef.current) return;
			if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) {
				dispatchMachine({
					type: 'channelSearch.succeeded',
					channels: [],
					results: [],
					total: 0,
					hitsPerPage: resultsPerPage,
					page: 1,
				});
				setHasSearched(true);
				return;
			}
			dispatchMachine({type: 'channelSearch.loading'});
			setHasSearched(true);
			try {
				const searchScope = overrides.scope ?? scope;
				const searchSortMode = overrides.sortMode ?? sortMode;
				const searchParams: MessageSearchParams = {
					...params,
					page,
					hitsPerPage: resultsPerPage,
					scope: searchScope,
				};
				applySortModeToParams(searchParams, searchSortMode);
				checkMatureContentChannels(searchParams);
				const result = await searchMessages(
					i18n,
					{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
					searchParams,
				);
				if (!mountedRef.current) return;
				if (isIndexing(result)) {
					dispatchMachine({type: 'channelSearch.indexingStarted'});
				} else {
					dispatchMachine({
						type: 'channelSearch.succeeded',
						channels: result.channels,
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					});
				}
			} catch (error) {
				if (!mountedRef.current) return;
				dispatchMachine({
					type: 'channelSearch.failed',
					error: FormUtils.extractErrorMessage(i18n, error) || i18n._(AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR),
				});
			}
		},
		[channel, resultsPerPage, scope, sortMode, checkMatureContentChannels, dispatchMachine],
	);
	const performSearch = useCallback(
		async (
			query: string,
			segments: Array<SearchSegment> = [],
			page = 1,
			overrides?: ChannelSearchExecutionOverrides,
		): Promise<void> => {
			if (!query['trim']()) return;
			currentQueryRef.current = query;
			currentSegmentsRef.current = segments;
			currentFiltersRef.current = null;
			const params = parseSearchQueryWithSegments(query, segments, {
				historyKey: getChannelSearchContextId(channel) ?? undefined,
				guildId: channel.guildId ?? null,
			});
			await executeSearch(params, page, overrides);
		},
		[executeSearch],
	);
	const performFilterSearch = useCallback(
		async (filters: ChannelSearchFilters, page = 1, overrides?: ChannelSearchExecutionOverrides): Promise<void> => {
			currentFiltersRef.current = filters;
			currentQueryRef.current = '';
			currentSegmentsRef.current = [];
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
			} else if (currentQueryRef.current) {
				const params = parseSearchQueryWithSegments(currentQueryRef.current, currentSegmentsRef.current, {
					historyKey: getChannelSearchContextId(channel) ?? undefined,
					guildId: channel.guildId ?? null,
				});
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
				} else if (currentQueryRef.current) {
					performSearch(currentQueryRef.current, currentSegmentsRef.current, 1, {sortMode: mode});
				}
			}
		},
		[hasSearched, machineState.status, performFilterSearch, performSearch],
	);
	const setScope = useCallback(
		(newScope: MessageSearchScope) => {
			setScopeState(newScope);
			if (hasSearched && machineState.status === 'success') {
				if (currentFiltersRef.current) {
					performFilterSearch(currentFiltersRef.current, 1, {scope: newScope});
				} else if (currentQueryRef.current) {
					performSearch(currentQueryRef.current, currentSegmentsRef.current, 1, {scope: newScope});
				}
			}
		},
		[hasSearched, machineState.status, performFilterSearch, performSearch],
	);
	const reset = useCallback(() => {
		stopPolling();
		dispatchMachine({type: 'channelSearch.reset'});
		setHasSearched(false);
		currentQueryRef.current = '';
		currentFiltersRef.current = null;
		currentSegmentsRef.current = [];
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
			if (GuildMatureContentAgree.shouldShowGate({channelId: channel.id, guildId: channel.guildId ?? null})) {
				stopPolling();
				dispatchMachine({
					type: 'channelSearch.succeeded',
					channels: [],
					results: [],
					total: 0,
					hitsPerPage: resultsPerPage,
					page: 1,
				});
				return;
			}
			try {
				let params: MessageSearchParams;
				if (currentFiltersRef.current) {
					params = filtersToParams(currentFiltersRef.current);
				} else {
					params = parseSearchQueryWithSegments(currentQueryRef.current, currentSegmentsRef.current, {
						historyKey: getChannelSearchContextId(channel) ?? undefined,
						guildId: channel.guildId ?? null,
					});
				}
				params.page = 1;
				params.hitsPerPage = resultsPerPage;
				params.scope = scope;
				applySortModeToParams(params, sortMode);
				checkMatureContentChannels(params);
				const result = await searchMessages(
					i18n,
					{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
					params,
				);
				if (!mountedRef.current) return;
				if (isIndexing(result)) {
					dispatchMachine({type: 'channelSearch.indexingPolled'});
				} else {
					stopPolling();
					dispatchMachine({
						type: 'channelSearch.succeeded',
						channels: result.channels,
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					});
				}
			} catch (error) {
				if (!mountedRef.current) return;
				stopPolling();
				dispatchMachine({
					type: 'channelSearch.failed',
					error: FormUtils.extractErrorMessage(i18n, error) || i18n._(AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR),
				});
			}
		};
		stopPolling();
		pollingTimeoutRef.current = setTimeout(() => {
			pollingTimeoutRef.current = null;
			void poll();
		}, pollInterval);
		return stopPolling;
	}, [
		machineState,
		channel,
		resultsPerPage,
		scope,
		sortMode,
		stopPolling,
		checkMatureContentChannels,
		dispatchMachine,
	]);
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
		performSearch,
		performFilterSearch,
		goToPage,
		setSortMode,
		setScope,
		reset,
	};
};
