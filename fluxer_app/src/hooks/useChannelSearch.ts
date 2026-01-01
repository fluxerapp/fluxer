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

import {useLingui} from '@lingui/react/macro';
import {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import type {SearchMachineState} from '~/components/channel/SearchResultsUtils';
import {DEFAULT_SCOPE_VALUE, getScopeOptionsForChannel} from '~/components/channel/searchScopeOptions';
import type {ChannelRecord} from '~/records/ChannelRecord';
import ChannelStore from '~/stores/ChannelStore';
import GuildNSFWAgreeStore from '~/stores/GuildNSFWAgreeStore';
import type {SearchSegment} from '~/utils/SearchSegmentManager';
import {
	isIndexing,
	type MessageSearchParams,
	type MessageSearchScope,
	parseSearchQueryWithSegments,
	searchMessages,
} from '~/utils/SearchUtils';

const INITIAL_POLL_INTERVAL = 5000;
const MAX_POLL_INTERVAL = 30000;
const POLL_BACKOFF_MULTIPLIER = 1.5;
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
	channel: ChannelRecord;
	resultsPerPage?: number;
}

export interface UseChannelSearchReturn {
	machineState: SearchMachineState;
	sortMode: ChannelSearchSortMode;
	scope: MessageSearchScope;
	scopeOptions: ReturnType<typeof getScopeOptionsForChannel>;
	hasSearched: boolean;

	performSearch: (query: string, segments?: Array<SearchSegment>, page?: number) => Promise<void>;
	performFilterSearch: (filters: ChannelSearchFilters, page?: number) => Promise<void>;
	goToPage: (page: number) => void;
	setSortMode: (mode: ChannelSearchSortMode) => void;
	setScope: (scope: MessageSearchScope) => void;
	reset: () => void;
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
	const {t, i18n} = useLingui();
	const [machineState, setMachineState] = useState<SearchMachineState>({status: 'idle'});
	const [sortMode, setSortModeState] = useState<ChannelSearchSortMode>('newest');
	const [scope, setScopeState] = useState<MessageSearchScope>(DEFAULT_SCOPE_VALUE);
	const [hasSearched, setHasSearched] = useState(false);

	const mountedRef = useRef(true);
	const pollingTimeoutRef = useRef<ReturnType<typeof setTimeout> | null>(null);
	const currentQueryRef = useRef<string>('');
	const currentFiltersRef = useRef<ChannelSearchFilters | null>(null);
	const currentSegmentsRef = useRef<Array<SearchSegment>>([]);

	const scopeOptions = useMemo(
		() => getScopeOptionsForChannel(i18n, channel),
		[i18n, channel?.id, channel?.type, channel?.guildId],
	);

	const stopPolling = useCallback(() => {
		if (pollingTimeoutRef.current) {
			clearTimeout(pollingTimeoutRef.current);
			pollingTimeoutRef.current = null;
		}
	}, []);

	const checkNSFWChannels = useCallback(
		(params: MessageSearchParams): boolean => {
			const searchingNSFWChannels: Array<string> = [];

			if (channel.isNSFW() && !GuildNSFWAgreeStore.shouldShowGate(channel.id)) {
				searchingNSFWChannels.push(channel.id);
			}

			if (params.channelId) {
				for (const channelId of params.channelId) {
					const targetChannel = ChannelStore.getChannel(channelId);
					if (targetChannel?.isNSFW() && !GuildNSFWAgreeStore.shouldShowGate(channelId)) {
						searchingNSFWChannels.push(channelId);
					}
				}
			}

			if (searchingNSFWChannels.length > 0) {
				params.includeNsfw = true;
				return true;
			}

			return false;
		},
		[channel],
	);

	const executeSearch = useCallback(
		async (params: MessageSearchParams, page: number): Promise<void> => {
			if (!mountedRef.current) return;

			setMachineState({status: 'loading'});
			setHasSearched(true);

			try {
				const searchParams: MessageSearchParams = {
					...params,
					page,
					hitsPerPage: resultsPerPage,
					scope,
				};

				applySortModeToParams(searchParams, sortMode);
				checkNSFWChannels(searchParams);

				const result = await searchMessages(
					i18n,
					{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
					searchParams,
				);

				if (!mountedRef.current) return;

				if (isIndexing(result)) {
					setMachineState({status: 'indexing', pollCount: 0});
				} else {
					setMachineState({
						status: 'success',
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					});
				}
			} catch (error) {
				if (!mountedRef.current) return;
				setMachineState({
					status: 'error',
					error: (error as Error).message || t`An error occurred while searching`,
				});
			}
		},
		[channel, resultsPerPage, scope, sortMode, checkNSFWChannels],
	);

	const performSearch = useCallback(
		async (query: string, segments: Array<SearchSegment> = [], page = 1): Promise<void> => {
			if (!query.trim()) return;

			currentQueryRef.current = query;
			currentSegmentsRef.current = segments;
			currentFiltersRef.current = null;

			const params = parseSearchQueryWithSegments(query, segments);
			await executeSearch(params, page);
		},
		[executeSearch],
	);

	const performFilterSearch = useCallback(
		async (filters: ChannelSearchFilters, page = 1): Promise<void> => {
			currentFiltersRef.current = filters;
			currentQueryRef.current = '';
			currentSegmentsRef.current = [];

			const params = filtersToParams(filters);
			await executeSearch(params, page);
		},
		[executeSearch],
	);

	const goToPage = useCallback(
		(page: number) => {
			if (currentFiltersRef.current) {
				const params = filtersToParams(currentFiltersRef.current);
				executeSearch(params, page);
			} else if (currentQueryRef.current) {
				const params = parseSearchQueryWithSegments(currentQueryRef.current, currentSegmentsRef.current);
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
					performFilterSearch(currentFiltersRef.current, 1);
				} else if (currentQueryRef.current) {
					performSearch(currentQueryRef.current, currentSegmentsRef.current, 1);
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
					performFilterSearch(currentFiltersRef.current, 1);
				} else if (currentQueryRef.current) {
					performSearch(currentQueryRef.current, currentSegmentsRef.current, 1);
				}
			}
		},
		[hasSearched, machineState.status, performFilterSearch, performSearch],
	);

	const reset = useCallback(() => {
		stopPolling();
		setMachineState({status: 'idle'});
		setHasSearched(false);
		currentQueryRef.current = '';
		currentFiltersRef.current = null;
		currentSegmentsRef.current = [];
	}, [stopPolling]);

	useEffect(() => {
		if (machineState.status !== 'indexing') {
			stopPolling();
			return;
		}

		const pollInterval = Math.min(
			INITIAL_POLL_INTERVAL * POLL_BACKOFF_MULTIPLIER ** machineState.pollCount,
			MAX_POLL_INTERVAL,
		);

		const poll = async () => {
			if (!mountedRef.current) {
				stopPolling();
				return;
			}

			try {
				let params: MessageSearchParams;
				if (currentFiltersRef.current) {
					params = filtersToParams(currentFiltersRef.current);
				} else {
					params = parseSearchQueryWithSegments(currentQueryRef.current, currentSegmentsRef.current);
				}

				params.page = machineState.status === 'indexing' ? 1 : 1;
				params.hitsPerPage = resultsPerPage;
				params.scope = scope;
				applySortModeToParams(params, sortMode);
				checkNSFWChannels(params);

				const result = await searchMessages(
					i18n,
					{contextChannelId: channel.id, contextGuildId: channel.guildId ?? null},
					params,
				);

				if (!mountedRef.current) return;

				if (isIndexing(result)) {
					setMachineState((prev) => ({
						status: 'indexing',
						pollCount: prev.status === 'indexing' ? prev.pollCount + 1 : 0,
					}));
				} else {
					stopPolling();
					setMachineState({
						status: 'success',
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					});
				}
			} catch (error) {
				if (!mountedRef.current) return;
				stopPolling();
				setMachineState({
					status: 'error',
					error: (error as Error).message || t`An error occurred while searching`,
				});
			}
		};

		pollingTimeoutRef.current = setTimeout(poll, pollInterval);

		return stopPolling;
	}, [machineState, channel, resultsPerPage, scope, sortMode, stopPolling, checkNSFWChannels]);

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
