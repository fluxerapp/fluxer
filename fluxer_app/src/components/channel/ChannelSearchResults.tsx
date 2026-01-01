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

import type {MessageDescriptor} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';

import type {IconProps} from '@phosphor-icons/react';
import {
	ChatCenteredDotsIcon,
	CircleNotchIcon,
	ClockClockwiseIcon,
	ClockCounterClockwiseIcon,
	EnvelopeSimpleIcon,
	GlobeIcon,
	HashIcon,
	MagnifyingGlassIcon,
	SparkleIcon,
	UsersIcon,
} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import {MessagePreviewContext} from '~/Constants';
import {Message, type MessageBehaviorOverrides} from '~/components/channel/Message';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {MessageContextPrefix} from '~/components/shared/MessageContextPrefix/MessageContextPrefix';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import {ContextMenuCloseProvider} from '~/components/uikit/ContextMenu/ContextMenu';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItemRadio} from '~/components/uikit/ContextMenu/MenuItemRadio';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {Spinner} from '~/components/uikit/Spinner';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelSearchStore, {getChannelSearchContextId} from '~/stores/ChannelSearchStore';
import ChannelStore from '~/stores/ChannelStore';
import GuildNSFWAgreeStore from '~/stores/GuildNSFWAgreeStore';
import GuildStore from '~/stores/GuildStore';
import UserStore from '~/stores/UserStore';
import {applyChannelSearchHighlight, clearChannelSearchHighlight} from '~/utils/ChannelSearchHighlight';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {goToMessage} from '~/utils/MessageNavigator';
import * as RouterUtils from '~/utils/RouterUtils';
import {tokenizeSearchQuery} from '~/utils/SearchQueryTokenizer';
import type {SearchSegment} from '~/utils/SearchSegmentManager';
import {
	isIndexing,
	type MessageSearchParams,
	type MessageSearchScope,
	parseSearchQueryWithSegments,
	searchMessages,
} from '~/utils/SearchUtils';
import './ChannelSearchHighlight.css';
import styles from './ChannelSearchResults.module.css';
import type {SearchMachineState} from './SearchResultsUtils';
import {areSegmentsEqual} from './SearchResultsUtils';
import {DEFAULT_SCOPE_VALUE, getScopeOptionsForChannel} from './searchScopeOptions';

const getChannelDisplayName = (channel: ChannelRecord): string => {
	if (channel.isPrivate()) {
		return ChannelUtils.getDMDisplayName(channel);
	}

	return channel.name?.trim() || ChannelUtils.getName(channel);
};

const getChannelGuild = (channel: ChannelRecord): GuildRecord | null => {
	if (!channel.guildId) {
		return null;
	}

	return GuildStore.getGuild(channel.guildId) ?? null;
};

const getChannelPath = (channel: ChannelRecord): string => {
	if (channel.guildId) {
		return Routes.guildChannel(channel.guildId, channel.id);
	}
	return Routes.dmChannel(channel.id);
};

const _renderChannelIcon = (channel: ChannelRecord): React.ReactNode => {
	if (channel.isPersonalNotes()) {
		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}

	if (channel.isDM()) {
		const recipientId = channel.recipientIds[0];
		const recipient = recipientId ? UserStore.getUser(recipientId) : null;

		if (recipient) {
			return (
				<div className={styles.channelIconAvatar}>
					<Avatar user={recipient} size={20} status={null} className={styles.channelIconAvatarImage} />
				</div>
			);
		}

		return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
	}

	if (channel.isGroupDM()) {
		return (
			<div className={styles.channelIconAvatar}>
				<GroupDMAvatar channel={channel} size={20} disableStatusIndicator />
			</div>
		);
	}

	return ChannelUtils.getIcon(channel, {className: styles.channelIcon});
};

interface ChannelSearchResultsProps {
	channel: ChannelRecord;
	searchQuery: string;
	searchSegments: Array<SearchSegment>;
	onClose: () => void;
	refreshKey?: number;
}

type PaginationItem = number | 'ellipsis-left' | 'ellipsis-right';

const INITIAL_POLL_INTERVAL = 5000;
const MAX_POLL_INTERVAL = 30000;
const POLL_BACKOFF_MULTIPLIER = 1.5;
const DEFAULT_MAX_VISIBLE_PAGES = 7;
const RESULTS_PER_PAGE = 25;

const getAdaptiveVisiblePageCount = (): number => {
	if (window.innerWidth <= 420) {
		return 3;
	}

	if (window.innerWidth <= 640) {
		return 5;
	}

	return DEFAULT_MAX_VISIBLE_PAGES;
};

const buildPaginationRange = (currentPage: number, totalPages: number, maxVisible: number): Array<PaginationItem> => {
	if (totalPages <= 0) {
		return [];
	}

	const effectiveMax = Math.max(3, maxVisible);

	if (totalPages <= effectiveMax) {
		return Array.from({length: totalPages}, (_, index) => index + 1);
	}

	const innerSlots = Math.max(1, effectiveMax - 2);
	let start = currentPage - Math.floor(innerSlots / 2);
	let end = currentPage + Math.ceil(innerSlots / 2) - 1;

	start = Math.max(2, start);
	end = Math.min(totalPages - 1, end);

	while (end - start + 1 < innerSlots) {
		if (start > 2) {
			start -= 1;
		} else if (end < totalPages - 1) {
			end += 1;
		} else {
			break;
		}
	}

	const range: Array<PaginationItem> = [1];

	if (start > 2) {
		range.push('ellipsis-left');
	}

	for (let page = start; page <= end; page++) {
		range.push(page);
	}

	if (end < totalPages - 1) {
		range.push('ellipsis-right');
	}

	range.push(totalPages);

	return range;
};

const DETACHED_MESSAGE_BEHAVIOR: MessageBehaviorOverrides = {
	isEditing: false,
	isReplying: false,
	isHighlight: false,
	contextMenuOpen: false,
	disableContextMenuTracking: true,
} as const;

type ChannelSearchSortMode = 'newest' | 'oldest' | 'relevant';

const getSortModeOptions = (
	t: (descriptor: MessageDescriptor) => string,
): Array<{mode: ChannelSearchSortMode; label: string}> => [
	{mode: 'newest', label: t(msg({message: 'Newest'}))},
	{mode: 'oldest', label: t(msg({message: 'Oldest'}))},
	{mode: 'relevant', label: t(msg({message: 'Most Relevant'}))},
];

const SCOPE_ICON_COMPONENTS: Record<MessageSearchScope, React.ComponentType<IconProps>> = {
	current: HashIcon,
	all_dms: EnvelopeSimpleIcon,
	open_dms: ChatCenteredDotsIcon,
	all_guilds: GlobeIcon,
	all: UsersIcon,
	open_dms_and_all_guilds: UsersIcon,
};

const SORT_ICON_COMPONENTS: Record<ChannelSearchSortMode, React.ComponentType<IconProps>> = {
	newest: ClockClockwiseIcon,
	oldest: ClockCounterClockwiseIcon,
	relevant: SparkleIcon,
};

const renderScopeIcon = (scope: MessageSearchScope, size = 16): React.ReactNode => {
	const IconComponent = SCOPE_ICON_COMPONENTS[scope] ?? HashIcon;
	return <IconComponent size={size} weight="bold" />;
};

const renderSortIcon = (mode: ChannelSearchSortMode, size = 16): React.ReactNode => {
	const IconComponent = SORT_ICON_COMPONENTS[mode];
	return <IconComponent size={size} weight="bold" />;
};

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

const shouldShowGuildMetaForScope = (guild: GuildRecord | null, scope: MessageSearchScope): boolean => {
	if (!guild) {
		return false;
	}

	switch (scope) {
		case 'all_guilds':
		case 'all':
		case 'open_dms_and_all_guilds':
			return true;
		default:
			return false;
	}
};

const collectSearchNsfwChannels = (params: MessageSearchParams, contextChannelId: string): Array<string> => {
	const nsfwChannels: Array<string> = [];

	const contextChannel = ChannelStore.getChannel(contextChannelId);
	if (contextChannel?.isNSFW() && !GuildNSFWAgreeStore.shouldShowGate(contextChannelId)) {
		nsfwChannels.push(contextChannelId);
	}

	if (params.channelId) {
		for (const channelIdParam of params.channelId) {
			const targetChannel = ChannelStore.getChannel(channelIdParam);
			if (targetChannel?.isNSFW() && !GuildNSFWAgreeStore.shouldShowGate(channelIdParam)) {
				nsfwChannels.push(channelIdParam);
			}
		}
	}

	return nsfwChannels;
};

const applyNsfwToParamsIfNeeded = (params: MessageSearchParams, contextChannelId: string): void => {
	const nsfwChannels = collectSearchNsfwChannels(params, contextChannelId);
	if (nsfwChannels.length > 0) {
		params.includeNsfw = true;
	}
};

const getHeaderTitleDescriptor = (machineState: SearchMachineState): MessageDescriptor => {
	switch (machineState.status) {
		case 'success':
			return machineState.total === 1 ? msg`${machineState.total} Result` : msg`${machineState.total} Results`;
		case 'indexing':
			return msg`Indexing...`;
		case 'loading':
			return msg`Searching...`;
		default:
			return msg`Search Results`;
	}
};

export const ChannelSearchResults = observer(
	({channel, searchQuery, searchSegments, refreshKey}: ChannelSearchResultsProps) => {
		const {t, i18n} = useLingui();
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		const pollingTimeout = useRef<number | null>(null);
		const currentChannelId = useRef(channel.id);
		const currentSearchQuery = useRef(searchQuery);
		const currentSearchSegments = useRef(searchSegments);
		const channelId = useRef(channel.id);
		const channelGuildId = useRef(channel.guildId ?? null);
		const channelIsNSFW = useRef(channel.isNSFW());
		const lastSearchAttempt = useRef<{
			query: string;
			segments: Array<SearchSegment>;
			refreshKey: number | null;
		} | null>(null);

		const contextId = getChannelSearchContextId(channel);
		const searchContext = contextId ? ChannelSearchStore.getContext(contextId) : null;
		const machineState = searchContext?.machineState ?? {status: 'loading' as const};
		const scrollPosition = searchContext?.scrollPosition ?? 0;
		const lastKnownScrollPosition = useRef(scrollPosition);
		const successMachineState = machineState.status === 'success' ? machineState : null;
		const indexingMachineState = machineState.status === 'indexing' ? machineState : null;
		const normalizedRefreshKey = refreshKey ?? null;
		const lastObservedRefreshKey = searchContext?.lastSearchRefreshKey ?? null;

		const [visiblePageSlots, setVisiblePageSlots] = useState(() => getAdaptiveVisiblePageCount());
		const [pageJumpValue, setPageJumpValue] = useState('');
		const [activeEllipsis, setActiveEllipsis] = useState<'left' | 'right' | null>(null);
		const ellipsisInputRef = useRef<HTMLInputElement | null>(null);
		const [sortMode, setSortMode] = useState<ChannelSearchSortMode>('newest');
		const sortModeRef = useRef<ChannelSearchSortMode>(sortMode);

		const activeScope = searchContext?.scope ?? DEFAULT_SCOPE_VALUE;
		const scopeOptions = useMemo(
			() => getScopeOptionsForChannel(i18n, channel),
			[i18n, channel.id, channel.type, channel.guildId],
		);
		const scopeOptionValues = useMemo(() => new Set(scopeOptions.map((option) => option.value)), [scopeOptions]);
		const sortModeOptions = useMemo(() => getSortModeOptions(t), [t]);

		useEffect(() => {
			if (!scopeOptions.length || !contextId) {
				return;
			}

			const fallbackScope = scopeOptions[0].value;
			const currentScope: MessageSearchScope = activeScope ?? fallbackScope;

			if (!scopeOptionValues.has(currentScope)) {
				ChannelSearchStore.setScope(contextId, fallbackScope);
			}
		}, [scopeOptions, scopeOptionValues, activeScope, contextId]);

		const applyScopeToParams = useCallback(
			(params: MessageSearchParams) => {
				const parsedScope = params.scope;
				let resolvedScope: MessageSearchScope = activeScope ?? DEFAULT_SCOPE_VALUE;

				if (parsedScope && scopeOptionValues.has(parsedScope)) {
					resolvedScope = parsedScope;
				} else if (!scopeOptionValues.has(resolvedScope)) {
					resolvedScope = DEFAULT_SCOPE_VALUE;
				}

				if (contextId && activeScope !== resolvedScope) {
					ChannelSearchStore.setScope(contextId, resolvedScope);
				}

				params.scope = resolvedScope;
			},
			[scopeOptionValues, activeScope, contextId],
		);

		useEffect(() => {
			sortModeRef.current = sortMode;
		}, [sortMode]);

		const getScrollPosition = useCallback((): number => {
			const node = scrollerRef.current?.getScrollerNode();
			return node ? node.scrollTop : 0;
		}, []);

		const updateScrollPosition = useCallback(
			(position: number) => {
				if (!contextId) {
					return;
				}
				ChannelSearchStore.setScrollPosition(contextId, position);
			},
			[contextId],
		);

		const handleScrollerScroll = useCallback(() => {
			const position = getScrollPosition();
			lastKnownScrollPosition.current = position;
			updateScrollPosition(position);
		}, [getScrollPosition, updateScrollPosition]);

		const resetScrollerToTop = useCallback(() => {
			lastKnownScrollPosition.current = 0;
			updateScrollPosition(0);
			scrollerRef.current?.scrollTo({to: 0, animate: false});
		}, [updateScrollPosition]);

		const setContextMachineState = useCallback(
			(state: SearchMachineState) => {
				if (!contextId) {
					return;
				}
				ChannelSearchStore.setMachineState(
					contextId,
					state,
					currentSearchQuery.current,
					currentSearchSegments.current,
					normalizedRefreshKey,
				);
			},
			[contextId, normalizedRefreshKey],
		);

		const stopPolling = useCallback(() => {
			if (pollingTimeout.current) {
				clearTimeout(pollingTimeout.current);
				pollingTimeout.current = null;
			}
		}, []);

		const buildSearchParams = useCallback(
			(page: number): MessageSearchParams => {
				const params: MessageSearchParams = {
					...parseSearchQueryWithSegments(currentSearchQuery.current, currentSearchSegments.current),
					page,
					hitsPerPage: RESULTS_PER_PAGE,
				};

				applyScopeToParams(params);
				applySortModeToParams(params, sortModeRef.current);
				applyNsfwToParamsIfNeeded(params, channelId.current);

				return params;
			},
			[applyScopeToParams],
		);

		const performSearch = useCallback(
			async (page = 1, sortOverride?: ChannelSearchSortMode) => {
				if (!currentSearchQuery.current.trim() || !contextId) {
					return;
				}

				const attemptSegments = currentSearchSegments.current.map((segment) => ({...segment}));
				lastSearchAttempt.current = {
					query: currentSearchQuery.current,
					segments: attemptSegments,
					refreshKey: normalizedRefreshKey,
				};

				setContextMachineState({status: 'loading'});
				resetScrollerToTop();

				try {
					const params = buildSearchParams(page);

					if (sortOverride) {
						applySortModeToParams(params, sortOverride);
					}

					const result = await searchMessages(
						i18n,
						{contextChannelId: channelId.current, contextGuildId: channelGuildId.current},
						params,
					);

					if (currentChannelId.current !== channelId.current) {
						return;
					}

					if (isIndexing(result)) {
						setContextMachineState({status: 'indexing', pollCount: 0});
						return;
					}

					setContextMachineState({
						status: 'success',
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					});
				} catch (error) {
					if (currentChannelId.current !== channelId.current) {
						return;
					}

					setContextMachineState({
						status: 'error',
						error: (error as Error).message || t(msg({message: 'An error occurred while searching'})),
					});
				}
			},
			[contextId, setContextMachineState, resetScrollerToTop, buildSearchParams, t, i18n, normalizedRefreshKey],
		);

		const poll = useCallback(async () => {
			if (currentChannelId.current !== channelId.current) {
				stopPolling();
				return;
			}

			try {
				const page = successMachineState?.page ?? 1;
				const params = buildSearchParams(page);

				const result = await searchMessages(
					i18n,
					{contextChannelId: channelId.current, contextGuildId: channelGuildId.current},
					params,
				);

				if (currentChannelId.current !== channelId.current) {
					return;
				}

				if (isIndexing(result)) {
					setContextMachineState({
						status: 'indexing',
						pollCount: indexingMachineState ? indexingMachineState.pollCount + 1 : 0,
					});
					return;
				}

				stopPolling();
				setContextMachineState({
					status: 'success',
					results: result.messages,
					total: result.total,
					hitsPerPage: result.hitsPerPage,
					page: result.page,
				});
			} catch (error) {
				if (currentChannelId.current !== channelId.current) {
					return;
				}

				stopPolling();
				setContextMachineState({
					status: 'error',
					error: (error as Error).message || t(msg({message: 'An error occurred while searching'})),
				});
			}
		}, [
			stopPolling,
			setContextMachineState,
			successMachineState?.page,
			indexingMachineState?.pollCount,
			buildSearchParams,
			t,
			i18n,
		]);

		const handleSortSelect = useCallback(
			(mode: ChannelSearchSortMode) => {
				if (sortModeRef.current === mode) {
					return;
				}
				setSortMode(mode);
				performSearch(1, mode);
			},
			[performSearch],
		);

		const handleSortMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuActionCreators.openFromElementBottomRight(event, ({onClose}) => (
					<ContextMenuCloseProvider value={onClose}>
						<MenuGroup>
							{sortModeOptions.map((option) => (
								<MenuItemRadio
									key={option.mode}
									selected={sortMode === option.mode}
									closeOnSelect
									onSelect={() => handleSortSelect(option.mode)}
									icon={renderSortIcon(option.mode)}
								>
									{option.label}
								</MenuItemRadio>
							))}
						</MenuGroup>
					</ContextMenuCloseProvider>
				));
			},
			[handleSortSelect, sortMode, sortModeOptions],
		);

		const handleScopeSelect = useCallback(
			(value: MessageSearchParams['scope']) => {
				if (!contextId || activeScope === value) {
					return;
				}
				ChannelSearchStore.setScope(contextId, value ?? DEFAULT_SCOPE_VALUE);
				performSearch(1);
			},
			[performSearch, contextId, activeScope],
		);

		const handleScopeMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuActionCreators.openFromElementBottomRight(event, ({onClose}) => (
					<ContextMenuCloseProvider value={onClose}>
						<MenuGroup>
							{scopeOptions.map((option) => (
								<MenuItemRadio
									key={option.value}
									selected={activeScope === option.value}
									closeOnSelect
									onSelect={() => handleScopeSelect(option.value)}
									icon={renderScopeIcon(option.value)}
								>
									{option.label}
								</MenuItemRadio>
							))}
						</MenuGroup>
					</ContextMenuCloseProvider>
				));
			},
			[handleScopeSelect, scopeOptions, activeScope],
		);

		const startPolling = useCallback(() => {
			if (machineState.status !== 'indexing') {
				return;
			}

			const pollCount = indexingMachineState?.pollCount ?? 0;
			const pollInterval = Math.min(INITIAL_POLL_INTERVAL * POLL_BACKOFF_MULTIPLIER ** pollCount, MAX_POLL_INTERVAL);

			pollingTimeout.current = window.setTimeout(() => {
				poll();
			}, pollInterval);
		}, [machineState.status, indexingMachineState?.pollCount, poll]);

		const setScrollerRef = useCallback((ref: ScrollerHandle | null) => {
			scrollerRef.current = ref;
		}, []);

		useEffect(() => {
			lastKnownScrollPosition.current = scrollPosition;
		}, [contextId, scrollPosition]);

		const transitionToChannel = useCallback((targetChannel: ChannelRecord) => {
			RouterUtils.transitionTo(getChannelPath(targetChannel));
		}, []);

		const renderContent = useCallback(() => {
			switch (machineState.status) {
				case 'idle':
				case 'loading':
					return null;

				case 'indexing':
					return (
						<div className={styles.loadingState}>
							<CircleNotchIcon className={styles.loadingIcon} />
							<div className={styles.loadingContent}>
								<h3 className={styles.loadingHeading}>{t`Indexing Channel`}</h3>
								<p className={styles.loadingText}>
									{t`We're indexing this channel for the first time. This might take a little while...`}
								</p>
							</div>
						</div>
					);

				case 'error':
					return (
						<div className={styles.errorState}>
							<div className={styles.errorContent}>
								<h3 className={styles.errorHeading}>{t`Error`}</h3>
								<p className={styles.errorText}>{machineState.error}</p>
								<Button variant="secondary" small onClick={() => performSearch(1)}>
									{t`Try Again`}
								</Button>
							</div>
						</div>
					);

				case 'success': {
					const {results, total, hitsPerPage, page: currentPage} = machineState;

					if (results.length === 0) {
						return (
							<div className={styles.emptyState}>
								<div className={styles.emptyStateContent}>
									<MagnifyingGlassIcon className={styles.emptyStateIcon} />
									<div className={styles.emptyStateTextWrapper}>
										<h3 className={styles.emptyStateHeading}>{t`No Results`}</h3>
										<p className={styles.emptyStateText}>{t`Try a different search query`}</p>
									</div>
								</div>
							</div>
						);
					}

					const totalPages = Math.max(1, Math.ceil(total / hitsPerPage));
					const paginationRange = buildPaginationRange(currentPage, totalPages, visiblePageSlots);

					const messagesByChannel = new Map<string, Array<MessageRecord>>();
					for (const message of results) {
						if (!messagesByChannel.has(message.channelId)) {
							messagesByChannel.set(message.channelId, []);
						}
						messagesByChannel.get(message.channelId)!.push(message);
					}

					return (
						<Scroller
							ref={setScrollerRef}
							className={styles.resultsScroller}
							onScroll={handleScrollerScroll}
							fade={false}
							reserveScrollbarTrack={false}
							key="channel-search-results-scroller-desktop"
						>
							{Array.from(messagesByChannel.entries()).map(([resultChannelId, messages]) => {
								const messageChannel = ChannelStore.getChannel(resultChannelId);
								if (!messageChannel) {
									return null;
								}

								const channelGuild = getChannelGuild(messageChannel);
								const _channelDisplayName = getChannelDisplayName(messageChannel);
								const showGuildMeta = shouldShowGuildMetaForScope(
									channelGuild,
									(activeScope ?? DEFAULT_SCOPE_VALUE) as MessageSearchScope,
								);

								return (
									<React.Fragment key={resultChannelId}>
										<MessageContextPrefix
											channel={messageChannel}
											showGuildMeta={showGuildMeta}
											onClick={() => transitionToChannel(messageChannel)}
										/>

										{messages.map((message) => (
											<div key={message.id} className={styles.messageItem}>
												<Message
													message={message}
													channel={messageChannel}
													previewContext={MessagePreviewContext.LIST_POPOUT}
													behaviorOverrides={DETACHED_MESSAGE_BEHAVIOR}
												/>

												<div className={styles.actionButtons}>
													<FocusRing offset={-2} ringClassName={styles.focusRingTight}>
														<button
															type="button"
															className={styles.jumpButton}
															onClick={() => {
																transitionToChannel(messageChannel);
																goToMessage(message.channelId, message.id);
															}}
														>
															{t`Jump`}
														</button>
													</FocusRing>
												</div>
											</div>
										))}
									</React.Fragment>
								);
							})}

							<div className={styles.resultsSpacer} />

							{totalPages > 1 && (
								<div className={styles.paginationBar}>
									<div className={styles.paginationWrapper}>
										{paginationRange.map((page) => {
											if (typeof page === 'number') {
												return (
													<FocusRing key={page} offset={-2} ringClassName={styles.focusRingCircular}>
														<button
															type="button"
															onClick={() => {
																if (page !== currentPage) {
																	resetScrollerToTop();
																	performSearch(page);
																	setPageJumpValue('');
																	setActiveEllipsis(null);
																}
															}}
															className={clsx(styles.pageButton, page === currentPage && styles.pageButtonActive)}
															aria-current={page === currentPage ? 'page' : undefined}
															aria-label={t`Go to page ${page}`}
														>
															{page}
														</button>
													</FocusRing>
												);
											}

											const side = page === 'ellipsis-left' ? 'left' : 'right';
											const isActive = activeEllipsis === side;

											if (isActive) {
												return (
													<form
														key={`ellipsis-input-${side}`}
														className={styles.pageInputForm}
														onSubmit={(e) => {
															e.preventDefault();
															const nextPage = parseInt(pageJumpValue, 10);
															if (!Number.isNaN(nextPage) && nextPage >= 1 && nextPage <= totalPages) {
																if (nextPage !== currentPage) {
																	resetScrollerToTop();
																	performSearch(nextPage);
																}
															}
															setPageJumpValue('');
															setActiveEllipsis(null);
														}}
													>
														<label
															htmlFor={`channel-search-pagination-input-${side}`}
															className={styles.pageInputLabel}
														>
															{t`Go to page`}
														</label>
														<input
															id={`channel-search-pagination-input-${side}`}
															ref={ellipsisInputRef}
															type="number"
															min={1}
															max={totalPages}
															inputMode="numeric"
															value={pageJumpValue}
															onChange={(e) => setPageJumpValue(e.target.value)}
															onBlur={() => {
																setActiveEllipsis(null);
																setPageJumpValue('');
															}}
															className={styles.pageInput}
															placeholder={side === 'left' ? '1' : totalPages.toString()}
														/>
													</form>
												);
											}

											return (
												<FocusRing key={`ellipsis-${side}`} offset={-2} ringClassName={styles.focusRingCircular}>
													<button
														type="button"
														onClick={() => {
															setActiveEllipsis(side);
															setPageJumpValue('');
														}}
														className={styles.ellipsisButton}
														aria-label={t`Jump to page`}
													>
														&hellip;
													</button>
												</FocusRing>
											);
										})}
									</div>
								</div>
							)}
						</Scroller>
					);
				}
			}
		}, [
			machineState,
			t,
			performSearch,
			setScrollerRef,
			handleScrollerScroll,
			visiblePageSlots,
			pageJumpValue,
			activeEllipsis,
			resetScrollerToTop,
			activeScope,
			transitionToChannel,
		]);

		useEffect(() => {
			currentSearchQuery.current = searchQuery;
			currentSearchSegments.current = searchSegments;
		}, [searchQuery, searchSegments]);

		useEffect(() => {
			if (!contextId || !searchContext) {
				return;
			}

			if (!searchQuery.trim()) {
				lastSearchAttempt.current = null;
				return;
			}

			const segmentsMatch = areSegmentsEqual(searchContext.lastSearchSegments, searchSegments);
			const hasCachedResults = searchContext.lastSearchQuery === searchQuery && segmentsMatch;
			const shouldRefresh = normalizedRefreshKey !== lastObservedRefreshKey;

			if (hasCachedResults && !shouldRefresh) {
				if (machineState.status === 'success' && scrollPosition > 0 && scrollerRef.current) {
					scrollerRef.current.scrollTo({to: scrollPosition, animate: false});
				}
				return;
			}

			const hasPendingAttempt =
				lastSearchAttempt.current &&
				lastSearchAttempt.current.query === searchQuery &&
				lastSearchAttempt.current.refreshKey === normalizedRefreshKey &&
				areSegmentsEqual(lastSearchAttempt.current.segments, searchSegments);

			if (hasPendingAttempt && machineState.status !== 'success') {
				return;
			}

			resetScrollerToTop();
			setContextMachineState({status: 'loading'});
			performSearch(1);
		}, [
			contextId,
			searchContext?.lastSearchQuery,
			searchContext?.lastSearchSegments,
			searchContext?.lastSearchRefreshKey,
			searchQuery,
			searchSegments,
			normalizedRefreshKey,
			resetScrollerToTop,
			setContextMachineState,
			performSearch,
			machineState.status,
			scrollPosition,
			lastObservedRefreshKey,
		]);

		useEffect(() => {
			if (currentChannelId.current !== channel.id) {
				stopPolling();
				currentChannelId.current = channel.id;
				channelId.current = channel.id;
				channelGuildId.current = channel.guildId ?? null;
				channelIsNSFW.current = channel.isNSFW();
			}
		}, [channel.id, channel.guildId, channel.isNSFW, stopPolling]);

		useEffect(() => {
			if (machineState.status === 'indexing') {
				startPolling();
			} else {
				stopPolling();
			}
		}, [machineState.status, startPolling, stopPolling]);

		useEffect(() => {
			if (machineState.status === 'success' && scrollPosition > 0 && scrollerRef.current) {
				scrollerRef.current.scrollTo({to: scrollPosition, animate: false});
				lastKnownScrollPosition.current = scrollPosition;
			}
		}, [machineState.status, scrollPosition]);

		useEffect(() => {
			if (!searchContext) {
				return;
			}
			if (activeEllipsis && ellipsisInputRef.current) {
				ellipsisInputRef.current.focus();
				ellipsisInputRef.current.select();
			}
		}, [activeEllipsis, searchContext]);

		useEffect(() => {
			const handleResize = () => {
				setVisiblePageSlots(getAdaptiveVisiblePageCount());
			};

			window.addEventListener('resize', handleResize);

			return () => {
				window.removeEventListener('resize', handleResize);
			};
		}, []);

		const successPage = machineState.status === 'success' ? machineState.page : null;
		useEffect(() => {
			setPageJumpValue('');
			setActiveEllipsis(null);
		}, [successPage]);

		useEffect(() => {
			return () => {
				stopPolling();
				updateScrollPosition(lastKnownScrollPosition.current);
			};
		}, [stopPolling, updateScrollPosition]);

		useEffect(() => {
			if (machineState.status !== 'success' || !searchQuery.trim()) {
				clearChannelSearchHighlight();
				return;
			}

			const scrollerNode = scrollerRef.current?.getScrollerNode();
			if (!scrollerNode) {
				return;
			}

			const timer = setTimeout(() => {
				const terms = tokenizeSearchQuery(searchQuery);
				if (terms.length > 0) {
					applyChannelSearchHighlight(scrollerNode, terms);
				}
			}, 50);

			return () => {
				clearTimeout(timer);
				clearChannelSearchHighlight();
			};
		}, [machineState.status, searchQuery, successMachineState?.results]);

		const renderHeader = useCallback(() => {
			const showSpinner = machineState.status === 'loading' || machineState.status === 'indexing';
			const headerTitleDescriptor = getHeaderTitleDescriptor(machineState);

			const activeScopeOption = scopeOptions.find((option) => option.value === activeScope) ?? scopeOptions[0];
			const activeSortOption = sortModeOptions.find((option) => option.mode === sortMode) ?? sortModeOptions[0];

			return (
				<div className={styles.header}>
					<div className={styles.headerLoading}>
						{showSpinner && <Spinner size="small" />}
						<h2 className={styles.headerTitle}>{t(headerTitleDescriptor)}</h2>
					</div>
					{!showSpinner && (
						<div className={styles.headerActions}>
							<Button
								type="button"
								variant="secondary"
								square
								compact
								fitContent
								className={styles.scopeButton}
								icon={renderScopeIcon(activeScope ?? DEFAULT_SCOPE_VALUE, 18)}
								onClick={handleScopeMenuOpen}
								aria-label={t`Search scope: ${activeScopeOption.label}`}
							/>
							<Button
								type="button"
								variant="secondary"
								square
								compact
								fitContent
								className={styles.sortButton}
								icon={renderSortIcon(activeSortOption.mode, 18)}
								onClick={handleSortMenuOpen}
								aria-label={t`Sort mode: ${activeSortOption.label}`}
							/>
						</div>
					)}
				</div>
			);
		}, [
			machineState,
			handleSortMenuOpen,
			handleScopeMenuOpen,
			scopeOptions,
			activeScope,
			sortMode,
			sortModeOptions,
			t,
		]);

		return (
			<div className={styles.container}>
				{renderHeader()}
				{renderContent()}
			</div>
		);
	},
);
