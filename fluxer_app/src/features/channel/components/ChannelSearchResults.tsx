// SPDX-License-Identifier: AGPL-3.0-or-later

import '@app/features/channel/components/ChannelSearchHighlight.css';
import * as PrivateChannelCommands from '@app/features/channel/commands/PrivateChannelCommands';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import styles from '@app/features/channel/components/ChannelSearchResults.module.css';
import {CollapsedMessageVisibilityProvider} from '@app/features/channel/components/CollapsedMessageVisibilityContext';
import {
	AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR,
	applyMatureContentToParamsIfNeeded,
	applySortModeToParams,
	type ChannelSearchSortMode,
	DETACHED_MESSAGE_BEHAVIOR,
	EMPTY_SEARCH_CHANNELS,
	EMPTY_SEARCH_MESSAGES,
	getAdaptiveVisiblePageCount,
	getSearchResultChannelRenderData,
	getSortModeOptions,
	RESULTS_PER_PAGE,
	renderScopeIcon,
	renderSortIcon,
} from '@app/features/channel/components/channel_search_results/ChannelSearchResultsShared';
import {shouldSearchResultRowJump} from '@app/features/channel/components/channel_search_results/SearchResultRowClick';
import {SearchResultsHeader} from '@app/features/channel/components/channel_search_results/SearchResultsHeader';
import {SearchResultsPagination} from '@app/features/channel/components/channel_search_results/SearchResultsPagination';
import {
	SearchEmptyState,
	SearchErrorState,
	SearchIndexingState,
	SearchUnappliedQueryState,
} from '@app/features/channel/components/channel_search_results/SearchResultsStateViews';
import {useChannelSearchHighlight} from '@app/features/channel/components/channel_search_results/useChannelSearchHighlight';
import type {MessageGroupRenderWrapperProps} from '@app/features/channel/components/MessageGroup';
import {
	buildSearchResultGroups,
	buildSearchResultGroupsByMessageId,
} from '@app/features/channel/components/SearchResultGrouping';
import {SearchResultMessageList} from '@app/features/channel/components/SearchResultMessageList';
import {areSegmentsEqual} from '@app/features/channel/components/SearchResultsUtils';
import {DEFAULT_SCOPE_VALUE, getScopeOptionsForChannel} from '@app/features/channel/components/SearchScopeOptions';
import {resolveSearchScope} from '@app/features/channel/components/SearchScopeResolution';
import type {SearchMachineEvent} from '@app/features/channel/components/SearchStateMachine';
import {Channel} from '@app/features/channel/models/Channel';
import ChannelSearch, {getChannelSearchContextId} from '@app/features/channel/state/ChannelSearch';
import Channels from '@app/features/channel/state/Channels';
import {getChannelSearchIndexingPollInterval} from '@app/features/channel/utils/ChannelSearchPolling';
import GuildMatureContentAgree from '@app/features/guild/state/GuildMatureContentAgree';
import {JUMP_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {MessageContextPrefix} from '@app/features/messaging/components/message_context_prefix/MessageContextPrefix';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {getCollapsedMessageGroupKey} from '@app/features/messaging/utils/MessageGroupingUtils';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import {SearchResultOpenFailedModal} from '@app/features/search/components/alerts/SearchResultOpenFailedModal';
import type {SearchSegment} from '@app/features/search/utils/SearchSegmentManager';
import {
	isIndexing,
	type MessageSearchParams,
	type MessageSearchScope,
	parseSearchQueryWithSegments,
	searchMessages,
} from '@app/features/search/utils/SearchUtils';
import {ContextMenuCloseProvider} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItemRadio} from '@app/features/ui/action_menu/MenuItemRadio';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import * as FormUtils from '@app/lib/forms';
import {ME} from '@fluxer/constants/src/AppConstants';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import type {MessageId} from '@fluxer/schema/src/branded/WireIds';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useMemo, useRef, useState} from 'react';

interface ChannelSearchResultsProps {
	channel: Channel;
	searchQuery: string;
	searchSegments: Array<SearchSegment>;
	onClose: () => void;
	refreshKey?: number;
}

export const ChannelSearchResults = observer(
	({channel, searchQuery, searchSegments, onClose, refreshKey}: ChannelSearchResultsProps) => {
		const {i18n} = useLingui();
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		const resolveSearchResultsScrollSurface = useMemo(
			() => () => scrollerRef.current?.getViewportElement() ?? null,
			[],
		);
		const pollingTimeout = useRef<number | null>(null);
		const currentChannelId = useRef(channel.id);
		const currentSearchQuery = useRef(searchQuery);
		const currentSearchSegments = useRef(searchSegments);
		const channelId = useRef(channel.id);
		const channelGuildId = useRef(channel.guildId ?? null);
		const channelIsMatureContent = useRef(channel.isMature());
		const lastSearchAttempt = useRef<{
			query: string;
			segments: Array<SearchSegment>;
			refreshKey: number | null;
			scope: MessageSearchScope;
			sortMode: ChannelSearchSortMode;
		} | null>(null);
		const contextId = getChannelSearchContextId(channel);
		const searchContext = contextId ? ChannelSearch.getContext(contextId) : null;
		const machineState = searchContext?.machineState ?? {status: 'loading' as const};
		const scrollPosition = searchContext?.scrollPosition ?? 0;
		const lastKnownScrollPosition = useRef(scrollPosition);
		const unsearchableQuery = searchContext?.unsearchableQuery ?? '';
		const successMachineState = machineState.status === 'success' ? machineState : null;
		const indexingMachineState = machineState.status === 'indexing' ? machineState : null;
		const normalizedRefreshKey = refreshKey ?? null;
		const lastObservedRefreshKey = searchContext?.lastSearchRefreshKey ?? null;
		const [visiblePageSlots, setVisiblePageSlots] = useState(() => getAdaptiveVisiblePageCount());
		const [revealedGroupKeys, setRevealedGroupKeys] = useState<Set<string>>(new Set());
		const [sortMode, setSortMode] = useState<ChannelSearchSortMode>(
			() => searchContext?.lastSearchSortMode ?? 'newest',
		);
		const sortModeRef = useRef<ChannelSearchSortMode>(sortMode);
		useMessageListKeyboardNavigation({containerRef: scrollerRef, allowWhenInactive: true});
		const activeScope = searchContext?.scope ?? DEFAULT_SCOPE_VALUE;
		const scopeOptions = useMemo(
			() => getScopeOptionsForChannel(i18n, channel),
			[i18n.locale, channel.id, channel.type, channel.guildId],
		);
		const scopeOptionValues = useMemo(() => new Set(scopeOptions.map((option) => option.value)), [scopeOptions]);
		const sortModeOptions = useMemo(() => getSortModeOptions(i18n), [i18n.locale]);
		const successResults = successMachineState?.results ?? EMPTY_SEARCH_MESSAGES;
		const successChannels = successMachineState?.channels ?? EMPTY_SEARCH_CHANNELS;
		const searchChannelsById = useMemo(
			() => new Map(successChannels.map((searchChannel) => [searchChannel.id, searchChannel])),
			[successChannels],
		);
		const resultGroups = useMemo(() => buildSearchResultGroups(successResults), [successResults]);
		const resultGroupsByMessageId = useMemo(() => buildSearchResultGroupsByMessageId(resultGroups), [resultGroups]);
		const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(successResults);
		const spammerOverrideVersion = LocalUserSpamOverride.version;
		const collapsedMessageVisibility = useMemo(
			() => ({
				isMessageRevealed: (message: Message) => {
					const resultGroup = resultGroupsByMessageId.get(message.id);
					if (!resultGroup) return false;
					const messageChannel = searchChannelsById.get(message.channelId) ?? Channels.getChannel(message.channelId);
					if (!messageChannel) return false;
					const groupKey = getCollapsedMessageGroupKey({
						channel: messageChannel,
						messages: resultGroup.messages,
						messageId: message.id,
						treatSpam: true,
					});
					return groupKey != null && revealedGroupKeys.has(groupKey);
				},
			}),
			[resultGroupsByMessageId, revealedGroupKeys, searchChannelsById, spammerOverrideVersion],
		);
		const handleCollapsedGroupRevealChange = useCallback((groupKey: string, revealed: boolean) => {
			setRevealedGroupKeys((current) => {
				const next = new Set(current);
				if (revealed) {
					next.add(groupKey);
				} else {
					next.delete(groupKey);
				}
				return next;
			});
		}, []);
		useEffect(() => {
			if (!scopeOptions.length || !contextId) return;
			const fallbackScope = scopeOptions[0].value;
			const currentScope: MessageSearchScope = activeScope ?? fallbackScope;
			if (!scopeOptionValues.has(currentScope)) {
				ChannelSearch.setScope(contextId, fallbackScope);
			}
		}, [scopeOptions, scopeOptionValues, activeScope, contextId]);
		const applyScopeToParams = useCallback(
			(params: MessageSearchParams, scopeOverride?: MessageSearchScope | null) => {
				const parsedScope = params['scope'];
				const resolvedScope = resolveSearchScope({
					activeScope,
					fallbackScope: DEFAULT_SCOPE_VALUE,
					parsedScope,
					scopeOverride,
					scopeOptionValues,
				});
				if (contextId && activeScope !== resolvedScope) {
					ChannelSearch.setScope(contextId, resolvedScope);
				}
				params['scope'] = resolvedScope;
			},
			[scopeOptionValues, activeScope, contextId],
		);
		useEffect(() => {
			sortModeRef.current = sortMode;
		}, [sortMode]);
		useEffect(() => {
			const cachedSortMode = searchContext?.lastSearchSortMode;
			if (!cachedSortMode || sortModeRef.current === cachedSortMode) return;
			sortModeRef.current = cachedSortMode;
			setSortMode(cachedSortMode);
		}, [contextId, searchContext?.lastSearchSortMode]);
		const getScrollPosition = useCallback((): number => {
			const node = scrollerRef.current?.getViewportElement();
			return node ? node.scrollTop : 0;
		}, []);
		const updateScrollPosition = useCallback(
			(position: number) => {
				if (!contextId) return;
				ChannelSearch.setScrollPosition(contextId, position);
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
		const sendContextMachineEvent = useCallback(
			(
				event: SearchMachineEvent,
				searchScope?: MessageSearchScope | null,
				searchSortMode?: ChannelSearchSortMode | null,
			) => {
				if (!contextId) return;
				ChannelSearch.sendMachineEvent(
					contextId,
					event,
					currentSearchQuery.current,
					currentSearchSegments.current,
					normalizedRefreshKey,
					{scope: searchScope ?? null, sortMode: searchSortMode ?? null},
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
			(page: number, scopeOverride?: MessageSearchScope | null): MessageSearchParams => {
				const params: MessageSearchParams = {
					...parseSearchQueryWithSegments(currentSearchQuery.current, currentSearchSegments.current, {
						historyKey: contextId ?? undefined,
						guildId: channelGuildId.current,
					}),
					page,
					hitsPerPage: RESULTS_PER_PAGE,
				};
				applyScopeToParams(params, scopeOverride);
				applySortModeToParams(params, sortModeRef.current);
				applyMatureContentToParamsIfNeeded(params, channelId.current);
				return params;
			},
			[applyScopeToParams, contextId],
		);
		const performSearch = useCallback(
			async (page = 1, sortOverride?: ChannelSearchSortMode, scopeOverride?: MessageSearchScope | null) => {
				if (!currentSearchQuery.current.trim() || !contextId) return;
				if (GuildMatureContentAgree.shouldShowGate({channelId: channelId.current, guildId: channelGuildId.current})) {
					sendContextMachineEvent({
						type: 'channelSearch.succeeded',
						channels: [],
						results: [],
						total: 0,
						hitsPerPage: RESULTS_PER_PAGE,
						page: 1,
					});
					return;
				}
				const params = buildSearchParams(page, scopeOverride);
				const searchScope = params.scope ?? DEFAULT_SCOPE_VALUE;
				const searchSortMode = sortOverride ?? sortModeRef.current;
				if (sortOverride) {
					applySortModeToParams(params, sortOverride);
				}
				const attemptSegments = currentSearchSegments.current.map((segment) => ({...segment}));
				lastSearchAttempt.current = {
					query: currentSearchQuery.current,
					segments: attemptSegments,
					refreshKey: normalizedRefreshKey,
					scope: searchScope,
					sortMode: searchSortMode,
				};
				sendContextMachineEvent({type: 'channelSearch.loading'});
				resetScrollerToTop();
				try {
					const result = await searchMessages(
						i18n,
						{contextChannelId: channelId.current, contextGuildId: channelGuildId.current},
						params,
					);
					if (currentChannelId.current !== channelId.current) return;
					if (isIndexing(result)) {
						sendContextMachineEvent({type: 'channelSearch.indexingStarted'});
						return;
					}
					sendContextMachineEvent(
						{
							type: 'channelSearch.succeeded',
							channels: result.channels,
							results: result.messages,
							total: result.total,
							hitsPerPage: result.hitsPerPage,
							page: result.page,
						},
						searchScope,
						searchSortMode,
					);
				} catch (error) {
					if (currentChannelId.current !== channelId.current) return;
					sendContextMachineEvent({
						type: 'channelSearch.failed',
						error: FormUtils.extractErrorMessage(i18n, error) || i18n._(AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR),
					});
				}
			},
			[contextId, sendContextMachineEvent, resetScrollerToTop, buildSearchParams, i18n, normalizedRefreshKey],
		);
		const poll = useCallback(async () => {
			if (currentChannelId.current !== channelId.current) {
				stopPolling();
				return;
			}
			if (GuildMatureContentAgree.shouldShowGate({channelId: channelId.current, guildId: channelGuildId.current})) {
				stopPolling();
				sendContextMachineEvent({
					type: 'channelSearch.succeeded',
					channels: [],
					results: [],
					total: 0,
					hitsPerPage: RESULTS_PER_PAGE,
					page: 1,
				});
				return;
			}
			try {
				const page = successMachineState?.page ?? 1;
				const params = buildSearchParams(page);
				const searchScope = params.scope ?? DEFAULT_SCOPE_VALUE;
				const searchSortMode = sortModeRef.current;
				const result = await searchMessages(
					i18n,
					{contextChannelId: channelId.current, contextGuildId: channelGuildId.current},
					params,
				);
				if (currentChannelId.current !== channelId.current) return;
				if (isIndexing(result)) {
					sendContextMachineEvent({type: 'channelSearch.indexingPolled'});
					return;
				}
				stopPolling();
				sendContextMachineEvent(
					{
						type: 'channelSearch.succeeded',
						channels: result.channels,
						results: result.messages,
						total: result.total,
						hitsPerPage: result.hitsPerPage,
						page: result.page,
					},
					searchScope,
					searchSortMode,
				);
			} catch (error) {
				if (currentChannelId.current !== channelId.current) return;
				stopPolling();
				sendContextMachineEvent({
					type: 'channelSearch.failed',
					error: FormUtils.extractErrorMessage(i18n, error) || i18n._(AN_ERROR_OCCURRED_WHILE_SEARCHING_DESCRIPTOR),
				});
			}
		}, [stopPolling, sendContextMachineEvent, successMachineState?.page, buildSearchParams, i18n]);
		const handleSortSelect = useCallback(
			(mode: ChannelSearchSortMode) => {
				if (sortModeRef.current === mode) return;
				setSortMode(mode);
				performSearch(1, mode);
			},
			[performSearch],
		);
		const handleSortMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuCommands.openFromElementBottomRight(event, ({onClose: menuClose}) => (
					<ContextMenuCloseProvider
						value={menuClose}
						data-flx="channel.channel-search-results.handle-sort-menu-open.context-menu-close-provider"
					>
						<MenuGroup data-flx="channel.channel-search-results.handle-sort-menu-open.menu-group">
							{sortModeOptions.map((option) => (
								<MenuItemRadio
									key={option.mode}
									selected={sortMode === option.mode}
									closeOnSelect
									onSelect={() => handleSortSelect(option.mode)}
									icon={renderSortIcon(option.mode)}
									data-flx="channel.channel-search-results.handle-sort-menu-open.menu-item-radio.sort-select"
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
				if (!contextId || activeScope === value) return;
				const nextScope = value ?? DEFAULT_SCOPE_VALUE;
				ChannelSearch.setScope(contextId, nextScope);
				performSearch(1, undefined, nextScope);
			},
			[performSearch, contextId, activeScope],
		);
		const handleScopeMenuOpen = useCallback(
			(event: React.MouseEvent<HTMLButtonElement>) => {
				ContextMenuCommands.openFromElementBottomRight(event, ({onClose: menuClose}) => (
					<ContextMenuCloseProvider
						value={menuClose}
						data-flx="channel.channel-search-results.handle-scope-menu-open.context-menu-close-provider"
					>
						<MenuGroup data-flx="channel.channel-search-results.handle-scope-menu-open.menu-group">
							{scopeOptions.map((option) => (
								<MenuItemRadio
									key={option.value}
									selected={activeScope === option.value}
									closeOnSelect
									onSelect={() => handleScopeSelect(option.value)}
									icon={renderScopeIcon(option.value)}
									data-flx="channel.channel-search-results.handle-scope-menu-open.menu-item-radio.scope-select"
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
			if (machineState.status !== 'indexing') return;
			const pollCount = indexingMachineState?.pollCount ?? 0;
			const pollInterval = getChannelSearchIndexingPollInterval(pollCount);
			stopPolling();
			pollingTimeout.current = window.setTimeout(() => {
				pollingTimeout.current = null;
				void poll();
			}, pollInterval);
		}, [machineState.status, indexingMachineState?.pollCount, poll, stopPolling]);
		const setScrollerRef = useCallback((ref: ScrollerHandle | null) => {
			scrollerRef.current = ref;
		}, []);
		useEffect(() => {
			lastKnownScrollPosition.current = scrollPosition;
		}, [contextId, scrollPosition]);
		const transitionToChannel = useCallback((targetChannel: Channel) => {
			if (targetChannel.guildId) {
				NavigationCommands.selectChannel(targetChannel.guildId, targetChannel.id);
			} else {
				NavigationCommands.selectChannel(undefined, targetChannel.id);
			}
		}, []);
		const showSearchResultOpenFailedModal = useCallback(() => {
			ModalCommands.push(
				modal(() => (
					<SearchResultOpenFailedModal data-flx="channel.channel-search-results.show-search-result-open-failed-modal.search-result-open-failed-modal" />
				)),
			);
		}, []);
		const ensureSearchChannelReady = useCallback(
			async (targetChannel: Channel): Promise<{channel: Channel; insertedTemporarily: boolean}> => {
				const existingChannel = Channels.getChannel(targetChannel.id);
				if (existingChannel) {
					return {channel: existingChannel, insertedTemporarily: false};
				}
				if (targetChannel.isDM()) {
					const recipientId = targetChannel.getRecipientId();
					if (!recipientId) {
						throw new Error(`Missing DM recipient for search result channel ${targetChannel.id}`);
					}
					const reopenedChannel = await PrivateChannelCommands.ensureDMChannelResponse(recipientId);
					return {
						channel: Channels.getChannel(reopenedChannel.id) ?? new Channel(reopenedChannel),
						insertedTemporarily: false,
					};
				}
				Channels.handleChannelCreate({channel: targetChannel.toJSON()});
				return {channel: Channels.getChannel(targetChannel.id) ?? targetChannel, insertedTemporarily: true};
			},
			[],
		);
		const navigateToSearchMessage = useCallback((targetChannel: Channel, messageId: string) => {
			const guildId = targetChannel.guildId && targetChannel.guildId !== ME ? targetChannel.guildId : ME;
			NavigationCommands.navigateToMessage(guildId, targetChannel.id, messageId);
		}, []);
		const handleSearchChannelOpen = useCallback(
			async (targetChannel: Channel) => {
				let ensuredChannel: Channel | null = null;
				let insertedTemporarily = false;
				try {
					const ensuredResult = await ensureSearchChannelReady(targetChannel);
					ensuredChannel = ensuredResult.channel;
					insertedTemporarily = ensuredResult.insertedTemporarily;
					await MessageCommands.fetchMessages(ensuredChannel.id, null, null, MAX_MESSAGES_PER_CHANNEL, undefined, {
						throwOnError: true,
					});
					transitionToChannel(ensuredChannel);
					onClose();
					focusChannelTextareaAfterNavigation(ensuredChannel.id);
				} catch {
					if (insertedTemporarily && ensuredChannel) {
						Channels.handleChannelDelete({channel: ensuredChannel.toJSON()});
					}
					showSearchResultOpenFailedModal();
				}
			},
			[ensureSearchChannelReady, onClose, showSearchResultOpenFailedModal, transitionToChannel],
		);
		const handleSearchMessageJump = useCallback(
			async (targetChannel: Channel, message: Message) => {
				let ensuredChannel: Channel | null = null;
				let insertedTemporarily = false;
				try {
					const ensuredResult = await ensureSearchChannelReady(targetChannel);
					ensuredChannel = ensuredResult.channel;
					insertedTemporarily = ensuredResult.insertedTemporarily;
					await MessageCommands.fetchMessages(
						ensuredChannel.id,
						null,
						null,
						MAX_MESSAGES_PER_CHANNEL,
						{messageId: message.id as MessageId, flash: true},
						{throwOnError: true},
					);
					navigateToSearchMessage(ensuredChannel, message.id);
					focusChannelTextareaAfterNavigation(ensuredChannel.id);
				} catch {
					if (insertedTemporarily && ensuredChannel) {
						Channels.handleChannelDelete({channel: ensuredChannel.toJSON()});
					}
					showSearchResultOpenFailedModal();
				}
			},
			[ensureSearchChannelReady, navigateToSearchMessage, showSearchResultOpenFailedModal],
		);
		const handleResultRowClick = useCallback(
			(event: React.MouseEvent<HTMLDivElement>, targetChannel: Channel, message: Message) => {
				if (!shouldSearchResultRowJump(event.target as Node, event.currentTarget)) {
					event.stopPropagation();
					return;
				}
				void handleSearchMessageJump(targetChannel, message);
			},
			[handleSearchMessageJump],
		);
		const handlePaginationJump = useCallback(
			(page: number) => {
				resetScrollerToTop();
				performSearch(page);
			},
			[resetScrollerToTop, performSearch],
		);
		const renderContent = useCallback(() => {
			if (unsearchableQuery !== '') {
				return (
					<SearchUnappliedQueryState
						query={unsearchableQuery}
						data-flx="channel.channel-search-results.render-content.search-unapplied-query-state"
					/>
				);
			}
			switch (machineState.status) {
				case 'idle':
				case 'loading':
					return null;
				case 'indexing':
					return <SearchIndexingState data-flx="channel.channel-search-results.render-content.search-indexing-state" />;
				case 'error':
					return (
						<SearchErrorState
							error={machineState.error}
							onRetry={() => performSearch(1)}
							data-flx="channel.channel-search-results.render-content.search-error-state"
						/>
					);
				case 'success': {
					const {results, total, hitsPerPage, page: currentPage} = machineState;
					if (results.length === 0) {
						return <SearchEmptyState data-flx="channel.channel-search-results.render-content.search-empty-state" />;
					}
					const totalPages = Math.max(1, Math.ceil(total / hitsPerPage));
					return (
						<CollapsedMessageVisibilityProvider
							value={collapsedMessageVisibility}
							data-flx="channel.channel-search-results.render-content.collapsed-message-visibility-provider"
						>
							<NearViewportSurfaceContext.Provider value={resolveSearchResultsScrollSurface}>
								<Scroller
									ref={setScrollerRef}
									className={styles.resultsScroller}
									onScroll={handleScrollerScroll}
									fade={false}
									key="channel-search-results-scroller-desktop"
									onCopy={onCopySelectedMessages}
									data-message-selection-root="true"
									data-flx="channel.channel-search-results.render-content.results-scroller"
								>
									{resultGroups.map((resultGroup) => {
										const renderData = getSearchResultChannelRenderData(
											resultGroup.channelId,
											searchChannelsById,
											(activeScope ?? DEFAULT_SCOPE_VALUE) as MessageSearchScope,
										);
										if (!renderData) return null;
										const {messageChannel, showGuildMeta} = renderData;
										const renderMessageActions = (message: Message) => (
											<FocusRing
												offset={-2}
												ringClassName={styles.focusRingTight}
												data-flx="channel.channel-search-results.render-message-actions.focus-ring"
											>
												<button
													type="button"
													className={styles.jumpButton}
													onClick={() => {
														void handleSearchMessageJump(messageChannel, message);
													}}
													data-flx="channel.channel-search-results.render-message-actions.jump-button"
												>
													{i18n._(JUMP_DESCRIPTOR)}
												</button>
											</FocusRing>
										);
										const renderMessageWrapper = ({
											message,
											index,
											isGroupStart,
											children,
										}: MessageGroupRenderWrapperProps) => (
											// biome-ignore lint/a11y/noStaticElementInteractions: mouse convenience; the row's Jump button is the keyboard affordance.
											// biome-ignore lint/a11y/useKeyWithClickEvents: mouse convenience; the row's Jump button is the keyboard affordance.
											<div
												data-message-index={index}
												data-message-id={message.id}
												data-is-group-start={isGroupStart}
												className={styles.messageItem}
												onClick={(event) => {
													handleResultRowClick(event, messageChannel, message);
												}}
												data-flx="channel.channel-search-results.render-message-wrapper.message-item"
											>
												{children}
											</div>
										);
										return (
											<React.Fragment key={resultGroup.key}>
												<MessageContextPrefix
													channel={messageChannel}
													showGuildMeta={showGuildMeta}
													onClick={() => {
														void handleSearchChannelOpen(messageChannel);
													}}
													data-flx="channel.channel-search-results.render-content.message-context-prefix"
												/>
												<SearchResultMessageList
													channel={messageChannel}
													messages={resultGroup.messages}
													revealedGroupKeys={revealedGroupKeys}
													onGroupRevealChange={handleCollapsedGroupRevealChange}
													collapsedGroupClassName={styles.collapsedMessageGroup}
													messagePreviewContext={MessagePreviewContext.LIST_POPOUT}
													messageBehaviorOverrides={DETACHED_MESSAGE_BEHAVIOR}
													messageActionsClassName={styles.actionButtons}
													renderMessageActions={renderMessageActions}
													renderMessageWrapper={renderMessageWrapper}
													spammerOverrideVersion={spammerOverrideVersion}
													renderMessage={(message) => (
														// biome-ignore lint/a11y/noStaticElementInteractions: mouse convenience; the row's Jump button is the keyboard affordance.
														// biome-ignore lint/a11y/useKeyWithClickEvents: mouse convenience; the row's Jump button is the keyboard affordance.
														<div
															className={styles.messageItem}
															data-message-id={message.id}
															data-is-group-start="true"
															onClick={(event) => {
																handleResultRowClick(event, messageChannel, message);
															}}
															data-flx="channel.channel-search-results.render-content.message-item"
														>
															<MessageComponent
																message={message}
																channel={messageChannel}
																previewContext={MessagePreviewContext.LIST_POPOUT}
																behaviorOverrides={DETACHED_MESSAGE_BEHAVIOR}
																data-flx="channel.channel-search-results.render-content.message-component"
															/>
															<div
																className={styles.actionButtons}
																data-flx="channel.channel-search-results.render-content.action-buttons"
															>
																{renderMessageActions(message)}
															</div>
														</div>
													)}
													data-flx="channel.channel-search-results.render-content.search-result-message-list"
												/>
											</React.Fragment>
										);
									})}
									<div
										className={styles.resultsSpacer}
										data-flx="channel.channel-search-results.render-content.results-spacer"
									/>
									<SearchResultsPagination
										currentPage={currentPage}
										totalPages={totalPages}
										visiblePageSlots={visiblePageSlots}
										onJumpToPage={handlePaginationJump}
										data-flx="channel.channel-search-results.render-content.search-results-pagination"
									/>
								</Scroller>
							</NearViewportSurfaceContext.Provider>
						</CollapsedMessageVisibilityProvider>
					);
				}
			}
		}, [
			unsearchableQuery,
			machineState,
			performSearch,
			setScrollerRef,
			resolveSearchResultsScrollSurface,
			handleScrollerScroll,
			visiblePageSlots,
			activeScope,
			handleSearchChannelOpen,
			handleSearchMessageJump,
			handleResultRowClick,
			handlePaginationJump,
			onCopySelectedMessages,
			collapsedMessageVisibility,
			resultGroups,
			searchChannelsById,
			revealedGroupKeys,
			handleCollapsedGroupRevealChange,
			spammerOverrideVersion,
			i18n,
		]);
		useEffect(() => {
			currentSearchQuery.current = searchQuery;
			currentSearchSegments.current = searchSegments;
		}, [searchQuery, searchSegments]);
		useEffect(() => {
			if (!contextId || !searchContext) return;
			if (!searchQuery.trim()) {
				lastSearchAttempt.current = null;
				return;
			}
			const segmentsMatch = areSegmentsEqual(searchContext.lastSearchSegments, searchSegments);
			const hasCachedResults =
				searchContext.lastSearchQuery === searchQuery &&
				segmentsMatch &&
				searchContext.lastSearchScope === activeScope &&
				searchContext.lastSearchSortMode === sortMode;
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
				lastSearchAttempt.current.scope === activeScope &&
				lastSearchAttempt.current.sortMode === sortMode &&
				areSegmentsEqual(lastSearchAttempt.current.segments, searchSegments);
			if (hasPendingAttempt && machineState.status !== 'success') return;
			resetScrollerToTop();
			sendContextMachineEvent({type: 'channelSearch.loading'});
			performSearch(1);
		}, [
			contextId,
			searchContext?.lastSearchQuery,
			searchContext?.lastSearchSegments,
			searchContext?.lastSearchRefreshKey,
			searchContext?.lastSearchScope,
			searchContext?.lastSearchSortMode,
			searchQuery,
			searchSegments,
			activeScope,
			sortMode,
			normalizedRefreshKey,
			resetScrollerToTop,
			sendContextMachineEvent,
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
				channelIsMatureContent.current = channel.isMature();
			}
		}, [channel.id, channel.guildId, channel.isMature, stopPolling]);
		useEffect(() => {
			if (machineState.status !== 'indexing') {
				stopPolling();
				return;
			}
			startPolling();
			return stopPolling;
		}, [machineState.status, startPolling, stopPolling]);
		useEffect(() => {
			if (machineState.status === 'success' && scrollPosition > 0 && scrollerRef.current) {
				scrollerRef.current.scrollTo({to: scrollPosition, animate: false});
				lastKnownScrollPosition.current = scrollPosition;
			}
		}, [machineState.status, scrollPosition]);
		useEffect(() => {
			const handleResize = () => {
				setVisiblePageSlots(getAdaptiveVisiblePageCount());
			};
			window.addEventListener('resize', handleResize);
			return () => {
				window.removeEventListener('resize', handleResize);
			};
		}, []);
		useEffect(() => {
			setRevealedGroupKeys(new Set());
		}, [successMachineState?.results]);
		useEffect(() => {
			return () => {
				stopPolling();
				updateScrollPosition(lastKnownScrollPosition.current);
			};
		}, [stopPolling, updateScrollPosition]);
		useChannelSearchHighlight({
			isSuccess: machineState.status === 'success',
			searchQuery,
			resultsRevision: successMachineState?.results,
			getHighlightRoot: () => scrollerRef.current?.getViewportElement() ?? null,
		});
		return (
			<div className={styles.container} data-flx="channel.channel-search-results.container">
				<SearchResultsHeader
					machineState={machineState}
					scopeOptions={scopeOptions}
					activeScope={activeScope}
					sortModeOptions={sortModeOptions}
					sortMode={sortMode}
					onScopeMenuOpen={handleScopeMenuOpen}
					onSortMenuOpen={handleSortMenuOpen}
					data-flx="channel.channel-search-results.search-results-header"
				/>
				{renderContent()}
			</div>
		);
	},
);
