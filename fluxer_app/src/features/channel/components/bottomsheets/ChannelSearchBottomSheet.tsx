// SPDX-License-Identifier: AGPL-3.0-or-later

import {msg} from '@lingui/core/macro';
import '@app/features/channel/components/ChannelSearchHighlight.css';
import sharedStyles from '@app/features/app/components/bottomsheets/shared.module.css';
import {LongPressable} from '@app/features/app/components/LongPressable';
import {Message as MessageComponent} from '@app/features/channel/components/ChannelMessage';
import {CollapsedMessageVisibilityProvider} from '@app/features/channel/components/CollapsedMessageVisibilityContext';
import {MessageActionBottomSheet} from '@app/features/channel/components/MessageActionBottomSheet';
import type {MessageGroupRenderWrapperProps} from '@app/features/channel/components/MessageGroup';
import {SearchResultMessageList} from '@app/features/channel/components/SearchResultMessageList';
import {type ChannelSearchFilters, useChannelSearch} from '@app/features/channel/hooks/useChannelSearch';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import * as ChannelUtils from '@app/features/channel/utils/ChannelUtils';
import {
	CLEAR_SEARCH_DESCRIPTOR,
	NEXT_DESCRIPTOR,
	TRY_AGAIN_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import type {Message} from '@app/features/messaging/models/MessagingMessage';
import {
	applyChannelSearchHighlight,
	clearChannelSearchHighlight,
} from '@app/features/messaging/utils/ChannelSearchHighlight';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import {isIMEComposing} from '@app/features/messaging/utils/IMECompositionUtils';
import {getCollapsedMessageGroupKey} from '@app/features/messaging/utils/MessageGroupingUtils';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import LocalUserSpamOverride from '@app/features/moderation/state/LocalUserSpamOverride';
import {shouldDisableAutofocusOnMobile} from '@app/features/platform/utils/AutofocusUtils';
import {PASSWORD_MANAGER_IGNORE_ATTRIBUTES} from '@app/features/platform/utils/PasswordManagerAutocomplete';
import {ChannelFilterSheet} from '@app/features/search/components/search/ChannelFilterSheet';
import {HasFilterSheet, type HasFilterType} from '@app/features/search/components/search/HasFilterSheet';
import {ScopeSheet} from '@app/features/search/components/search/ScopeSheet';
import {SearchFilterChip} from '@app/features/search/components/search/SearchFilterChip';
import {SortModeSheet} from '@app/features/search/components/search/SortModeSheet';
import {UserFilterSheet} from '@app/features/search/components/search/UserFilterSheet';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import styles from '@app/features/theme/styles/ChannelSearchBottomSheet.module.css';
import {
	CloseIcon,
	ExpandChevronIcon,
	FilterIcon,
	LoadingIcon,
	NextIcon,
	PreviousIcon,
	SearchIcon,
	SortIcon,
	UserFilterIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import {Button} from '@app/features/ui/button/Button';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {Trans, useLingui} from '@lingui/react/macro';
import {HashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React, {useCallback, useEffect, useRef, useState} from 'react';

const NEWEST_DESCRIPTOR = msg({
	message: 'Newest',
	comment: 'Sort mode label in the mobile channel search bottom sheet. Sorts results newest first.',
});
const OLDEST_DESCRIPTOR = msg({
	message: 'Oldest',
	comment: 'Sort mode label in the mobile channel search bottom sheet. Sorts results oldest first.',
});
const RELEVANT_DESCRIPTOR = msg({
	message: 'Relevant',
	comment: 'Sort mode label in the mobile channel search bottom sheet. Sorts by relevance.',
});
const MESSAGE_1_USER_DESCRIPTOR = msg({
	message: '1 user',
	comment: 'Summary chip text in the mobile channel search sheet when exactly one user is selected as a filter.',
});
const USERS_DESCRIPTOR = msg({
	message: '{length} users',
	comment:
		'Summary chip text in the mobile channel search sheet when more than one user is selected. length is the count.',
});
const IMAGE_UPLOAD_DESCRIPTOR = msg({
	message: 'image upload',
	comment: 'Label for the has:image filter option in the mobile channel search sheet.',
});
const VIDEO_UPLOAD_DESCRIPTOR = msg({
	message: 'video upload',
	comment: 'Label for the has:video filter option in the mobile channel search sheet.',
});
const AUDIO_UPLOAD_DESCRIPTOR = msg({
	message: 'audio upload',
	comment: 'Label for the has:sound filter option in the mobile channel search sheet.',
});
const FILE_UPLOAD_DESCRIPTOR = msg({
	message: 'file upload',
	comment: 'Label for the has:file filter option in the mobile channel search sheet.',
});
const LINK_PREVIEW_OR_EMBED_DESCRIPTOR = msg({
	message: 'link preview or embed',
	comment: 'Label for the has:embed filter option in the mobile channel search sheet.',
});
const LINK_DESCRIPTOR = msg({
	message: 'link',
	comment: 'Label for the has:link filter option in the mobile channel search sheet.',
});
const STICKER_DESCRIPTOR = msg({
	message: 'sticker',
	comment: 'Label for the has:sticker filter option in the mobile channel search sheet.',
});
const TYPES_DESCRIPTOR = msg({
	message: '{length} types',
	comment:
		'Summary chip text in the mobile channel search sheet when multiple content types are selected. length is the count.',
});
const SEARCH_DESCRIPTOR = msg({
	message: 'Search',
	comment: 'Title of the mobile channel search bottom sheet.',
});
const SEARCH_RESULTS_COUNT_DESCRIPTOR = msg({
	message: '{resultCount} results',
	comment: 'Subtitle in the mobile channel search bottom sheet showing the total number of search results.',
});
const SEARCH_MESSAGES_DESCRIPTOR = msg({
	message: 'Search messages',
	comment: 'Placeholder text in the mobile channel search input.',
});
const FROM_DESCRIPTOR = msg({
	message: 'From',
	comment: 'Filter category label in the mobile channel search sheet for the from: user filter.',
});
const HAS_DESCRIPTOR = msg({
	message: 'Has',
	comment: 'Filter category label in the mobile channel search sheet for the has: content type filter.',
});
const IN_DESCRIPTOR = msg({
	message: 'In',
	comment: 'Filter category label in the mobile channel search sheet for the in: channel filter.',
});
const SORT_DESCRIPTOR = msg({
	message: 'Sort',
	comment: 'Filter category label in the mobile channel search sheet for the sort mode selector.',
});
const SCOPE_DESCRIPTOR = msg({
	message: 'Scope',
	comment: 'Filter category label in the mobile channel search sheet for the search scope selector.',
});
const FROM_USER_DESCRIPTOR = msg({
	message: 'From user',
	comment: 'Section header in the mobile channel search user picker. Filters by sending user.',
});
const IN_CHANNEL_DESCRIPTOR = msg({
	message: 'In channel',
	comment: 'Section header in the mobile channel search channel picker. Filters by channel.',
});
const MESSAGE_1_CHANNEL_DESCRIPTOR = msg({
	message: '1 channel',
	comment: 'Summary chip text in the mobile channel search sheet when exactly one channel is selected as a filter.',
});
const CHANNELS_DESCRIPTOR = msg({
	message: '{length} channels',
	comment:
		'Summary chip text in the mobile channel search sheet when more than one channel is selected. length is the count.',
});

interface ChannelSearchBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: Channel;
}

const EMPTY_SEARCH_MESSAGES: Array<Message> = [];
const EMPTY_SEARCH_CHANNELS: Array<Channel> = [];
export const ChannelSearchBottomSheet: React.FC<ChannelSearchBottomSheetProps> = observer(
	({isOpen, onClose, channel}) => {
		const {i18n} = useLingui();
		const [contentQuery, setContentQuery] = useState('');
		const [menuOpen, setMenuOpen] = useState(false);
		const [selectedMessage, setSelectedMessage] = useState<Message | null>(null);
		const [selectedMessageChannel, setSelectedMessageChannel] = useState<Channel | null>(null);
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		const inputRef = useRef<HTMLInputElement>(null);
		const [hasFilters, setHasFilters] = useState<Array<HasFilterType>>([]);
		const [fromUserIds, setFromUserIds] = useState<Array<string>>([]);
		const [inChannelIds, setInChannelIds] = useState<Array<string>>([]);
		const [revealedGroupKeys, setRevealedGroupKeys] = useState<Set<string>>(new Set());
		const [hasSheetOpen, setHasSheetOpen] = useState(false);
		const [userSheetOpen, setUserSheetOpen] = useState(false);
		const [channelSheetOpen, setChannelSheetOpen] = useState(false);
		const [sortSheetOpen, setSortSheetOpen] = useState(false);
		const [scopeSheetOpen, setScopeSheetOpen] = useState(false);
		useMessageListKeyboardNavigation({
			containerRef: scrollerRef,
		});
		const {
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
		} = useChannelSearch({channel});
		const successResults = machineState.status === 'success' ? machineState.results : EMPTY_SEARCH_MESSAGES;
		const successChannels = machineState.status === 'success' ? machineState.channels : EMPTY_SEARCH_CHANNELS;
		const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(successResults);
		const searchChannelsById = React.useMemo(
			() => new Map(successChannels.map((searchChannel) => [searchChannel.id, searchChannel])),
			[successChannels],
		);
		const messagesByChannel = React.useMemo(() => {
			const grouped = new Map<string, Array<Message>>();
			for (const message of successResults) {
				if (!grouped.has(message.channelId)) {
					grouped.set(message.channelId, []);
				}
				grouped.get(message.channelId)!.push(message);
			}
			return grouped;
		}, [successResults]);
		const spammerOverrideVersion = LocalUserSpamOverride.version;
		const collapsedMessageVisibility = React.useMemo(
			() => ({
				isMessageRevealed: (message: Message) => {
					const channelMessages = messagesByChannel.get(message.channelId);
					if (!channelMessages) {
						return false;
					}
					const messageChannel = searchChannelsById.get(message.channelId) ?? Channels.getChannel(message.channelId);
					if (!messageChannel) {
						return false;
					}
					const groupKey = getCollapsedMessageGroupKey({
						channel: messageChannel,
						messages: channelMessages,
						messageId: message.id,
						treatSpam: true,
					});
					return groupKey != null && revealedGroupKeys.has(groupKey);
				},
			}),
			[messagesByChannel, revealedGroupKeys, searchChannelsById, spammerOverrideVersion],
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
		const sortModeLabel = (() => {
			if (sortMode === 'newest') return i18n._(NEWEST_DESCRIPTOR);
			if (sortMode === 'oldest') return i18n._(OLDEST_DESCRIPTOR);
			return i18n._(RELEVANT_DESCRIPTOR);
		})();
		const buildFilters = useCallback((): ChannelSearchFilters => {
			return {
				content: contentQuery.trim() || undefined,
				has: hasFilters.length > 0 ? hasFilters : undefined,
				authorIds: fromUserIds.length > 0 ? fromUserIds : undefined,
				channelIds: inChannelIds.length > 0 ? inChannelIds : undefined,
			};
		}, [contentQuery, hasFilters, fromUserIds, inChannelIds]);
		const handleSearch = useCallback(() => {
			const filters = buildFilters();
			if (!filters.content && !filters.has?.length && !filters.authorIds?.length && !filters.channelIds?.length) {
				return;
			}
			performFilterSearch(filters);
		}, [buildFilters, performFilterSearch]);
		const handleClear = useCallback(() => {
			setContentQuery('');
			setHasFilters([]);
			setFromUserIds([]);
			setInChannelIds([]);
			reset();
		}, [reset]);
		const handleNextPage = useCallback(() => {
			if (machineState.status !== 'success') return;
			const totalPages = Math.max(1, Math.ceil(machineState.total / machineState.hitsPerPage));
			if (machineState.page < totalPages) {
				goToPage(machineState.page + 1);
			}
		}, [machineState, goToPage]);
		const handlePrevPage = useCallback(() => {
			if (machineState.status !== 'success' || machineState.page === 1) return;
			goToPage(machineState.page - 1);
		}, [machineState, goToPage]);
		const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
			if (isIMEComposing(e)) {
				return;
			}
			if (e.key === 'Enter') {
				e.preventDefault();
				handleSearch();
			} else if (e.key === 'Escape') {
				e.preventDefault();
				if (contentQuery) {
					setContentQuery('');
				} else {
					onClose();
				}
			}
		};
		const handleJump = (channelId: string, messageId: string) => {
			goToMessage(channelId, messageId);
			onClose();
			focusChannelTextareaAfterNavigation(channelId);
		};
		const handleTap = (message: Message) => {
			handleJump(message.channelId, message.id);
		};
		const handleDelete = useCallback(
			(bypassConfirm = false) => {
				if (!selectedMessage) return;
				if (bypassConfirm) {
					MessageCommands.remove(selectedMessage.channelId, selectedMessage.id);
				} else {
					MessageCommands.showDeleteConfirmation(i18n, {
						message: selectedMessage,
						showShiftBypassConfirmationTip: true,
					});
				}
			},
			[selectedMessage, i18n],
		);
		useEffect(() => {
			if (shouldDisableAutofocusOnMobile()) {
				return;
			}
			if (isOpen && inputRef.current) {
				setTimeout(() => {
					inputRef.current?.focus();
				}, 100);
			}
		}, [isOpen]);
		useEffect(() => {
			if (hasSearched) {
				const filters = buildFilters();
				if (filters.content || filters.has?.length || filters.authorIds?.length || filters.channelIds?.length) {
					performFilterSearch(filters);
				}
			}
		}, [hasFilters, fromUserIds, inChannelIds]);
		useEffect(() => {
			setRevealedGroupKeys(new Set());
		}, [machineState.status === 'success' ? machineState.results : null]);
		useEffect(() => {
			if (machineState.status !== 'success' || machineState.results.length === 0) {
				clearChannelSearchHighlight();
				return;
			}
			const trimmedQuery = contentQuery.trim();
			if (!trimmedQuery) {
				clearChannelSearchHighlight();
				return;
			}
			const container = scrollerRef.current?.getScrollerNode();
			if (!container) {
				return;
			}
			const searchTerms = trimmedQuery.split(/\s+/).filter((term) => term.length > 0);
			applyChannelSearchHighlight(container, searchTerms);
			return () => {
				clearChannelSearchHighlight();
			};
		}, [machineState, contentQuery]);
		const hasActiveFilters = hasFilters.length > 0 || fromUserIds.length > 0 || inChannelIds.length > 0;
		const canSearch = contentQuery.trim() || hasActiveFilters;
		const getFromUserLabel = (): string => {
			if (fromUserIds.length === 0) return '';
			if (fromUserIds.length === 1) {
				const user = Users.getUser(fromUserIds[0]);
				return user ? NicknameUtils.getDisplayName(user) : i18n._(MESSAGE_1_USER_DESCRIPTOR);
			}
			return i18n._(USERS_DESCRIPTOR, {length: fromUserIds.length});
		};
		const getInChannelLabel = (): string => {
			if (inChannelIds.length === 0) return '';
			if (inChannelIds.length === 1) {
				const inChannel = Channels.getChannel(inChannelIds[0]);
				return inChannel?.name ? `#${inChannel.name}` : i18n._(MESSAGE_1_CHANNEL_DESCRIPTOR);
			}
			return i18n._(CHANNELS_DESCRIPTOR, {length: inChannelIds.length});
		};
		const getHasFilterDisplayLabel = (filter: HasFilterType): string => {
			switch (filter) {
				case 'image':
					return i18n._(IMAGE_UPLOAD_DESCRIPTOR);
				case 'video':
					return i18n._(VIDEO_UPLOAD_DESCRIPTOR);
				case 'sound':
					return i18n._(AUDIO_UPLOAD_DESCRIPTOR);
				case 'file':
					return i18n._(FILE_UPLOAD_DESCRIPTOR);
				case 'embed':
					return i18n._(LINK_PREVIEW_OR_EMBED_DESCRIPTOR);
				case 'link':
					return i18n._(LINK_DESCRIPTOR);
				case 'sticker':
					return i18n._(STICKER_DESCRIPTOR);
				default:
					return filter;
			}
		};
		const getHasFilterLabel = (): string => {
			if (hasFilters.length === 0) return '';
			if (hasFilters.length === 1) return getHasFilterDisplayLabel(hasFilters[0]);
			return i18n._(TYPES_DESCRIPTOR, {length: hasFilters.length});
		};
		const activeScopeOption = scopeOptions.find((opt) => opt.value === scope) ?? scopeOptions[0];
		const SearchResultItem: React.FC<{message: Message; messageChannel: Channel}> = observer(
			({message, messageChannel}) => {
				return (
					<LongPressable
						className={styles.searchResultItem}
						data-message-id={message.id}
						data-is-group-start="true"
						role="button"
						tabIndex={0}
						onClick={() => handleTap(message)}
						onKeyDown={(e) => {
							if (isKeyboardActivationKey(e.key)) {
								e.preventDefault();
								handleTap(message);
							}
						}}
						onLongPress={() => {
							setSelectedMessage(message);
							setSelectedMessageChannel(messageChannel);
							setMenuOpen(true);
						}}
						data-flx="channel.channel-search-bottom-sheet.search-result-item.search-result-item.tap"
					>
						<MessageComponent
							message={message}
							channel={messageChannel}
							previewContext={MessagePreviewContext.LIST_POPOUT}
							data-flx="channel.channel-search-bottom-sheet.search-result-item.message-component"
						/>
					</LongPressable>
				);
			},
		);
		const renderContent = () => {
			if (!hasSearched) {
				return (
					<div
						className={styles.emptyStateContainer}
						data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-container"
					>
						<div
							className={styles.emptyStateContent}
							data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-content"
						>
							<SearchIcon
								className={styles.emptyStateIcon}
								data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-icon"
							/>
							<h3
								className={styles.emptyStateTitle}
								data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-title"
							>
								<Trans>Search messages</Trans>
							</h3>
							<p
								className={styles.emptyStateDescription}
								data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-description"
							>
								<Trans>Use filters or enter keywords to find messages</Trans>
							</p>
						</div>
					</div>
				);
			}
			switch (machineState.status) {
				case 'idle':
				case 'loading':
					return (
						<div
							className={styles.loadingContainer}
							data-flx="channel.channel-search-bottom-sheet.render-content.loading-container"
						>
							<LoadingIcon
								className={styles.loadingIcon}
								data-flx="channel.channel-search-bottom-sheet.render-content.loading-icon"
							/>
						</div>
					);
				case 'indexing':
					return (
						<div
							className={styles.indexingContainer}
							data-flx="channel.channel-search-bottom-sheet.render-content.indexing-container"
						>
							<LoadingIcon
								className={styles.indexingIcon}
								data-flx="channel.channel-search-bottom-sheet.render-content.indexing-icon"
							/>
							<div
								className={styles.indexingContent}
								data-flx="channel.channel-search-bottom-sheet.render-content.indexing-content"
							>
								<h3
									className={styles.indexingTitle}
									data-flx="channel.channel-search-bottom-sheet.render-content.indexing-title"
								>
									<Trans>Indexing channel</Trans>
								</h3>
								<p
									className={styles.indexingDescription}
									data-flx="channel.channel-search-bottom-sheet.render-content.indexing-description"
								>
									<Trans>We're indexing this channel for the first time. This might take a little while...</Trans>
								</p>
							</div>
						</div>
					);
				case 'error':
					return (
						<div
							className={styles.errorContainer}
							data-flx="channel.channel-search-bottom-sheet.render-content.error-container"
						>
							<div
								className={styles.errorContent}
								data-flx="channel.channel-search-bottom-sheet.render-content.error-content"
							>
								<h3
									className={styles.errorTitle}
									data-flx="channel.channel-search-bottom-sheet.render-content.error-title"
								>
									<Trans>Error</Trans>
								</h3>
								<p
									className={styles.errorMessage}
									data-flx="channel.channel-search-bottom-sheet.render-content.error-message"
								>
									{machineState.error}
								</p>
								<Button
									variant="secondary"
									onClick={handleSearch}
									data-flx="channel.channel-search-bottom-sheet.render-content.button.search"
								>
									{i18n._(TRY_AGAIN_DESCRIPTOR)}
								</Button>
							</div>
						</div>
					);
				case 'success': {
					const {results, total, hitsPerPage, page: currentPage} = machineState;
					if (results.length === 0) {
						return (
							<div
								className={styles.emptyStateContainer}
								data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-container--2"
							>
								<div
									className={styles.emptyStateContent}
									data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-content--2"
								>
									<SearchIcon
										className={styles.emptyStateIcon}
										data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-icon--2"
									/>
									<div
										className={styles.emptyStateContent}
										data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-content--3"
									>
										<h3
											className={styles.emptyStateTitle}
											data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-title--2"
										>
											<Trans>Nothing matches</Trans>
										</h3>
										<p
											className={styles.emptyStateDescription}
											data-flx="channel.channel-search-bottom-sheet.render-content.empty-state-description--2"
										>
											<Trans>Try different filters or search terms.</Trans>
										</p>
									</div>
								</div>
							</div>
						);
					}
					const totalPages = Math.max(1, Math.ceil(total / hitsPerPage));
					const hasMultipleChannels = messagesByChannel.size > 1;
					return (
						<>
							<CollapsedMessageVisibilityProvider
								value={collapsedMessageVisibility}
								data-flx="channel.channel-search-bottom-sheet.render-content.collapsed-message-visibility-provider"
							>
								<Scroller
									ref={scrollerRef}
									className={styles.resultsScroller}
									key="channel-search-results-scroller"
									onCopy={onCopySelectedMessages}
									data-message-selection-root="true"
									data-flx="channel.channel-search-bottom-sheet.render-content.results-scroller"
								>
									{Array.from(messagesByChannel.entries()).map(([channelId, messages]) => {
										const messageChannel = searchChannelsById.get(channelId) ?? Channels.getChannel(channelId);
										if (!messageChannel) {
											return null;
										}
										const renderMessageWrapper = ({
											message,
											index,
											isGroupStart,
											children,
										}: MessageGroupRenderWrapperProps) => (
											<LongPressable
												data-message-index={index}
												data-message-id={message.id}
												data-is-group-start={isGroupStart}
												className={styles.searchResultItem}
												role="button"
												tabIndex={0}
												onClick={() => handleTap(message)}
												onKeyDown={(e) => {
													if (isKeyboardActivationKey(e.key)) {
														e.preventDefault();
														handleTap(message);
													}
												}}
												onLongPress={() => {
													setSelectedMessage(message);
													setMenuOpen(true);
												}}
												data-flx="channel.channel-search-bottom-sheet.render-message-wrapper.search-result-item.tap"
											>
												{children}
											</LongPressable>
										);
										return (
											<React.Fragment key={channelId}>
												{hasMultipleChannels && (
													<div
														className={styles.channelSection}
														data-flx="channel.channel-search-bottom-sheet.render-content.channel-section"
													>
														{ChannelUtils.getIcon(messageChannel, {
															className: styles.channelIcon,
														})}
														<span
															className={styles.channelName}
															data-flx="channel.channel-search-bottom-sheet.render-content.channel-name"
														>
															{messageChannel.name || 'Unnamed Channel'}
														</span>
													</div>
												)}
												<SearchResultMessageList
													channel={messageChannel}
													messages={messages}
													revealedGroupKeys={revealedGroupKeys}
													onGroupRevealChange={handleCollapsedGroupRevealChange}
													collapsedGroupClassName={styles.collapsedMessageGroup}
													messagePreviewContext={MessagePreviewContext.LIST_POPOUT}
													renderMessageWrapper={renderMessageWrapper}
													spammerOverrideVersion={spammerOverrideVersion}
													renderMessage={(message) => (
														<SearchResultItem
															message={message}
															messageChannel={messageChannel}
															data-flx="channel.channel-search-bottom-sheet.render-content.search-result-item"
														/>
													)}
													data-flx="channel.channel-search-bottom-sheet.render-content.search-result-message-list"
												/>
											</React.Fragment>
										);
									})}
								</Scroller>
							</CollapsedMessageVisibilityProvider>
							{totalPages > 1 && (
								<div
									className={styles.paginationContainer}
									data-flx="channel.channel-search-bottom-sheet.render-content.pagination-container"
								>
									<button
										type="button"
										onClick={handlePrevPage}
										disabled={currentPage === 1}
										className={styles.paginationButton}
										data-flx="channel.channel-search-bottom-sheet.render-content.pagination-button.prev-page"
									>
										<PreviousIcon
											className={sharedStyles.iconSmall}
											data-flx="channel.channel-search-bottom-sheet.render-content.previous-icon"
										/>
										<Trans>Previous</Trans>
									</button>
									<span
										className={styles.paginationText}
										data-flx="channel.channel-search-bottom-sheet.render-content.pagination-text"
									>
										<Trans>
											Page {currentPage} of {totalPages}
										</Trans>
									</span>
									<button
										type="button"
										onClick={handleNextPage}
										disabled={currentPage === totalPages}
										className={styles.paginationButton}
										data-flx="channel.channel-search-bottom-sheet.render-content.pagination-button.next-page"
									>
										{i18n._(NEXT_DESCRIPTOR)}
										<NextIcon
											className={sharedStyles.iconSmall}
											data-flx="channel.channel-search-bottom-sheet.render-content.next-icon"
										/>
									</button>
								</div>
							)}
						</>
					);
				}
			}
		};
		const headerSubtitle =
			machineState.status === 'success' && machineState.total > 0
				? i18n._(SEARCH_RESULTS_COUNT_DESCRIPTOR, {resultCount: machineState.total})
				: null;
		return (
			<>
				<BottomSheet
					isOpen={isOpen}
					onClose={onClose}
					snapPoints={[0, 1]}
					initialSnap={1}
					disablePadding={true}
					title={i18n._(SEARCH_DESCRIPTOR)}
					data-flx="channel.channel-search-bottom-sheet.bottom-sheet"
				>
					<div className={styles.container} data-flx="channel.channel-search-bottom-sheet.container">
						<div className={styles.searchContainer} data-flx="channel.channel-search-bottom-sheet.search-container">
							<div
								className={styles.searchInputWrapper}
								data-flx="channel.channel-search-bottom-sheet.search-input-wrapper"
							>
								<SearchIcon className={styles.searchIcon} data-flx="channel.channel-search-bottom-sheet.search-icon" />
								<input
									ref={inputRef}
									type="text"
									data-flx="channel.channel-search-bottom-sheet.search-input.set-content-query.text"
									{...PASSWORD_MANAGER_IGNORE_ATTRIBUTES}
									value={contentQuery}
									onChange={(e) => setContentQuery(e.target.value)}
									onKeyDown={handleKeyDown}
									placeholder={i18n._(SEARCH_MESSAGES_DESCRIPTOR)}
									aria-label={i18n._(SEARCH_MESSAGES_DESCRIPTOR)}
									className={styles.searchInput}
								/>
								{(contentQuery || hasActiveFilters) && (
									<button
										type="button"
										onClick={handleClear}
										className={styles.clearButton}
										aria-label={i18n._(CLEAR_SEARCH_DESCRIPTOR)}
										data-flx="channel.channel-search-bottom-sheet.clear-button"
									>
										<CloseIcon
											className={sharedStyles.icon}
											data-flx="channel.channel-search-bottom-sheet.close-icon"
										/>
									</button>
								)}
							</div>
							<div className={styles.filterChipsRow} data-flx="channel.channel-search-bottom-sheet.filter-chips-row">
								<SearchFilterChip
									label={i18n._(FROM_DESCRIPTOR)}
									value={getFromUserLabel()}
									icon={<UserFilterIcon size={14} data-flx="channel.channel-search-bottom-sheet.user-filter-icon" />}
									onPress={() => setUserSheetOpen(true)}
									onRemove={fromUserIds.length > 0 ? () => setFromUserIds([]) : undefined}
									isActive={fromUserIds.length > 0}
									data-flx="channel.channel-search-bottom-sheet.search-filter-chip.set-user-sheet-open"
								/>
								{channel.guildId && (
									<SearchFilterChip
										label={i18n._(IN_DESCRIPTOR)}
										value={getInChannelLabel()}
										icon={
											<HashIcon
												size={remFromPx(14)}
												weight="bold"
												data-flx="channel.channel-search-bottom-sheet.hash-icon"
											/>
										}
										onPress={() => setChannelSheetOpen(true)}
										onRemove={inChannelIds.length > 0 ? () => setInChannelIds([]) : undefined}
										isActive={inChannelIds.length > 0}
										data-flx="channel.channel-search-bottom-sheet.search-filter-chip.set-channel-sheet-open"
									/>
								)}
								<SearchFilterChip
									label={i18n._(HAS_DESCRIPTOR)}
									value={getHasFilterLabel()}
									icon={<FilterIcon size={14} data-flx="channel.channel-search-bottom-sheet.filter-icon" />}
									onPress={() => setHasSheetOpen(true)}
									onRemove={hasFilters.length > 0 ? () => setHasFilters([]) : undefined}
									isActive={hasFilters.length > 0}
									data-flx="channel.channel-search-bottom-sheet.search-filter-chip.set-has-sheet-open"
								/>
								<SearchFilterChip
									label={i18n._(SORT_DESCRIPTOR)}
									value={sortModeLabel}
									icon={<SortIcon size={14} data-flx="channel.channel-search-bottom-sheet.sort-icon" />}
									onPress={() => setSortSheetOpen(true)}
									isActive={false}
									data-flx="channel.channel-search-bottom-sheet.search-filter-chip.set-sort-sheet-open"
								/>
								<SearchFilterChip
									label={activeScopeOption?.label ?? i18n._(SCOPE_DESCRIPTOR)}
									icon={
										<ExpandChevronIcon size={14} data-flx="channel.channel-search-bottom-sheet.expand-chevron-icon" />
									}
									onPress={() => setScopeSheetOpen(true)}
									isActive={false}
									data-flx="channel.channel-search-bottom-sheet.search-filter-chip.set-scope-sheet-open"
								/>
							</div>
							<Button
								variant="primary"
								onClick={handleSearch}
								disabled={!canSearch}
								className={styles.searchButton}
								data-flx="channel.channel-search-bottom-sheet.search-button"
							>
								<Trans>Search</Trans>
							</Button>
							{headerSubtitle && (
								<p className={styles.searchResults} data-flx="channel.channel-search-bottom-sheet.search-results">
									{headerSubtitle}
								</p>
							)}
						</div>
						{renderContent()}
					</div>
				</BottomSheet>
				<HasFilterSheet
					isOpen={hasSheetOpen}
					onClose={() => setHasSheetOpen(false)}
					selectedFilters={hasFilters}
					onFiltersChange={setHasFilters}
					data-flx="channel.channel-search-bottom-sheet.has-filter-sheet"
				/>
				<UserFilterSheet
					isOpen={userSheetOpen}
					onClose={() => setUserSheetOpen(false)}
					channel={channel}
					selectedUserIds={fromUserIds}
					onUsersChange={setFromUserIds}
					title={i18n._(FROM_USER_DESCRIPTOR)}
					data-flx="channel.channel-search-bottom-sheet.user-filter-sheet"
				/>
				{channel.guildId && (
					<ChannelFilterSheet
						isOpen={channelSheetOpen}
						onClose={() => setChannelSheetOpen(false)}
						guildId={channel.guildId}
						selectedChannelIds={inChannelIds}
						onChannelsChange={setInChannelIds}
						title={i18n._(IN_CHANNEL_DESCRIPTOR)}
						data-flx="channel.channel-search-bottom-sheet.channel-filter-sheet"
					/>
				)}
				<SortModeSheet
					isOpen={sortSheetOpen}
					onClose={() => setSortSheetOpen(false)}
					selectedMode={sortMode}
					onModeChange={setSortMode}
					data-flx="channel.channel-search-bottom-sheet.sort-mode-sheet"
				/>
				<ScopeSheet
					isOpen={scopeSheetOpen}
					onClose={() => setScopeSheetOpen(false)}
					selectedScope={scope}
					scopeOptions={scopeOptions}
					onScopeChange={setScope}
					data-flx="channel.channel-search-bottom-sheet.scope-sheet"
				/>
				{selectedMessage && (
					<MessageActionBottomSheet
						isOpen={menuOpen}
						onClose={() => {
							setMenuOpen(false);
							setSelectedMessage(null);
							setSelectedMessageChannel(null);
						}}
						message={selectedMessage}
						sourceChannel={selectedMessageChannel}
						handleDelete={handleDelete}
						data-flx="channel.channel-search-bottom-sheet.message-action-bottom-sheet"
					/>
				)}
			</>
		);
	},
);
