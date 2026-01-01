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

import {Trans, useLingui} from '@lingui/react/macro';
import {
	ArrowLeftIcon,
	ArrowRightIcon,
	CaretDownIcon,
	CircleNotchIcon,
	FunnelIcon,
	MagnifyingGlassIcon,
	SortAscendingIcon,
	UserIcon,
	XIcon,
} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as MessageActionCreators from '~/actions/MessageActionCreators';
import '~/components/channel/ChannelSearchHighlight.css';
import {MessagePreviewContext} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {MessageActionBottomSheet} from '~/components/channel/MessageActionBottomSheet';
import {LongPressable} from '~/components/LongPressable';
import {HasFilterSheet, type HasFilterType} from '~/components/search/HasFilterSheet';
import {ScopeSheet} from '~/components/search/ScopeSheet';
import {SearchFilterChip} from '~/components/search/SearchFilterChip';
import {SortModeSheet} from '~/components/search/SortModeSheet';
import {UserFilterSheet} from '~/components/search/UserFilterSheet';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Button} from '~/components/uikit/Button/Button';
import {Scroller, type ScrollerHandle} from '~/components/uikit/Scroller';
import {type ChannelSearchFilters, useChannelSearch} from '~/hooks/useChannelSearch';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {MessageRecord} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import UserStore from '~/stores/UserStore';
import styles from '~/styles/ChannelSearchBottomSheet.module.css';
import {applyChannelSearchHighlight, clearChannelSearchHighlight} from '~/utils/ChannelSearchHighlight';
import * as ChannelUtils from '~/utils/ChannelUtils';
import {goToMessage} from '~/utils/MessageNavigator';
import sharedStyles from './shared.module.css';

interface ChannelSearchBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channel: ChannelRecord;
}

export const ChannelSearchBottomSheet: React.FC<ChannelSearchBottomSheetProps> = observer(
	({isOpen, onClose, channel}) => {
		const {t, i18n} = useLingui();
		const [contentQuery, setContentQuery] = React.useState('');
		const [menuOpen, setMenuOpen] = React.useState(false);
		const [selectedMessage, setSelectedMessage] = React.useState<MessageRecord | null>(null);
		const scrollerRef = React.useRef<ScrollerHandle | null>(null);
		const inputRef = React.useRef<HTMLInputElement>(null);

		const [hasFilters, setHasFilters] = React.useState<Array<HasFilterType>>([]);
		const [fromUserIds, setFromUserIds] = React.useState<Array<string>>([]);

		const [hasSheetOpen, setHasSheetOpen] = React.useState(false);
		const [userSheetOpen, setUserSheetOpen] = React.useState(false);
		const [sortSheetOpen, setSortSheetOpen] = React.useState(false);
		const [scopeSheetOpen, setScopeSheetOpen] = React.useState(false);

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

		const buildFilters = React.useCallback((): ChannelSearchFilters => {
			return {
				content: contentQuery.trim() || undefined,
				has: hasFilters.length > 0 ? hasFilters : undefined,
				authorIds: fromUserIds.length > 0 ? fromUserIds : undefined,
			};
		}, [contentQuery, hasFilters, fromUserIds]);

		const handleSearch = React.useCallback(() => {
			const filters = buildFilters();
			if (!filters.content && !filters.has?.length && !filters.authorIds?.length) {
				return;
			}
			performFilterSearch(filters);
		}, [buildFilters, performFilterSearch]);

		const handleClear = React.useCallback(() => {
			setContentQuery('');
			setHasFilters([]);
			setFromUserIds([]);
			reset();
		}, [reset]);

		const handleNextPage = React.useCallback(() => {
			if (machineState.status !== 'success') return;
			const totalPages = Math.max(1, Math.ceil(machineState.total / machineState.hitsPerPage));
			if (machineState.page < totalPages) {
				goToPage(machineState.page + 1);
			}
		}, [machineState, goToPage]);

		const handlePrevPage = React.useCallback(() => {
			if (machineState.status !== 'success' || machineState.page === 1) return;
			goToPage(machineState.page - 1);
		}, [machineState, goToPage]);

		const handleKeyDown = (e: React.KeyboardEvent<HTMLInputElement>) => {
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
		};

		const handleTap = (message: MessageRecord) => {
			handleJump(message.channelId, message.id);
		};

		const handleDelete = React.useCallback(
			(bypassConfirm = false) => {
				if (!selectedMessage) return;
				if (bypassConfirm) {
					MessageActionCreators.remove(selectedMessage.channelId, selectedMessage.id);
				} else {
					MessageActionCreators.showDeleteConfirmation(i18n, {message: selectedMessage});
				}
			},
			[t, selectedMessage],
		);

		React.useEffect(() => {
			if (isOpen && inputRef.current) {
				setTimeout(() => {
					inputRef.current?.focus();
				}, 100);
			}
		}, [isOpen]);

		React.useEffect(() => {
			if (hasSearched) {
				const filters = buildFilters();
				if (filters.content || filters.has?.length || filters.authorIds?.length) {
					performFilterSearch(filters);
				}
			}
		}, [hasFilters, fromUserIds]);

		React.useEffect(() => {
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

		const hasActiveFilters = hasFilters.length > 0 || fromUserIds.length > 0;
		const canSearch = contentQuery.trim() || hasActiveFilters;

		const getFromUserLabel = (): string => {
			if (fromUserIds.length === 0) return '';
			if (fromUserIds.length === 1) {
				const user = UserStore.getUser(fromUserIds[0]);
				return user?.username ?? t`1 user`;
			}
			return t`${fromUserIds.length} users`;
		};

		const getHasFilterLabel = (): string => {
			if (hasFilters.length === 0) return '';
			if (hasFilters.length === 1) return hasFilters[0];
			return t`${hasFilters.length} types`;
		};

		const activeScopeOption = scopeOptions.find((opt) => opt.value === scope) ?? scopeOptions[0];

		const SearchResultItem: React.FC<{message: MessageRecord; messageChannel: ChannelRecord}> = observer(
			({message, messageChannel}) => {
				return (
					<LongPressable
						className={styles.searchResultItem}
						role="button"
						tabIndex={0}
						onClick={() => handleTap(message)}
						onKeyDown={(e) => {
							if (e.key === 'Enter' || e.key === ' ') {
								e.preventDefault();
								handleTap(message);
							}
						}}
						onLongPress={() => {
							setSelectedMessage(message);
							setMenuOpen(true);
						}}
					>
						<Message message={message} channel={messageChannel!} previewContext={MessagePreviewContext.LIST_POPOUT} />
					</LongPressable>
				);
			},
		);

		const renderContent = () => {
			if (!hasSearched) {
				return (
					<div className={styles.emptyStateContainer}>
						<div className={styles.emptyStateContent}>
							<MagnifyingGlassIcon className={styles.emptyStateIcon} />
							<h3 className={styles.emptyStateTitle}>
								<Trans>Search Messages</Trans>
							</h3>
							<p className={styles.emptyStateDescription}>
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
						<div className={styles.loadingContainer}>
							<CircleNotchIcon className={styles.loadingIcon} />
						</div>
					);
				case 'indexing':
					return (
						<div className={styles.indexingContainer}>
							<CircleNotchIcon className={styles.indexingIcon} />
							<div className={styles.indexingContent}>
								<h3 className={styles.indexingTitle}>
									<Trans>Indexing Channel</Trans>
								</h3>
								<p className={styles.indexingDescription}>
									<Trans>We're indexing this channel for the first time. This might take a little while...</Trans>
								</p>
							</div>
						</div>
					);
				case 'error':
					return (
						<div className={styles.errorContainer}>
							<div className={styles.errorContent}>
								<h3 className={styles.errorTitle}>
									<Trans>Error</Trans>
								</h3>
								<p className={styles.errorMessage}>{machineState.error}</p>
								<Button variant="secondary" onClick={handleSearch}>
									<Trans>Try Again</Trans>
								</Button>
							</div>
						</div>
					);
				case 'success': {
					const {results, total, hitsPerPage, page: currentPage} = machineState;
					if (results.length === 0) {
						return (
							<div className={styles.emptyStateContainer}>
								<div className={styles.emptyStateContent}>
									<MagnifyingGlassIcon className={styles.emptyStateIcon} />
									<div className={styles.emptyStateContent}>
										<h3 className={styles.emptyStateTitle}>
											<Trans>No Results</Trans>
										</h3>
										<p className={styles.emptyStateDescription}>
											<Trans>Try different filters or search terms</Trans>
										</p>
									</div>
								</div>
							</div>
						);
					}
					const totalPages = Math.max(1, Math.ceil(total / hitsPerPage));
					const messagesByChannel = new Map<string, Array<MessageRecord>>();
					for (const message of results) {
						if (!messagesByChannel.has(message.channelId)) {
							messagesByChannel.set(message.channelId, []);
						}
						messagesByChannel.get(message.channelId)!.push(message);
					}
					const hasMultipleChannels = messagesByChannel.size > 1;
					return (
						<>
							<Scroller ref={scrollerRef} className={styles.resultsScroller} key="channel-search-results-scroller">
								{Array.from(messagesByChannel.entries()).map(([channelId, messages]) => {
									const messageChannel = ChannelStore.getChannel(channelId);
									return (
										<React.Fragment key={channelId}>
											{hasMultipleChannels && messageChannel && (
												<div className={styles.channelSection}>
													{ChannelUtils.getIcon(messageChannel, {
														className: styles.channelIcon,
													})}
													<span className={styles.channelName}>{messageChannel.name || 'Unnamed Channel'}</span>
												</div>
											)}
											{messages.map((message) => (
												<SearchResultItem key={message.id} message={message} messageChannel={messageChannel!} />
											))}
										</React.Fragment>
									);
								})}
							</Scroller>
							{totalPages > 1 && (
								<div className={styles.paginationContainer}>
									<button
										type="button"
										onClick={handlePrevPage}
										disabled={currentPage === 1}
										className={styles.paginationButton}
									>
										<ArrowLeftIcon className={sharedStyles.iconSmall} />
										<Trans>Previous</Trans>
									</button>
									<span className={styles.paginationText}>
										<Trans>
											Page {currentPage} of {totalPages}
										</Trans>
									</span>
									<button
										type="button"
										onClick={handleNextPage}
										disabled={currentPage === totalPages}
										className={styles.paginationButton}
									>
										<Trans>Next</Trans>
										<ArrowRightIcon className={sharedStyles.iconSmall} />
									</button>
								</div>
							)}
						</>
					);
				}
			}
		};

		const headerSubtitle =
			machineState.status === 'success' && machineState.total > 0 ? <Trans>{machineState.total} Results</Trans> : null;

		return (
			<>
				<BottomSheet
					isOpen={isOpen}
					onClose={onClose}
					snapPoints={[0, 1]}
					initialSnap={1}
					disablePadding={true}
					title={t`Search`}
				>
					<div className={styles.container}>
						<div className={styles.searchContainer}>
							<div className={styles.searchInputWrapper}>
								<MagnifyingGlassIcon className={styles.searchIcon} weight="bold" />
								<input
									ref={inputRef}
									type="text"
									value={contentQuery}
									onChange={(e) => setContentQuery(e.target.value)}
									onKeyDown={handleKeyDown}
									placeholder={t`Search messages`}
									className={styles.searchInput}
								/>
								{(contentQuery || hasActiveFilters) && (
									<button
										type="button"
										onClick={handleClear}
										className={styles.clearButton}
										aria-label={t`Clear search`}
									>
										<XIcon className={sharedStyles.icon} weight="bold" />
									</button>
								)}
							</div>

							<div className={styles.filterChipsRow}>
								<SearchFilterChip
									label={t`From`}
									value={getFromUserLabel()}
									icon={<UserIcon size={14} weight="bold" />}
									onPress={() => setUserSheetOpen(true)}
									onRemove={fromUserIds.length > 0 ? () => setFromUserIds([]) : undefined}
									isActive={fromUserIds.length > 0}
								/>
								<SearchFilterChip
									label={t`Has`}
									value={getHasFilterLabel()}
									icon={<FunnelIcon size={14} weight="bold" />}
									onPress={() => setHasSheetOpen(true)}
									onRemove={hasFilters.length > 0 ? () => setHasFilters([]) : undefined}
									isActive={hasFilters.length > 0}
								/>
								<SearchFilterChip
									label={t`Sort`}
									value={sortMode === 'newest' ? t`Newest` : sortMode === 'oldest' ? t`Oldest` : t`Relevant`}
									icon={<SortAscendingIcon size={14} weight="bold" />}
									onPress={() => setSortSheetOpen(true)}
									isActive={false}
								/>
								<SearchFilterChip
									label={activeScopeOption?.label ?? t`Scope`}
									icon={<CaretDownIcon size={14} weight="bold" />}
									onPress={() => setScopeSheetOpen(true)}
									isActive={false}
								/>
							</div>

							<Button variant="primary" onClick={handleSearch} disabled={!canSearch} className={styles.searchButton}>
								<Trans>Search</Trans>
							</Button>

							{headerSubtitle && <p className={styles.searchResults}>{headerSubtitle}</p>}
						</div>
						{renderContent()}
					</div>
				</BottomSheet>

				<HasFilterSheet
					isOpen={hasSheetOpen}
					onClose={() => setHasSheetOpen(false)}
					selectedFilters={hasFilters}
					onFiltersChange={setHasFilters}
				/>
				<UserFilterSheet
					isOpen={userSheetOpen}
					onClose={() => setUserSheetOpen(false)}
					channel={channel}
					selectedUserIds={fromUserIds}
					onUsersChange={setFromUserIds}
					title={t`From user`}
				/>
				<SortModeSheet
					isOpen={sortSheetOpen}
					onClose={() => setSortSheetOpen(false)}
					selectedMode={sortMode}
					onModeChange={setSortMode}
				/>
				<ScopeSheet
					isOpen={scopeSheetOpen}
					onClose={() => setScopeSheetOpen(false)}
					selectedScope={scope}
					scopeOptions={scopeOptions}
					onScopeChange={setScope}
				/>

				{selectedMessage && (
					<MessageActionBottomSheet
						isOpen={menuOpen}
						onClose={() => {
							setMenuOpen(false);
							setSelectedMessage(null);
						}}
						message={selectedMessage}
						handleDelete={handleDelete}
					/>
				)}
			</>
		);
	},
);
