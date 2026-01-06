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

import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {DotsThreeIcon, FlagCheckeredIcon, SparkleIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {autorun} from 'mobx';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ContextMenuActionCreators from '~/actions/ContextMenuActionCreators';
import * as RecentMentionActionCreators from '~/actions/RecentMentionActionCreators';
import {MessagePreviewContext} from '~/Constants';
import {Message} from '~/components/channel/Message';
import styles from '~/components/popouts/RecentMentionsContent.module.css';
import {MessageContextPrefix} from '~/components/shared/MessageContextPrefix/MessageContextPrefix';
import previewStyles from '~/components/shared/MessagePreview.module.css';
import {MenuGroup} from '~/components/uikit/ContextMenu/MenuGroup';
import {MenuItemCheckbox} from '~/components/uikit/ContextMenu/MenuItemCheckbox';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Scroller} from '~/components/uikit/Scroller';
import ChannelStore from '~/stores/ChannelStore';
import ContextMenuStore from '~/stores/ContextMenuStore';
import RecentMentionsStore from '~/stores/RecentMentionsStore';
import {goToMessage} from '~/utils/MessageNavigator';

const FilterMenuContent = observer(() => {
	const filters = RecentMentionsStore.getFilters();

	return (
		<MenuGroup>
			<MenuItemCheckbox
				checked={filters.includeEveryone}
				onChange={(checked) => {
					RecentMentionActionCreators.updateFilters({
						includeEveryone: checked,
					});
					RecentMentionActionCreators.fetch();
				}}
			>
				<Trans>Include @everyone mentions</Trans>
			</MenuItemCheckbox>
			<MenuItemCheckbox
				checked={filters.includeRoles}
				onChange={(checked) => {
					RecentMentionActionCreators.updateFilters({
						includeRoles: checked,
					});
					RecentMentionActionCreators.fetch();
				}}
			>
				<Trans>Include @role mentions</Trans>
			</MenuItemCheckbox>
			<MenuItemCheckbox
				checked={filters.includeGuilds}
				onChange={(checked) => {
					RecentMentionActionCreators.updateFilters({
						includeGuilds: checked,
					});
					RecentMentionActionCreators.fetch();
				}}
			>
				<Trans>Include all community mentions</Trans>
			</MenuItemCheckbox>
		</MenuGroup>
	);
});

export const RecentMentionsContent = observer(
	({onHeaderActionsChange}: {onHeaderActionsChange?: (actions: React.ReactNode) => void}) => {
		const {i18n} = useLingui();

		const fetched = RecentMentionsStore.fetched;
		const hasMore = RecentMentionsStore.getHasMore();
		const isLoadingMore = RecentMentionsStore.getIsLoadingMore();
		const filterButtonRef = React.useRef<HTMLButtonElement>(null);
		const [isFilterMenuOpen, setIsFilterMenuOpen] = React.useState(false);

		const accessibleMentions = RecentMentionsStore.getAccessibleMentions();

		React.useEffect(() => {
			if (!fetched) {
				RecentMentionActionCreators.fetch();
			}
		}, [fetched]);

		React.useEffect(() => {
			const handleContextMenuChange = () => {
				const contextMenu = ContextMenuStore.contextMenu;
				const isOpen =
					!!contextMenu && !!filterButtonRef.current && contextMenu.target.target === filterButtonRef.current;

				setIsFilterMenuOpen(isOpen);
			};

			const disposer = autorun(handleContextMenuChange);
			return () => disposer();
		}, []);

		const handleFilterPointerDown = React.useCallback((event: React.PointerEvent) => {
			const contextMenu = ContextMenuStore.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === filterButtonRef.current;

			if (isOpen) {
				event.stopPropagation();
				event.preventDefault();
				ContextMenuActionCreators.close();
			}
		}, []);

		const handleFilterClick = React.useCallback((event: React.MouseEvent) => {
			const contextMenu = ContextMenuStore.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;

			if (isOpen) {
				return;
			}

			ContextMenuActionCreators.openFromEvent(event, () => <FilterMenuContent />);
		}, []);

		React.useEffect(() => {
			onHeaderActionsChange?.(
				<FocusRing offset={-2} focusTarget={filterButtonRef} ringTarget={filterButtonRef}>
					<button
						ref={filterButtonRef}
						type="button"
						onPointerDownCapture={handleFilterPointerDown}
						onClick={handleFilterClick}
						className={clsx(styles.filterButton, isFilterMenuOpen && styles.filterButtonActive)}
						title={i18n._(msg`Filter mentions`)}
					>
						<DotsThreeIcon weight="bold" className={styles.iconMedium} />
					</button>
				</FocusRing>,
			);

			return () => onHeaderActionsChange?.(null);
		}, [onHeaderActionsChange, handleFilterPointerDown, handleFilterClick, isFilterMenuOpen, i18n]);

		const handleScroll = React.useCallback(
			(event: React.UIEvent<HTMLDivElement>) => {
				const target = event.currentTarget;
				const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;

				if (scrollPercentage > 0.8 && hasMore && !isLoadingMore) {
					RecentMentionActionCreators.loadMore();
				}
			},
			[hasMore, isLoadingMore],
		);

		const handleJumpToMessage = React.useCallback((channelId: string, messageId: string) => {
			goToMessage(channelId, messageId);
		}, []);

		if (accessibleMentions.length === 0) {
			return (
				<div className={previewStyles.emptyState}>
					<div className={previewStyles.emptyStateContent}>
						<SparkleIcon className={previewStyles.emptyStateIcon} />
						<div className={previewStyles.emptyStateTextContainer}>
							<h3 className={previewStyles.emptyStateTitle}>
								<Trans>No Recent Mentions</Trans>
							</h3>
							<p className={previewStyles.emptyStateDescription}>
								<Trans>All @mentions of you will appear here for 7 days.</Trans>
							</p>
						</div>
					</div>
				</div>
			);
		}

		return (
			<Scroller
				className={styles.scroller}
				onScroll={handleScroll}
				key="recent-mentions-scroller"
				reserveScrollbarTrack
			>
				{accessibleMentions.map((message) => {
					const channel = ChannelStore.getChannel(message.channelId);
					if (!channel) return null;

					return (
						<React.Fragment key={message.id}>
							<MessageContextPrefix
								channel={channel}
								showGuildMeta={Boolean(channel.guildId)}
								compact
								onClick={() => handleJumpToMessage(message.channelId, message.id)}
							/>

							<div className={previewStyles.previewCard}>
								<Message message={message} channel={channel} previewContext={MessagePreviewContext.LIST_POPOUT} />

								<div className={previewStyles.actionButtons}>
									<FocusRing offset={-2}>
										<button
											type="button"
											className={previewStyles.actionButton}
											onClick={() => handleJumpToMessage(message.channelId, message.id)}
										>
											<Trans>Jump</Trans>
										</button>
									</FocusRing>

									<FocusRing offset={-2}>
										<button
											type="button"
											className={previewStyles.actionIconButton}
											onClick={() => RecentMentionActionCreators.remove(message.id)}
											title={i18n._(msg`Remove mention`)}
										>
											<XIcon weight="regular" className={previewStyles.actionIcon} />
										</button>
									</FocusRing>
								</div>
							</div>
						</React.Fragment>
					);
				})}

				{isLoadingMore && (
					<div className={previewStyles.loadingState}>
						<div className={previewStyles.loadingText}>
							<Trans>Loading more...</Trans>
						</div>
					</div>
				)}

				{!hasMore && !isLoadingMore && (
					<div className={previewStyles.endState}>
						<div className={previewStyles.endStateContent}>
							<FlagCheckeredIcon className={previewStyles.endStateIcon} />
							<div className={previewStyles.endStateTextContainer}>
								<h3 className={previewStyles.endStateTitle}>
									<Trans>You&apos;ve reached the end</Trans>
								</h3>
								<p className={previewStyles.endStateDescription}>
									<Trans>You&apos;ve seen all your recent mentions. Don&apos;t fret, more will appear here soon</Trans>
								</p>
							</div>
						</div>
					</div>
				)}
			</Scroller>
		);
	},
);
