// SPDX-License-Identifier: AGPL-3.0-or-later

import previewStyles from '@app/features/app/components/shared/MessagePreview.module.css';
import {
	EVERYONE_MENTION,
	RECENT_MENTIONS_RETENTION_DAYS,
	ROLE_MENTION,
} from '@app/features/app/config/I18nDisplayConstants';
import {Message} from '@app/features/channel/components/ChannelMessage';
import type {Channel} from '@app/features/channel/models/Channel';
import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import {DIRECT_MESSAGES_DESCRIPTOR, MARK_AS_READ_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InboxCommands from '@app/features/inbox/commands/InboxCommands';
import {ensureMembersForMessages} from '@app/features/messaging/commands/MessageCommands';
import {InboxMessageHeader} from '@app/features/messaging/components/popouts/InboxMessageHeader';
import headerStyles from '@app/features/messaging/components/popouts/InboxMessageHeader.module.css';
import styles from '@app/features/messaging/components/popouts/RecentMentionsContent.module.css';
import {useMessageListKeyboardNavigation} from '@app/features/messaging/hooks/useMessageListKeyboardNavigation';
import {useMessageSelectionCopyForMessages} from '@app/features/messaging/hooks/useMessageSelectionCopy';
import {NearViewportSurfaceContext} from '@app/features/messaging/hooks/useNearViewport';
import type {Message as MessagingMessage} from '@app/features/messaging/models/MessagingMessage';
import {goToMessage} from '@app/features/messaging/utils/MessageNavigator';
import * as RecentMentionCommands from '@app/features/notification/commands/RecentMentionCommands';
import MentionFeed from '@app/features/notification/state/MentionFeed';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {Scroller, type ScrollerHandle} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import ContextMenu from '@app/features/ui/state/ContextMenu';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {MessagePreviewContext} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {CheckIcon, DotsThreeIcon, FlagCheckeredIcon, SparkleIcon, XIcon} from '@phosphor-icons/react';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useMemo, useRef} from 'react';

const UNKNOWN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Unknown community',
	comment: 'Short label in the recent mentions content popout. Keep it concise.',
});
const FILTER_MENTIONS_DESCRIPTOR = msg({
	message: 'Filter mentions',
	comment: 'Short label in the recent mentions content popout. Keep it concise.',
});
const REMOVE_MENTION_DESCRIPTOR = msg({
	message: 'Remove mention',
	comment:
		'Button or menu action label in the recent mentions content popout. Keep it concise. Keep the tone plain and specific.',
});
const INCLUDE_MENTION_TYPE_DESCRIPTOR = msg({
	message: 'Include {mentionType} mentions',
	comment: 'Recent mentions filter checkbox. mentionType is a mention token such as @everyone or @role.',
});
const RECENT_MENTIONS_RETENTION_DESCRIPTOR = msg({
	message: 'Mentions of you show up here for {retentionDays} days.',
	comment: 'Recent mentions empty-state description. retentionDays is the number of days mentions are retained.',
});
const readonlyBehaviorOverrides = {
	disableContextMenu: true,
};
const FilterMenuContent = observer(() => {
	const {i18n} = useLingui();
	const filters = MentionFeed.getFilters();
	const accessibleMentionCount = MentionFeed.getAccessibleMentions().length;
	return (
		<>
			<MenuGroup data-flx="messaging.recent-mentions-content.filter-menu-content.menu-group">
				<MenuItem
					disabled={accessibleMentionCount === 0}
					icon={
						<CheckIcon
							weight="bold"
							className={styles.iconMedium}
							data-flx="messaging.recent-mentions-content.filter-menu-content.check-icon"
						/>
					}
					onClick={() => {
						void RecentMentionCommands.markLoadedAsRead();
					}}
					data-flx="messaging.recent-mentions-content.filter-menu-content.menu-item.mark-loaded-read"
				>
					{i18n._(MARK_AS_READ_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="messaging.recent-mentions-content.filter-menu-content.menu-group--2">
				<CheckboxItem
					checked={filters.includeEveryone}
					onCheckedChange={(checked) => {
						RecentMentionCommands.updateFilters({
							includeEveryone: checked,
						});
						RecentMentionCommands.fetch();
					}}
					data-flx="messaging.recent-mentions-content.filter-menu-content.checkbox-item"
				>
					{i18n._(INCLUDE_MENTION_TYPE_DESCRIPTOR, {mentionType: EVERYONE_MENTION})}
				</CheckboxItem>
				<CheckboxItem
					checked={filters.includeRoles}
					onCheckedChange={(checked) => {
						RecentMentionCommands.updateFilters({
							includeRoles: checked,
						});
						RecentMentionCommands.fetch();
					}}
					data-flx="messaging.recent-mentions-content.filter-menu-content.checkbox-item--2"
				>
					{i18n._(INCLUDE_MENTION_TYPE_DESCRIPTOR, {mentionType: ROLE_MENTION})}
				</CheckboxItem>
				<CheckboxItem
					checked={filters.includeGuilds}
					onCheckedChange={(checked) => {
						RecentMentionCommands.updateFilters({
							includeGuilds: checked,
						});
						RecentMentionCommands.fetch();
					}}
					data-flx="messaging.recent-mentions-content.filter-menu-content.checkbox-item--3"
				>
					<Trans>Include all community mentions</Trans>
				</CheckboxItem>
			</MenuGroup>
		</>
	);
});
interface MentionGroup {
	key: string;
	label: string;
	mentions: Array<MessagingMessage>;
}

const MentionMessageCard = observer(function MentionMessageCard({
	message,
	channel,
	onJump,
	onRemove,
}: {
	message: MessagingMessage;
	channel: Channel;
	onJump: (channelId: string, messageId: string) => void;
	onRemove: (messageId: string) => void;
}) {
	const {i18n} = useLingui();
	return (
		<div className={styles.messageCard} data-flx="messaging.recent-mentions-content.message-card">
			<InboxMessageHeader
				channel={channel}
				onClick={() => onJump(message.channelId, message.id)}
				rightActions={
					<Tooltip
						text={i18n._(REMOVE_MENTION_DESCRIPTOR)}
						position="top"
						data-flx="messaging.recent-mentions-content.tooltip"
					>
						<FocusRing offset={-2} data-flx="messaging.recent-mentions-content.focus-ring--2">
							<button
								type="button"
								className={headerStyles.headerIconButton}
								onClick={() => onRemove(message.id)}
								aria-label={i18n._(REMOVE_MENTION_DESCRIPTOR)}
								data-flx="messaging.recent-mentions-content.button.remove"
							>
								<XIcon
									weight="bold"
									className={headerStyles.headerIcon}
									data-flx="messaging.recent-mentions-content.x-icon"
								/>
							</button>
						</FocusRing>
					</Tooltip>
				}
				data-flx="messaging.recent-mentions-content.inbox-message-header.jump-to-message"
			/>
			<div
				className={previewStyles.previewCard}
				data-message-id={message.id}
				data-is-group-start="true"
				data-flx="messaging.recent-mentions-content.div--5"
			>
				<Message
					message={message}
					channel={channel}
					previewContext={MessagePreviewContext.LIST_POPOUT}
					behaviorOverrides={readonlyBehaviorOverrides}
					suppressMessageActions
					onHeadingActivate={() => onJump(message.channelId, message.id)}
					data-flx="messaging.recent-mentions-content.message"
				/>
				<div className={previewStyles.actionButtons} data-flx="messaging.recent-mentions-content.div--6">
					<FocusRing offset={-2} data-flx="messaging.recent-mentions-content.focus-ring--3">
						<button
							type="button"
							className={previewStyles.actionButton}
							onClick={() => onJump(message.channelId, message.id)}
							data-flx="messaging.recent-mentions-content.button.jump-to-message"
						>
							<Trans>Jump</Trans>
						</button>
					</FocusRing>
				</div>
			</div>
		</div>
	);
});

export const RecentMentionsContent = observer(
	({onHeaderActionsChange}: {onHeaderActionsChange?: (actions: React.ReactNode) => void}) => {
		const {i18n} = useLingui();
		const fetched = MentionFeed.fetched;
		const hasMore = MentionFeed.getHasMore();
		const isLoadingMore = MentionFeed.getIsLoadingMore();
		const filterButtonRef = useRef<HTMLButtonElement>(null);
		const scrollerRef = useRef<ScrollerHandle | null>(null);
		const resolveMentionsScrollSurface = useMemo(() => () => scrollerRef.current?.getViewportElement() ?? null, []);
		const {isOpen: isFilterMenuOpen, withTracking} = useContextMenuTrigger();
		const accessibleMentions = MentionFeed.getAccessibleMentions();
		const onCopySelectedMessages = useMessageSelectionCopyForMessages<HTMLDivElement>(accessibleMentions);
		const groupedMentions = useMemo(() => {
			const groups: Array<MentionGroup> = [];
			const groupIdx = new Map<string, number>();
			const directMessagesLabel = i18n._(DIRECT_MESSAGES_DESCRIPTOR);
			for (const message of accessibleMentions) {
				const channel = Channels.getChannel(message.channelId);
				if (!channel) continue;
				const guildId = channel.guildId ?? null;
				const key = guildId ?? '__dm__';
				let idx = groupIdx.get(key);
				if (idx === undefined) {
					const label = guildId
						? (Guilds.getGuild(guildId)?.name ?? i18n._(UNKNOWN_COMMUNITY_DESCRIPTOR))
						: directMessagesLabel;
					idx = groups.length;
					groups.push({key, label, mentions: []});
					groupIdx.set(key, idx);
				}
				groups[idx].mentions.push(message);
			}
			return groups;
		}, [accessibleMentions, i18n.locale]);
		useEffect(() => {
			if (!fetched) {
				RecentMentionCommands.fetch();
			}
		}, [fetched]);
		useEffect(() => {
			if (accessibleMentions.length === 0) return;
			void ensureMembersForMessages(accessibleMentions);
		}, [accessibleMentions]);
		useMessageListKeyboardNavigation({
			containerRef: scrollerRef,
		});
		const handleFilterPointerDown = useCallback((event: React.PointerEvent) => {
			const contextMenu = ContextMenu.contextMenu;
			const isOpen = !!contextMenu && contextMenu.target.target === filterButtonRef.current;
			if (isOpen) {
				event.stopPropagation();
				event.preventDefault();
				ContextMenuCommands.close();
			}
		}, []);
		const handleFilterClick = useCallback(
			(event: React.MouseEvent) => {
				const contextMenu = ContextMenu.contextMenu;
				const isOpen = !!contextMenu && contextMenu.target.target === event.currentTarget;
				if (isOpen) {
					return;
				}
				ContextMenuCommands.openFromEvent(
					event,
					() => (
						<FilterMenuContent data-flx="messaging.recent-mentions-content.handle-filter-click.filter-menu-content" />
					),
					withTracking(),
				);
			},
			[withTracking],
		);
		useEffect(() => {
			onHeaderActionsChange?.(
				<FocusRing
					offset={-2}
					focusTarget={filterButtonRef}
					ringTarget={filterButtonRef}
					data-flx="messaging.recent-mentions-content.focus-ring"
				>
					<button
						ref={filterButtonRef}
						type="button"
						onPointerDownCapture={handleFilterPointerDown}
						onClick={handleFilterClick}
						className={clsx(styles.filterButton, isFilterMenuOpen && styles.filterButtonActive)}
						aria-label={i18n._(FILTER_MENTIONS_DESCRIPTOR)}
						data-flx="messaging.recent-mentions-content.filter-button.filter-click"
					>
						<DotsThreeIcon
							weight="bold"
							className={styles.iconMedium}
							data-flx="messaging.recent-mentions-content.icon-medium"
						/>
					</button>
				</FocusRing>,
			);
			return () => onHeaderActionsChange?.(null);
		}, [onHeaderActionsChange, handleFilterPointerDown, handleFilterClick, isFilterMenuOpen, i18n]);
		const handleScroll = useCallback(
			(event: React.UIEvent<HTMLDivElement>) => {
				const target = event.currentTarget;
				const scrollPercentage = (target.scrollTop + target.offsetHeight) / target.scrollHeight;
				if (scrollPercentage > 0.8 && hasMore && !isLoadingMore) {
					RecentMentionCommands.loadMore();
				}
			},
			[hasMore, isLoadingMore],
		);
		const handleJumpToMessage = useCallback((channelId: string, messageId: string) => {
			goToMessage(channelId, messageId);
			InboxCommands.closeInboxAndFocusChannelTextarea(channelId);
		}, []);
		const handleRemoveMention = useCallback((messageId: string) => {
			RecentMentionCommands.remove(messageId);
		}, []);
		if (!fetched) {
			return (
				<div className={previewStyles.emptyState} data-flx="messaging.recent-mentions-content.div">
					<Spinner data-flx="messaging.recent-mentions-content.spinner" />
				</div>
			);
		}
		if (accessibleMentions.length === 0) {
			return (
				<div className={previewStyles.emptyState} data-flx="messaging.recent-mentions-content.div--2">
					<div className={previewStyles.emptyStateContent} data-flx="messaging.recent-mentions-content.div--3">
						<SparkleIcon
							className={previewStyles.emptyStateIcon}
							data-flx="messaging.recent-mentions-content.sparkle-icon"
						/>
						<div className={previewStyles.emptyStateTextContainer} data-flx="messaging.recent-mentions-content.div--4">
							<h3 className={previewStyles.emptyStateTitle} data-flx="messaging.recent-mentions-content.h3">
								<Trans>No mentions yet</Trans>
							</h3>
							<p className={previewStyles.emptyStateDescription} data-flx="messaging.recent-mentions-content.p">
								{i18n._(RECENT_MENTIONS_RETENTION_DESCRIPTOR, {
									retentionDays: RECENT_MENTIONS_RETENTION_DAYS,
								})}
							</p>
						</div>
					</div>
				</div>
			);
		}
		return (
			<NearViewportSurfaceContext.Provider value={resolveMentionsScrollSurface}>
				<Scroller
					className={styles.scroller}
					onScroll={handleScroll}
					key="recent-mentions-scroller"
					ref={scrollerRef}
					onCopy={onCopySelectedMessages}
					data-message-selection-root="true"
					data-flx="messaging.recent-mentions-content.scroller"
				>
					{groupedMentions.map((group) => {
						return (
							<section
								key={group.key}
								className={styles.guildGroup}
								aria-label={group.label}
								data-flx="messaging.recent-mentions-content.guild-group"
							>
								{group.mentions.map((message) => {
									const channel = Channels.getChannel(message.channelId);
									if (!channel) return null;
									return (
										<MentionMessageCard
											key={message.id}
											message={message}
											channel={channel}
											onJump={handleJumpToMessage}
											onRemove={handleRemoveMention}
											data-flx="messaging.recent-mentions-content.mention-message-card"
										/>
									);
								})}
							</section>
						);
					})}
					{isLoadingMore && (
						<div className={previewStyles.loadingState} data-flx="messaging.recent-mentions-content.div--7">
							<Spinner data-flx="messaging.recent-mentions-content.spinner--2" />
						</div>
					)}
					{!hasMore && !isLoadingMore && (
						<div className={previewStyles.endState} data-flx="messaging.recent-mentions-content.div--8">
							<div className={previewStyles.endStateContent} data-flx="messaging.recent-mentions-content.div--9">
								<FlagCheckeredIcon
									className={previewStyles.endStateIcon}
									data-flx="messaging.recent-mentions-content.flag-checkered-icon"
								/>
								<div
									className={previewStyles.endStateTextContainer}
									data-flx="messaging.recent-mentions-content.div--10"
								>
									<h3 className={previewStyles.endStateTitle} data-flx="messaging.recent-mentions-content.h3--2">
										<Trans>You've reached the end</Trans>
									</h3>
									<p className={previewStyles.endStateDescription} data-flx="messaging.recent-mentions-content.p--2">
										<Trans>You've seen all your recent mentions. More will appear here soon.</Trans>
									</p>
								</div>
							</div>
						</div>
					)}
				</Scroller>
			</NearViewportSurfaceContext.Provider>
		);
	},
);
