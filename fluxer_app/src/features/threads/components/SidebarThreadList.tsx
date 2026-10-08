// SPDX-License-Identifier: AGPL-3.0-or-later

import channelItemStyles from '@app/features/app/components/layout/ChannelItem.module.css';
import {ChannelItemContent} from '@app/features/app/components/layout/ChannelItemContent';
import channelItemSurfaceStyles from '@app/features/app/components/layout/ChannelItemSurface.module.css';
import {GenericChannelItem} from '@app/features/app/components/layout/GenericChannelItem';
import type {Channel} from '@app/features/channel/models/Channel';
import Navigation from '@app/features/navigation/state/Navigation';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {openThreadFullView} from '@app/features/threads/commands/ThreadNavigation';
import styles from '@app/features/threads/components/SidebarThreadList.module.css';
import {ThreadContextMenu} from '@app/features/threads/components/ThreadContextMenu';
import {useThreadMenuData} from '@app/features/threads/hooks/useThreadMenuData';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {isThreadMutedInSidebar} from '@app/features/threads/utils/ThreadNotificationUtils';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {MentionBadge} from '@app/features/ui/components/MentionBadge';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

interface SidebarThreadRowProps {
	thread: Channel;
	isLast: boolean;
	isSelected: boolean;
}

const ThreadMenuSheet = observer(({thread, onClose}: {thread: Channel; onClose: () => void}) => {
	const {groups} = useThreadMenuData(thread, {onClose});
	return (
		<MenuBottomSheet
			isOpen={true}
			onClose={onClose}
			title={thread.name ?? undefined}
			groups={groups}
			data-flx="threads.sidebar-thread-list.thread-menu-sheet.menu-bottom-sheet"
		/>
	);
});

const SidebarThreadRow = observer(({thread, isLast, isSelected}: SidebarThreadRowProps) => {
	const [menuOpen, setMenuOpen] = useState(false);
	const joined = ThreadMemberships.isMember(thread.id);
	const muted = isThreadMutedInSidebar(thread);
	const mentionCount = ReadStates.getMentionCount(thread.id);
	const isUnread = joined && !muted && ReadStates.hasUnread(thread.id);
	const handleClick = useCallback(() => openThreadFullView(thread), [thread]);
	const handleContextMenu = useCallback(
		(event: React.MouseEvent) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<ThreadContextMenu
					thread={thread}
					onClose={onClose}
					data-flx="threads.sidebar-thread-list.sidebar-thread-row.thread-context-menu"
				/>
			));
		},
		[thread],
	);
	return (
		<div className={styles.threadRow} data-flx="threads.sidebar-thread-list.sidebar-thread-row.thread-row">
			{isUnread && !isSelected && (
				<div
					className={channelItemStyles.unreadIndicator}
					data-flx="threads.sidebar-thread-list.sidebar-thread-row.unread-indicator"
				/>
			)}
			<div className={styles.spine} aria-hidden data-flx="threads.sidebar-thread-list.sidebar-thread-row.spine" />
			{!isLast && (
				<div
					className={styles.spineContinuation}
					aria-hidden
					data-flx="threads.sidebar-thread-list.sidebar-thread-row.spine-continuation"
				/>
			)}
			<GenericChannelItem
				containerClassName={channelItemStyles.container}
				className={clsx(
					channelItemStyles.channelItem,
					channelItemStyles.channelItemRegular,
					styles.threadItem,
					isSelected && channelItemSurfaceStyles.channelItemSurfaceSelected,
					isSelected && channelItemStyles.channelItemSelected,
					!isSelected && channelItemStyles.channelItemHoverable,
					!isSelected && (isUnread || mentionCount > 0) && channelItemStyles.channelItemHighlight,
					!isSelected && !isUnread && mentionCount === 0 && channelItemStyles.channelItemMuted,
					muted && channelItemStyles.channelItemMutedState,
				)}
				isSelected={isSelected}
				aria-current={isSelected ? 'page' : undefined}
				aria-label={thread.name ?? ''}
				onClick={handleClick}
				onContextMenu={handleContextMenu}
				onLongPress={MobileLayout.enabled ? () => setMenuOpen(true) : undefined}
				data-flx="threads.sidebar-thread-list.sidebar-thread-row.generic-channel-item.click"
			>
				<ChannelItemContent
					name={thread.name ?? ''}
					nameClassName={styles.threadName}
					actions={
						mentionCount > 0 ? (
							<MentionBadge
								mentionCount={mentionCount}
								size="small"
								data-flx="threads.sidebar-thread-list.sidebar-thread-row.mention-badge"
							/>
						) : undefined
					}
					data-flx="threads.sidebar-thread-list.sidebar-thread-row.channel-item-content"
				/>
			</GenericChannelItem>
			{menuOpen && (
				<ThreadMenuSheet
					thread={thread}
					onClose={() => setMenuOpen(false)}
					data-flx="threads.sidebar-thread-list.sidebar-thread-row.thread-menu-sheet"
				/>
			)}
		</div>
	);
});

interface SidebarThreadListProps {
	guildId: string;
	parentId: string;
	onlySelected?: boolean;
}

export const SidebarThreadList = observer(({guildId, parentId, onlySelected = false}: SidebarThreadListProps) => {
	if (!ThreadGuilds.isActive(guildId)) return null;
	const selectedThreadId =
		Navigation.threadId ??
		(Navigation.channelId && ChannelThreads.hasThread(Navigation.channelId) ? Navigation.channelId : null);
	const sidebarThreads = ChannelThreads.getSidebarThreads(parentId, selectedThreadId);
	const threads = onlySelected ? sidebarThreads.filter((thread) => thread.id === selectedThreadId) : sidebarThreads;
	if (threads.length === 0) return null;
	return (
		<div className={styles.threadList} role="group" data-flx="threads.sidebar-thread-list.thread-list">
			{threads.map((thread, index) => (
				<SidebarThreadRow
					key={thread.id}
					thread={thread}
					isLast={index === threads.length - 1}
					isSelected={thread.id === selectedThreadId}
					data-flx="threads.sidebar-thread-list.sidebar-thread-row"
				/>
			))}
		</div>
	);
});
