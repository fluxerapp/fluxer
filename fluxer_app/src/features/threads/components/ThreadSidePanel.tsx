// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelChatLayout} from '@app/features/channel/components/ChannelChatLayout';
import headerStyles from '@app/features/channel/components/ChannelHeader.module.css';
import {Messages} from '@app/features/channel/components/ChannelMessages';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {MemberListContainer} from '@app/features/channel/components/MemberListContainer';
import type {Channel} from '@app/features/channel/models/Channel';
import {getPostForum} from '@app/features/forum/utils/ForumChannelUtils';
import {BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Navigation from '@app/features/navigation/state/Navigation';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {closeThreadPanel, openThreadFullView} from '@app/features/threads/commands/ThreadNavigation';
import {ThreadComposerArea} from '@app/features/threads/components/ThreadComposerArea';
import {ThreadContextMenu} from '@app/features/threads/components/ThreadContextMenu';
import {ThreadCreatePane} from '@app/features/threads/components/ThreadCreatePane';
import {ThreadMembersList} from '@app/features/threads/components/ThreadMembersPanel';
import styles from '@app/features/threads/components/ThreadSidePanel.module.css';
import ChannelThreads from '@app/features/threads/state/ChannelThreads';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadPanel from '@app/features/threads/state/ThreadPanel';
import ThreadPanelWidth from '@app/features/threads/state/ThreadPanelWidth';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, ArrowsOutSimpleIcon, ChatsIcon, DotsThreeIcon, UsersIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef} from 'react';

function usePanelResize(): (event: React.PointerEvent<HTMLDivElement>) => void {
	const startRef = useRef<{x: number; width: number} | null>(null);
	return useCallback((event: React.PointerEvent<HTMLDivElement>) => {
		event.preventDefault();
		startRef.current = {x: event.clientX, width: ThreadPanelWidth.width};
		const handleMove = (moveEvent: PointerEvent) => {
			const start = startRef.current;
			if (!start) return;
			ThreadPanelWidth.setWidth(start.width + (start.x - moveEvent.clientX));
		};
		const handleUp = () => {
			startRef.current = null;
			window.removeEventListener('pointermove', handleMove);
			window.removeEventListener('pointerup', handleUp);
		};
		window.addEventListener('pointermove', handleMove);
		window.addEventListener('pointerup', handleUp);
	}, []);
}

const ThreadMembersButton = observer(({thread}: {thread: Channel}) => {
	const {i18n} = useLingui();
	const {isOpen, openProps} = usePopout(`thread-members-${thread.id}`);
	return (
		<Popout
			{...openProps}
			position="bottom-end"
			render={() => (
				<div className={styles.membersPopout} data-flx="threads.thread-side-panel.members-popout">
					<MemberListContainer
						channelId={thread.id}
						className={styles.membersPopoutList}
						data-flx="threads.thread-side-panel.member-list-container"
					>
						<ThreadMembersList thread={thread} data-flx="threads.thread-side-panel.thread-members-list" />
					</MemberListContainer>
				</div>
			)}
			data-flx="threads.thread-side-panel.popout.members"
		>
			<ChannelHeaderIcon
				icon={UsersIcon}
				label={i18n._(D.THREAD_MEMBERS_DESCRIPTOR)}
				isSelected={isOpen}
				aria-haspopup={true}
				aria-expanded={isOpen}
				data-flx="threads.thread-side-panel.channel-header-icon.members"
			/>
		</Popout>
	);
});

const PanelHeader = observer(({thread, title}: {thread?: Channel; title: string}) => {
	const {i18n} = useLingui();
	const handleMore = useCallback(
		(event: React.MouseEvent) => {
			if (!thread) return;
			ContextMenuCommands.openFromEvent(event, ({onClose}) => (
				<ThreadContextMenu
					thread={thread}
					onClose={onClose}
					data-flx="threads.thread-side-panel.panel-header.thread-context-menu"
				/>
			));
		},
		[thread],
	);
	const mobile = MobileLayout.enabled;
	const post = thread ? getPostForum(thread) !== undefined : false;
	return (
		<div className={styles.header} data-flx="threads.thread-side-panel.panel-header.header">
			{mobile && (
				<FocusRing offset={-2} data-flx="threads.thread-side-panel.panel-header.focus-ring">
					<button
						type="button"
						className={headerStyles.backButton}
						aria-label={i18n._(BACK_DESCRIPTOR)}
						onClick={closeThreadPanel}
						data-flx="threads.thread-side-panel.panel-header.back-button"
					>
						<ArrowLeftIcon
							className={headerStyles.backIconBold}
							weight="bold"
							data-flx="threads.thread-side-panel.panel-header.back-icon"
						/>
					</button>
				</FocusRing>
			)}
			{!post && <ChatsIcon className={styles.headerIcon} data-flx="threads.thread-side-panel.panel-header.icon" />}
			<span className={styles.headerTitle} data-flx="threads.thread-side-panel.panel-header.title">
				{title}
			</span>
			<div className={styles.headerActions} data-flx="threads.thread-side-panel.panel-header.actions">
				{thread && !post && (
					<>
						<ThreadMembersButton
							thread={thread}
							data-flx="threads.thread-side-panel.panel-header.thread-members-button"
						/>
						<ChannelHeaderIcon
							icon={ArrowsOutSimpleIcon}
							label={i18n._(D.OPEN_FULL_VIEW_DESCRIPTOR)}
							onClick={() => openThreadFullView(thread)}
							data-flx="threads.thread-side-panel.panel-header.channel-header-icon.full-view"
						/>
					</>
				)}
				{thread && (
					<ChannelHeaderIcon
						icon={DotsThreeIcon}
						iconWeight="bold"
						label={i18n._(D.THREAD_SETTINGS_DESCRIPTOR)}
						onClick={handleMore}
						data-flx="threads.thread-side-panel.panel-header.channel-header-icon.more"
					/>
				)}
				{!mobile && (
					<ChannelHeaderIcon
						icon={XIcon}
						iconWeight="bold"
						label={i18n._(D.CLOSE_THREAD_PANEL_DESCRIPTOR)}
						onClick={closeThreadPanel}
						className={headerStyles.iconButtonDefault}
						data-flx="threads.thread-side-panel.panel-header.channel-header-icon.close"
					/>
				)}
			</div>
		</div>
	);
});

export const ThreadChat = observer(({thread}: {thread: Channel}) => (
	<ChannelChatLayout
		messages={<Messages key={thread.id} channel={thread} data-flx="threads.thread-chat.messages" />}
		textarea={<ThreadComposerArea thread={thread} data-flx="threads.thread-chat.thread-composer-area" />}
		data-flx="threads.thread-chat.channel-chat-layout"
	/>
));

export function useThreadPanelState(parent: Channel | undefined): {
	thread: Channel | undefined;
	createMessageId: string | null | undefined;
} {
	if (!parent || !ThreadGuilds.isActive(parent.guildId)) return {thread: undefined, createMessageId: undefined};
	const openThreadId = Navigation.threadId;
	const thread = openThreadId ? ChannelThreads.getThread(openThreadId) : undefined;
	if (thread && thread.parentId === parent.id) return {thread, createMessageId: undefined};
	const target = ThreadPanel.getCreateTarget(parent.id);
	return {thread: undefined, createMessageId: target ? target.messageId : undefined};
}

export const ThreadSidePanel = observer(({parent}: {parent: Channel}) => {
	const {i18n} = useLingui();
	const {thread, createMessageId} = useThreadPanelState(parent);
	const handleResizeStart = usePanelResize();
	if (!thread && createMessageId === undefined) return null;
	const mobile = MobileLayout.enabled;
	return (
		<aside
			className={styles.panel}
			style={mobile ? {width: '100%', maxWidth: '100%'} : {width: remFromPx(ThreadPanelWidth.width)}}
			aria-label={thread?.name ?? i18n._(D.NEW_THREAD_DESCRIPTOR)}
			data-flx="threads.thread-side-panel.panel"
		>
			{!mobile && (
				<div
					className={styles.resizeHandle}
					role="separator"
					aria-orientation="vertical"
					onPointerDown={handleResizeStart}
					data-flx="threads.thread-side-panel.resize-handle"
				/>
			)}
			<PanelHeader
				thread={thread}
				title={thread?.name ?? i18n._(D.NEW_THREAD_DESCRIPTOR)}
				data-flx="threads.thread-side-panel.panel-header"
			/>
			<div className={styles.content} data-flx="threads.thread-side-panel.content">
				{thread ? (
					<ThreadChat key={thread.id} thread={thread} data-flx="threads.thread-side-panel.thread-chat" />
				) : (
					<ThreadCreatePane
						key={`${parent.id}:${createMessageId ?? ''}`}
						parent={parent}
						messageId={createMessageId ?? null}
						data-flx="threads.thread-side-panel.thread-create-pane"
					/>
				)}
			</div>
		</aside>
	);
});

export const ThreadSplitView = observer(({parent, children}: {parent: Channel; children: React.ReactNode}) => {
	const {thread, createMessageId} = useThreadPanelState(parent);
	if (!ThreadGuilds.isActive(parent.guildId)) return <>{children}</>;
	const panelOpen = thread !== undefined || createMessageId !== undefined;
	if (panelOpen && MobileLayout.enabled) {
		return <ThreadSidePanel parent={parent} data-flx="threads.thread-split-view.thread-side-panel--mobile" />;
	}
	return (
		<div className={styles.split} data-flx="threads.thread-split-view.split">
			<div className={styles.splitMain} data-flx="threads.thread-split-view.split-main">
				{children}
			</div>
			{panelOpen && <ThreadSidePanel parent={parent} data-flx="threads.thread-split-view.thread-side-panel" />}
		</div>
	);
});
