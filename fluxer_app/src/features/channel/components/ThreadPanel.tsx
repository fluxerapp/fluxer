// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ThreadPanel.module.css';
import {ChannelChatLayout} from '@app/features/channel/components/ChannelChatLayout';
import {Messages} from '@app/features/channel/components/ChannelMessages';
import {ChannelTextarea} from '@app/features/channel/components/ChannelTextarea';
import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import {ChannelNotificationSettingsButton} from '@app/features/channel/components/channel_header_components/ChannelNotificationSettingsButton';
import {ChannelNotificationSettingsDropdown} from '@app/features/channel/components/channel_header_components/ChannelNotificationSettingsDropdown';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import {ThreadSettingsModal} from '@app/features/channel/components/modals/ThreadSettingsModal';
import Channels from '@app/features/channel/state/Channels';
import Threads from '@app/features/channel/state/Threads';
import {ComponentDispatch} from '@app/features/platform/utils/ComponentBus';
import {Routes} from '@app/app/Routes';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {ThreadIcon} from '@app/features/ui/components/icons/ThreadIcon';
import * as MessageCommands from '@app/features/messaging/commands/MessageCommands';
import {MAX_MESSAGES_PER_CHANNEL} from '@fluxer/constants/src/LimitConstants';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {ArrowSquareOutIcon, DotsThreeIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect} from 'react';
import type React from 'react';

const CLOSE_DESCRIPTOR = msg({
	message: 'Close thread panel',
	comment: 'Accessible label on the close button in the thread side panel.',
});

const THREAD_OPTIONS_DESCRIPTOR = msg({
	message: 'Thread options',
	comment: 'Accessible label on the ... options button in the thread side panel.',
});

const LEAVE_THREAD_DESCRIPTOR = msg({message: 'Leave Thread', comment: '... menu item.'});
const OPEN_FULL_VIEW_DESCRIPTOR = msg({message: 'Open in Full View', comment: '... menu item.'});
const MUTE_THREAD_DESCRIPTOR = msg({message: 'Mute Thread', comment: '... menu item.'});
const NOTIFICATION_SETTINGS_DESCRIPTOR = msg({message: 'Notification Settings', comment: '... menu item.'});
const EDIT_THREAD_DESCRIPTOR = msg({message: 'Edit Thread', comment: '... menu item.'});
const SEARCH_DESCRIPTOR = msg({message: 'Search', comment: '... menu item.'});
const PINS_DESCRIPTOR = msg({message: 'Pins', comment: '... menu item.'});
const CLOSE_THREAD_DESCRIPTOR = msg({message: 'Close Thread', comment: '... menu item.'});
const OPEN_THREAD_DESCRIPTOR = msg({message: 'Open Thread', comment: '... menu item.'});
const LOCK_THREAD_DESCRIPTOR = msg({message: 'Lock Thread', comment: '... menu item.'});
const UNLOCK_THREAD_DESCRIPTOR = msg({message: 'Unlock Thread', comment: '... menu item.'});
const LOCK_FAILED_DESCRIPTOR = msg({message: 'Failed to lock thread', comment: 'Error toast.'});
const UNLOCK_FAILED_DESCRIPTOR = msg({message: 'Failed to unlock thread', comment: 'Error toast.'});
const DELETE_THREAD_DESCRIPTOR = msg({message: 'Delete Thread', comment: '... menu item.'});
const COPY_LINK_DESCRIPTOR = msg({message: 'Copy Link', comment: '... menu item.'});
const COPY_THREAD_ID_DESCRIPTOR = msg({message: 'Copy Thread ID', comment: '... menu item.'});
const THREAD_CLOSED_DESCRIPTOR = msg({message: 'Thread closed', comment: 'Toast.'});
const THREAD_OPENED_DESCRIPTOR = msg({message: 'Thread opened', comment: 'Toast.'});
const LINK_COPIED_DESCRIPTOR = msg({message: 'Link copied', comment: 'Toast.'});
const ID_COPIED_DESCRIPTOR = msg({message: 'ID copied', comment: 'Toast.'});
const LOCKED_THREAD_DESCRIPTOR = msg({
	message: 'This thread has been locked. Only moderators can send messages.',
	comment: 'Banner shown in a locked thread.',
});

interface ThreadPanelProps {
	threadId: string;
	onClose: () => void;
}

export const ThreadPanel = observer(({threadId, onClose}: ThreadPanelProps) => {
	const {i18n} = useLingui();
	const thread = Threads.getThread(threadId);
	const channel = Channels.getChannel(threadId);
	const isJoined = Threads.isJoined(threadId);
	const canManage = thread ? Permission.can(Permissions.MANAGE_THREADS, thread.toChannel()) : false;
	const isLocked = thread?.isLocked() ?? false;

	const threadName = thread?.name ?? channel?.name ?? '';
	const parentChannelId = thread?.threadParentChannelId ?? channel?.parentId ?? '';
	const guildId = thread?.guildId ?? channel?.guildId ?? '';

	useEffect(() => {
		void MessageCommands.fetchMessages(threadId, null, null, MAX_MESSAGES_PER_CHANNEL);
	}, [threadId]);

	const handleOpenMenu = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromElementBottomRight(event, ({onClose: closeMenu}) => (
				<>
					<MenuGroup data-flx="channel.thread-panel.menu.group-1">
						{isJoined && (
							<MenuItem
								onClick={() => {
									void ThreadCommands.leave(parentChannelId, threadId);
									closeMenu();
								}}
								data-flx="channel.thread-panel.menu.leave"
							>
								{i18n._(LEAVE_THREAD_DESCRIPTOR)}
							</MenuItem>
						)}
						<MenuItem
							shortcut={<ArrowSquareOutIcon size={14} />}
							onClick={() => {
								if (guildId) NavigationCommands.selectChannel(guildId, threadId);
								closeMenu();
							}}
							data-flx="channel.thread-panel.menu.open-full"
						>
							{i18n._(OPEN_FULL_VIEW_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
					<MenuGroup data-flx="channel.thread-panel.menu.group-2">
						{channel && (
							<MenuItemSubmenu
								label={i18n._(MUTE_THREAD_DESCRIPTOR)}
								render={() => (
									<ChannelNotificationSettingsDropdown
										channel={channel}
										onClose={closeMenu}
										data-flx="channel.thread-panel.menu.mute-dropdown"
									/>
								)}
								data-flx="channel.thread-panel.menu.mute"
							/>
						)}
						{channel && (
							<MenuItemSubmenu
								label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
								render={() => (
									<ChannelNotificationSettingsDropdown
										channel={channel}
										onClose={closeMenu}
										data-flx="channel.thread-panel.menu.notif-dropdown"
									/>
								)}
								data-flx="channel.thread-panel.menu.notif-settings"
							/>
						)}
					</MenuGroup>
					{canManage && (
						<MenuGroup data-flx="channel.thread-panel.menu.group-3">
							<MenuItem
								onClick={() => {
									ModalCommands.push(
										modal(() => (
											<ThreadSettingsModal
												threadId={threadId}
												parentChannelId={parentChannelId}
												data-flx="channel.thread-panel.menu.settings-modal"
											/>
										)),
									);
									closeMenu();
								}}
								data-flx="channel.thread-panel.menu.edit"
							>
								{i18n._(EDIT_THREAD_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
					<MenuGroup data-flx="channel.thread-panel.menu.group-4">
						<MenuItem
							shortcut={<ArrowSquareOutIcon size={14} />}
							onClick={() => {
								if (guildId) NavigationCommands.selectChannel(guildId, threadId);
								closeMenu();
								setTimeout(() => ComponentDispatch.dispatch('SEARCH_BAR_FOCUS', {channelId: threadId}), 150);
							}}
							data-flx="channel.thread-panel.menu.search"
						>
							{i18n._(SEARCH_DESCRIPTOR)}
						</MenuItem>
						{channel && (
							<MenuItem
								shortcut={<ArrowSquareOutIcon size={14} />}
								onClick={() => {
									ComponentDispatch.dispatch('CHANNEL_PINS_OPEN');
									closeMenu();
								}}
								data-flx="channel.thread-panel.menu.pins"
							>
								{i18n._(PINS_DESCRIPTOR)}
							</MenuItem>
						)}
					</MenuGroup>
					{canManage && (
						<MenuGroup data-flx="channel.thread-panel.menu.group-5">
							{thread?.isOpen() && (
								<MenuItem
									onClick={async () => {
										await ThreadCommands.update(parentChannelId, threadId, {state: 1});
										ToastCommands.createToast({type: 'success', children: i18n._(THREAD_CLOSED_DESCRIPTOR)});
										closeMenu();
									}}
									data-flx="channel.thread-panel.menu.close"
								>
									{i18n._(CLOSE_THREAD_DESCRIPTOR)}
								</MenuItem>
							)}
							{thread?.isClosed() && (
								<MenuItem
									onClick={async () => {
										await ThreadCommands.update(parentChannelId, threadId, {state: 0});
										ToastCommands.createToast({type: 'success', children: i18n._(THREAD_OPENED_DESCRIPTOR)});
										closeMenu();
									}}
									data-flx="channel.thread-panel.menu.open"
								>
									{i18n._(OPEN_THREAD_DESCRIPTOR)}
								</MenuItem>
							)}
							{!isLocked && (
								<MenuItem
									onClick={async () => {
										try {
											await ThreadCommands.update(parentChannelId, threadId, {locked: true, archived: true});
										} catch {
											ToastCommands.createToast({type: 'error', children: i18n._(LOCK_FAILED_DESCRIPTOR)});
										}
										closeMenu();
									}}
									data-flx="channel.thread-panel.menu.lock"
								>
									{i18n._(LOCK_THREAD_DESCRIPTOR)}
								</MenuItem>
							)}
							{isLocked && (
								<MenuItem
									onClick={async () => {
										try {
											await ThreadCommands.update(parentChannelId, threadId, {locked: false, archived: false});
										} catch {
											ToastCommands.createToast({type: 'error', children: i18n._(UNLOCK_FAILED_DESCRIPTOR)});
										}
										closeMenu();
									}}
									data-flx="channel.thread-panel.menu.unlock"
								>
									{i18n._(UNLOCK_THREAD_DESCRIPTOR)}
								</MenuItem>
							)}
							<MenuItem
								danger
								onClick={() => {
									void ThreadCommands.remove(parentChannelId, threadId);
									onClose();
									closeMenu();
								}}
								data-flx="channel.thread-panel.menu.delete"
							>
								{i18n._(DELETE_THREAD_DESCRIPTOR)}
							</MenuItem>
						</MenuGroup>
					)}
					<MenuGroup data-flx="channel.thread-panel.menu.group-6">
						<MenuItem
							onClick={() => {
								if (guildId && parentChannelId) {
									const url = window.location.origin + Routes.guildThread(guildId, parentChannelId, threadId);
									void navigator.clipboard.writeText(url);
									ToastCommands.createToast({type: 'success', children: i18n._(LINK_COPIED_DESCRIPTOR)});
								}
								closeMenu();
							}}
							data-flx="channel.thread-panel.menu.copy-link"
						>
							{i18n._(COPY_LINK_DESCRIPTOR)}
						</MenuItem>
						<MenuItem
							hint={<span style={{fontSize: '10px', background: '#2b2d31', padding: '1px 4px', borderRadius: '3px', color: '#80848e'}}>ID</span>}
							onClick={() => {
								void navigator.clipboard.writeText(threadId);
								ToastCommands.createToast({type: 'success', children: i18n._(ID_COPIED_DESCRIPTOR)});
								closeMenu();
							}}
							data-flx="channel.thread-panel.menu.copy-id"
						>
							{i18n._(COPY_THREAD_ID_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				</>
			));
		},
		[thread, threadId, parentChannelId, guildId, channel, isJoined, canManage, onClose, i18n],
	);

	return (
		<div className={styles.panel} data-flx="channel.thread-panel.panel">
			<div className={styles.header} data-flx="channel.thread-panel.header">
				<ThreadIcon size={16} className={styles.threadIcon} aria-hidden="true" data-flx="channel.thread-panel.thread-icon" />
				<span className={styles.threadName} data-flx="channel.thread-panel.thread-name">
					{threadName}
				</span>
				<div className={styles.headerActions} data-flx="channel.thread-panel.header-actions">
					{channel && (
						<ChannelNotificationSettingsButton
							channel={channel}
							data-flx="channel.thread-panel.notification-settings-button"
						/>
					)}
					<ChannelHeaderIcon
						icon={DotsThreeIcon}
						label={i18n._(THREAD_OPTIONS_DESCRIPTOR)}
						onClick={handleOpenMenu}
						aria-haspopup="menu"
						data-flx="channel.thread-panel.options-button.click"
					/>
					<FocusRing data-flx="channel.thread-panel.focus-ring">
						<button
							type="button"
							className={styles.closeButton}
							aria-label={i18n._(CLOSE_DESCRIPTOR)}
							onClick={onClose}
							data-flx="channel.thread-panel.close-button.click"
						>
							<XIcon size={18} />
						</button>
					</FocusRing>
				</div>
			</div>
			{channel && (
				<div className={styles.content} data-flx="channel.thread-panel.content">
					{isLocked && (
						<div className={styles.lockedBanner} data-flx="channel.thread-panel.locked-banner">
							{i18n._(LOCKED_THREAD_DESCRIPTOR)}
						</div>
					)}
					<ChannelChatLayout
						channel={channel}
						messages={
							<Messages
								key={threadId}
								channel={channel}
								data-flx="channel.thread-panel.messages"
							/>
						}
						textarea={
							<ChannelTextarea
								channel={channel}
								data-flx="channel.thread-panel.channel-textarea"
							/>
						}
						data-flx="channel.thread-panel.channel-chat-layout"
					/>
				</div>
			)}
		</div>
	);
});
