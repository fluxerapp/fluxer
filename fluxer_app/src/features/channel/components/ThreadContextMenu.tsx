// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import * as ThreadCommands from '@app/features/channel/commands/ThreadCommands';
import type {Thread} from '@app/features/channel/state/Threads';
import Threads from '@app/features/channel/state/Threads';
import {ChannelNotificationSettingsDropdown} from '@app/features/channel/components/channel_header_components/ChannelNotificationSettingsDropdown';
import {ThreadSettingsModal} from '@app/features/channel/components/modals/ThreadSettingsModal';
import Permission from '@app/features/permissions/state/Permission';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import type {I18n} from '@lingui/core';
import type React from 'react';

const MARK_AS_READ_DESCRIPTOR = msg({message: 'Mark as Read', comment: 'Context menu item.'});
const LEAVE_THREAD_DESCRIPTOR = msg({message: 'Leave Thread', comment: 'Context menu item.'});
const CLOSE_THREAD_DESCRIPTOR = msg({message: 'Close Thread', comment: 'Context menu item.'});
const OPEN_THREAD_DESCRIPTOR = msg({message: 'Open Thread', comment: 'Context menu item.'});
const LOCK_THREAD_DESCRIPTOR = msg({message: 'Lock Thread', comment: 'Context menu item.'});
const UNLOCK_THREAD_DESCRIPTOR = msg({message: 'Unlock Thread', comment: 'Context menu item.'});
const EDIT_THREAD_DESCRIPTOR = msg({message: 'Edit Thread', comment: 'Context menu item.'});
const COPY_LINK_DESCRIPTOR = msg({message: 'Copy Link', comment: 'Context menu item.'});
const MUTE_THREAD_DESCRIPTOR = msg({message: 'Mute Thread', comment: 'Context menu item.'});
const NOTIFICATION_SETTINGS_DESCRIPTOR = msg({message: 'Notification Settings', comment: 'Context menu item.'});
const DELETE_THREAD_DESCRIPTOR = msg({message: 'Delete Thread', comment: 'Context menu item.'});
const COPY_THREAD_ID_DESCRIPTOR = msg({message: 'Copy Thread ID', comment: 'Context menu item.'});
const THREAD_CLOSED_DESCRIPTOR = msg({message: 'Thread closed', comment: 'Toast.'});
const THREAD_OPENED_DESCRIPTOR = msg({message: 'Thread opened', comment: 'Toast.'});
const LINK_COPIED_DESCRIPTOR = msg({message: 'Link copied', comment: 'Toast.'});
const ID_COPIED_DESCRIPTOR = msg({message: 'ID copied', comment: 'Toast.'});

export function openThreadContextMenu(
	event: React.MouseEvent,
	{
		thread,
		guildId,
		i18n,
		onClose,
		extraGroups,
	}: {
		thread: Thread;
		guildId: string;
		i18n: I18n;
		onClose?: () => void;
		extraGroups?: React.ReactNode;
	},
) {
	const threadId = thread.id;
	const parentChannelId = thread.threadParentChannelId;
	const isJoined = Threads.isJoined(threadId);
	const canManage = Permission.can(Permissions.MANAGE_THREADS, thread.toChannel());
	const channel = thread.toChannel();

	ContextMenuCommands.openFromEvent(event, ({onClose: closeMenu}) => {
		const close = () => {
			closeMenu();
			onClose?.();
		};

		return (
			<>
				<MenuGroup data-flx="channel.thread-context-menu.group-top">
					<MenuItem
						onClick={() => close()}
						data-flx="channel.thread-context-menu.mark-read"
					>
						{i18n._(MARK_AS_READ_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
				<MenuGroup data-flx="channel.thread-context-menu.group-actions">
					{isJoined && (
						<MenuItem
							onClick={() => {
								void ThreadCommands.leave(parentChannelId, threadId);
								close();
							}}
							data-flx="channel.thread-context-menu.leave"
						>
							{i18n._(LEAVE_THREAD_DESCRIPTOR)}
						</MenuItem>
					)}
					{canManage && thread.isOpen() && (
						<MenuItem
							onClick={async () => {
								await ThreadCommands.update(parentChannelId, threadId, {state: 1});
								ToastCommands.createToast({type: 'success', children: i18n._(THREAD_CLOSED_DESCRIPTOR)});
								close();
							}}
							data-flx="channel.thread-context-menu.close"
						>
							{i18n._(CLOSE_THREAD_DESCRIPTOR)}
						</MenuItem>
					)}
					{canManage && thread.isClosed() && (
						<MenuItem
							onClick={async () => {
								await ThreadCommands.update(parentChannelId, threadId, {state: 0});
								ToastCommands.createToast({type: 'success', children: i18n._(THREAD_OPENED_DESCRIPTOR)});
								close();
							}}
							data-flx="channel.thread-context-menu.open"
						>
							{i18n._(OPEN_THREAD_DESCRIPTOR)}
						</MenuItem>
					)}
					{canManage && (
						<MenuItem
							onClick={async () => {
								if (thread.isLocked()) {
									await ThreadCommands.update(parentChannelId, threadId, {locked: false, archived: false});
								} else {
									await ThreadCommands.update(parentChannelId, threadId, {locked: true, archived: true});
								}
								close();
							}}
							data-flx="channel.thread-context-menu.lock"
						>
							{thread.isLocked() ? i18n._(UNLOCK_THREAD_DESCRIPTOR) : i18n._(LOCK_THREAD_DESCRIPTOR)}
						</MenuItem>
					)}
					{canManage && (
						<MenuItem
							onClick={() => {
								ModalCommands.push(
									modal(() => (
										<ThreadSettingsModal
											threadId={threadId}
											parentChannelId={parentChannelId}
											data-flx="channel.thread-context-menu.settings-modal"
										/>
									)),
								);
								close();
							}}
							data-flx="channel.thread-context-menu.edit"
						>
							{i18n._(EDIT_THREAD_DESCRIPTOR)}
						</MenuItem>
					)}
					<MenuItem
						onClick={() => {
							const url = window.location.origin + Routes.guildThread(guildId, parentChannelId, threadId);
							void navigator.clipboard.writeText(url);
							ToastCommands.createToast({type: 'success', children: i18n._(LINK_COPIED_DESCRIPTOR)});
							close();
						}}
						data-flx="channel.thread-context-menu.copy-link"
					>
						{i18n._(COPY_LINK_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
				<MenuGroup data-flx="channel.thread-context-menu.group-notifications">
					<MenuItemSubmenu
						label={i18n._(MUTE_THREAD_DESCRIPTOR)}
						render={() => (
							<ChannelNotificationSettingsDropdown
								channel={channel}
								onClose={closeMenu}
								data-flx="channel.thread-context-menu.mute-dropdown"
							/>
						)}
						data-flx="channel.thread-context-menu.mute"
					/>
					<MenuItemSubmenu
						label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
						render={() => (
							<ChannelNotificationSettingsDropdown
								channel={channel}
								onClose={closeMenu}
								data-flx="channel.thread-context-menu.notif-dropdown"
							/>
						)}
						data-flx="channel.thread-context-menu.notif-settings"
					/>
				</MenuGroup>
				{canManage && (
					<MenuGroup data-flx="channel.thread-context-menu.group-danger">
						<MenuItem
							danger
							onClick={() => {
								void ThreadCommands.remove(parentChannelId, threadId);
								close();
							}}
							data-flx="channel.thread-context-menu.delete"
						>
							{i18n._(DELETE_THREAD_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				<MenuGroup data-flx="channel.thread-context-menu.group-copy">
					<MenuItem
						onClick={() => {
							void navigator.clipboard.writeText(threadId);
							ToastCommands.createToast({type: 'success', children: i18n._(ID_COPIED_DESCRIPTOR)});
							close();
						}}
						data-flx="channel.thread-context-menu.copy-id"
					>
						{i18n._(COPY_THREAD_ID_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
				{extraGroups}
			</>
		);
	});
}
