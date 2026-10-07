// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import type {Channel} from '@app/features/channel/models/Channel';
import {MARK_AS_READ_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {buildChannelLink} from '@app/features/messaging/utils/MessageLinkUtils';
import Navigation from '@app/features/navigation/state/Navigation';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {openThread, openThreadFullView} from '@app/features/threads/commands/ThreadNavigation';
import {ThreadSettingsModal} from '@app/features/threads/components/ThreadSettingsModal';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import {
	canDeleteThreadChannel,
	canJoinThreadChannel,
	canLeaveThreadChannel,
	canPatchThread,
	canUnarchiveThread,
	isThreadModeratorFor,
} from '@app/features/threads/utils/ThreadActionRules';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {
	getThreadNotificationFlag,
	getThreadNotificationSetting,
	ThreadNotificationSetting,
} from '@app/features/threads/utils/ThreadNotificationUtils';
import {
	CopyIdIcon,
	CopyLinkIcon,
	DeleteIcon,
	EditIcon,
	LeaveIcon,
	MarkAsReadIcon,
	MuteIcon,
	NotificationSettingsIcon,
	OpenLinkIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import type {MenuGroupType, MenuSheetItem} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import UserSettings from '@app/features/user/state/UserSettings';
import * as FormUtils from '@app/lib/forms';
import type {I18n} from '@lingui/core';
import {useLingui} from '@lingui/react/macro';
import {ArchiveIcon, ArrowsOutSimpleIcon, LockSimpleIcon, LockSimpleOpenIcon, SignInIcon} from '@phosphor-icons/react';

export function reportThreadActionError(i18n: I18n, error: unknown): void {
	ToastCommands.createToast({
		type: 'error',
		children: i18n._(D.THREAD_ACTION_FAILED_DESCRIPTOR, {detail: FormUtils.extractErrorMessage(i18n, error)}),
	});
}

function run(i18n: I18n, action: () => Promise<void>): void {
	void action().catch((error) => reportThreadActionError(i18n, error));
}

export function openThreadSettings(thread: Channel): void {
	ModalCommands.push(
		modal(() => (
			<ThreadSettingsModal threadId={thread.id} data-flx="threads.use-thread-menu-data.thread-settings-modal" />
		)),
	);
}

export function confirmDeleteThread(i18n: I18n, thread: Channel): void {
	ModalCommands.push(
		modal(() => (
			<ConfirmModal
				title={i18n._(D.DELETE_THREAD_DESCRIPTOR)}
				description={i18n._(D.DELETE_THREAD_CONFIRM_DESCRIPTOR, {threadName: thread.name ?? ''})}
				primaryText={i18n._(D.DELETE_THREAD_DESCRIPTOR)}
				primaryVariant="danger"
				onPrimary={async () => {
					try {
						await ThreadCommands.deleteThread(thread);
						ToastCommands.createToast({type: 'success', children: i18n._(D.THREAD_DELETED_DESCRIPTOR)});
					} catch (error) {
						reportThreadActionError(i18n, error);
					}
				}}
				data-flx="threads.use-thread-menu-data.confirm-delete-thread"
			/>
		)),
	);
}

export function buildThreadNotificationItems(i18n: I18n, thread: Channel): Array<MenuSheetItem> {
	const current = getThreadNotificationSetting(thread.id);
	const options: Array<[ThreadNotificationSetting, string]> = [
		[ThreadNotificationSetting.DEFAULT, i18n._(D.NOTIFICATION_DEFAULT_DESCRIPTOR)],
		[ThreadNotificationSetting.ALL_MESSAGES, i18n._(D.NOTIFICATION_ALL_DESCRIPTOR)],
		[ThreadNotificationSetting.ONLY_MENTIONS, i18n._(D.NOTIFICATION_MENTIONS_DESCRIPTOR)],
		[ThreadNotificationSetting.NO_MESSAGES, i18n._(D.NOTIFICATION_NOTHING_DESCRIPTOR)],
	];
	return options.map(([setting, label]) => ({
		label,
		selected: current === setting,
		onSelect: () =>
			run(i18n, () => ThreadCommands.updateThreadMemberSettings(thread, {flags: getThreadNotificationFlag(setting)})),
	}));
}

export function useThreadMenuData(thread: Channel, {onClose}: {onClose: () => void}): {groups: Array<MenuGroupType>} {
	const {i18n} = useLingui();
	const joined = ThreadMemberships.isMember(thread.id);
	const muted = ThreadMemberships.isMuted(thread.id);
	const moderator = isThreadModeratorFor(thread);
	const archived = thread.isArchived;
	const locked = thread.isLocked;
	const groups: Array<MenuGroupType> = [];
	const readItems: Array<MenuSheetItem> = [];
	if (ReadStates.hasUnread(thread.id) || ReadStates.getMentionCount(thread.id) > 0) {
		readItems.push({
			icon: <MarkAsReadIcon size={20} data-flx="threads.use-thread-menu-data.mark-as-read-icon" />,
			label: i18n._(MARK_AS_READ_DESCRIPTOR),
			onClick: () => {
				ReadStateCommands.ack(thread.id, true, true);
				onClose();
			},
		});
	}
	if (readItems.length > 0) groups.push({items: readItems});
	const openItems: Array<MenuSheetItem> = [];
	if (Navigation.threadId !== thread.id && Navigation.channelId !== thread.id) {
		openItems.push({
			icon: <OpenLinkIcon size={20} data-flx="threads.use-thread-menu-data.open-thread-icon" />,
			label: i18n._(D.OPEN_THREAD_DESCRIPTOR),
			onClick: () => {
				openThread(thread);
				onClose();
			},
		});
	}
	if (Navigation.channelId !== thread.id) {
		openItems.push({
			icon: <ArrowsOutSimpleIcon size={20} data-flx="threads.use-thread-menu-data.full-view-icon" />,
			label: i18n._(D.OPEN_FULL_VIEW_DESCRIPTOR),
			onClick: () => {
				openThreadFullView(thread);
				onClose();
			},
		});
	}
	if (openItems.length > 0) groups.push({items: openItems});
	const behaviorItems: Array<MenuSheetItem> = [];
	if (joined) {
		behaviorItems.push({
			icon: <MuteIcon size={20} data-flx="threads.use-thread-menu-data.mute-icon" />,
			label: muted ? i18n._(D.UNMUTE_THREAD_DESCRIPTOR) : i18n._(D.MUTE_THREAD_DESCRIPTOR),
			onClick: () => {
				run(i18n, () =>
					ThreadCommands.updateThreadMemberSettings(thread, muted ? {muted: false} : {muted: true, mute_config: null}),
				);
				onClose();
			},
		});
		behaviorItems.push({
			icon: <NotificationSettingsIcon size={20} data-flx="threads.use-thread-menu-data.notification-settings-icon" />,
			label: i18n._(D.THREAD_NOTIFICATIONS_DESCRIPTOR),
			items: buildThreadNotificationItems(i18n, thread),
		});
	}
	if (behaviorItems.length > 0) groups.push({items: behaviorItems});
	const manageItems: Array<MenuSheetItem> = [];
	if (canPatchThread(thread, {name: thread.name})) {
		manageItems.push({
			icon: <EditIcon size={20} data-flx="threads.use-thread-menu-data.edit-icon" />,
			label: i18n._(D.EDIT_THREAD_DESCRIPTOR),
			onClick: () => {
				openThreadSettings(thread);
				onClose();
			},
		});
	}
	if (archived ? canUnarchiveThread(thread) : canPatchThread(thread, {archived: true})) {
		manageItems.push({
			icon: <ArchiveIcon size={20} data-flx="threads.use-thread-menu-data.archive-icon" />,
			label: archived ? i18n._(D.UNARCHIVE_THREAD_DESCRIPTOR) : i18n._(D.ARCHIVE_THREAD_DESCRIPTOR),
			onClick: () => {
				run(i18n, () => ThreadCommands.updateThread(thread, {archived: !archived}));
				onClose();
			},
		});
	}
	if (moderator) {
		manageItems.push({
			icon: locked ? (
				<LockSimpleOpenIcon size={20} data-flx="threads.use-thread-menu-data.unlock-icon" />
			) : (
				<LockSimpleIcon size={20} data-flx="threads.use-thread-menu-data.lock-icon" />
			),
			label: locked ? i18n._(D.UNLOCK_THREAD_DESCRIPTOR) : i18n._(D.LOCK_THREAD_DESCRIPTOR),
			onClick: () => {
				run(i18n, () =>
					ThreadCommands.updateThread(thread, archived ? {archived: false, locked: !locked} : {locked: !locked}),
				);
				onClose();
			},
		});
	}
	if (!joined && canJoinThreadChannel(thread)) {
		manageItems.push({
			icon: <SignInIcon size={20} data-flx="threads.use-thread-menu-data.join-icon" />,
			label: i18n._(D.JOIN_THREAD_DESCRIPTOR),
			onClick: () => {
				run(i18n, () => ThreadCommands.joinThread(thread));
				onClose();
			},
		});
	}
	if (joined && canLeaveThreadChannel(thread)) {
		manageItems.push({
			icon: <LeaveIcon size={20} data-flx="threads.use-thread-menu-data.leave-icon" />,
			label: i18n._(D.LEAVE_THREAD_DESCRIPTOR),
			danger: true,
			onClick: () => {
				run(i18n, () => ThreadCommands.leaveThread(thread));
				onClose();
			},
		});
	}
	if (canDeleteThreadChannel(thread)) {
		manageItems.push({
			icon: <DeleteIcon size={20} data-flx="threads.use-thread-menu-data.delete-icon" />,
			label: i18n._(D.DELETE_THREAD_DESCRIPTOR),
			danger: true,
			onClick: () => {
				onClose();
				confirmDeleteThread(i18n, thread);
			},
		});
	}
	if (manageItems.length > 0) groups.push({items: manageItems});
	const copyItems: Array<MenuSheetItem> = [
		{
			icon: <CopyLinkIcon size={20} data-flx="threads.use-thread-menu-data.copy-link-icon" />,
			label: i18n._(D.COPY_THREAD_LINK_DESCRIPTOR),
			onClick: () => {
				void TextCopyCommands.copy(i18n, buildChannelLink({guildId: thread.guildId, channelId: thread.id}), true);
				ToastCommands.createToast({type: 'success', children: i18n._(D.THREAD_LINK_COPIED_DESCRIPTOR)});
				onClose();
			},
		},
	];
	if (UserSettings.developerMode) {
		copyItems.push({
			icon: <CopyIdIcon size={20} data-flx="threads.use-thread-menu-data.copy-id-icon" />,
			label: i18n._(D.COPY_THREAD_ID_DESCRIPTOR),
			onClick: () => {
				void TextCopyCommands.copy(i18n, thread.id, true);
				onClose();
			},
		});
	}
	groups.push({items: copyItems});
	return {groups};
}
