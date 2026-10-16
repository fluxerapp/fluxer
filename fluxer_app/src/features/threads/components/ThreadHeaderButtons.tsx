// SPDX-License-Identifier: AGPL-3.0-or-later

import {ChannelHeaderIcon} from '@app/features/channel/components/channel_header_components/ChannelHeaderIcon';
import type {Channel} from '@app/features/channel/models/Channel';
import {NOTIFICATION_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ThreadCommands from '@app/features/threads/commands/ThreadCommands';
import {ThreadBrowser, ThreadBrowserCreateButton} from '@app/features/threads/components/ThreadBrowser';
import {buildThreadNotificationItems, reportThreadActionError} from '@app/features/threads/hooks/useThreadMenuData';
import ThreadGuilds from '@app/features/threads/state/ThreadGuilds';
import ThreadMemberships from '@app/features/threads/state/ThreadMemberships';
import * as D from '@app/features/threads/utils/ThreadMessageDescriptors';
import {DataMenuRenderer} from '@app/features/ui/action_menu/DataMenuRenderer';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ContextMenuCommands from '@app/features/ui/commands/ContextMenuCommands';
import {useContextMenuTrigger} from '@app/features/ui/hooks/useContextMenuTrigger';
import {usePopout} from '@app/features/ui/hooks/usePopout';
import {Popout} from '@app/features/ui/popover/PopoverPopout';
import MobileLayout from '@app/features/ui/state/MobileLayout';
import {TEXT_THREAD_PARENT_CHANNEL_TYPES} from '@fluxer/constants/src/ThreadConstants';
import {useLingui} from '@lingui/react/macro';
import {BellIcon, BellSlashIcon, ChatsIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

function isThreadBrowserParent(channel: Channel): boolean {
	return TEXT_THREAD_PARENT_CHANNEL_TYPES.has(channel.type);
}

export const ThreadsButton = observer(({channel}: {channel: Channel}) => {
	const {i18n} = useLingui();
	const {isOpen, openProps} = usePopout(`thread-browser-${channel.id}`);
	const [sheetOpen, setSheetOpen] = useState(false);
	if (!ThreadGuilds.isActive(channel.guildId) || !isThreadBrowserParent(channel)) return null;
	if (MobileLayout.enabled) {
		return (
			<>
				<ChannelHeaderIcon
					icon={ChatsIcon}
					label={i18n._(D.THREADS_DESCRIPTOR)}
					isSelected={sheetOpen}
					aria-haspopup="dialog"
					aria-expanded={sheetOpen}
					onClick={() => setSheetOpen(true)}
					data-flx="threads.threads-button.channel-header-icon--mobile"
				/>
				<BottomSheet
					isOpen={sheetOpen}
					onClose={() => setSheetOpen(false)}
					title={i18n._(D.THREADS_DESCRIPTOR)}
					snapPoints={[0, 1]}
					initialSnap={1}
					disablePadding
					leadingAction={
						<ThreadBrowserCreateButton
							parent={channel}
							onClose={() => setSheetOpen(false)}
							sheet
							data-flx="threads.threads-button.thread-browser-create-button"
						/>
					}
					data-flx="threads.threads-button.bottom-sheet"
				>
					<ThreadBrowser
						parent={channel}
						onClose={() => setSheetOpen(false)}
						sheet
						data-flx="threads.threads-button.thread-browser--sheet"
					/>
				</BottomSheet>
			</>
		);
	}
	return (
		<Popout
			{...openProps}
			position="bottom-end"
			render={({onClose}) => (
				<ThreadBrowser parent={channel} onClose={onClose} data-flx="threads.threads-button.thread-browser" />
			)}
			subscribeTo="THREAD_BROWSER_OPEN"
			data-flx="threads.threads-button.popout"
		>
			<ChannelHeaderIcon
				icon={ChatsIcon}
				label={i18n._(D.THREADS_DESCRIPTOR)}
				isSelected={isOpen}
				aria-haspopup={true}
				aria-expanded={isOpen}
				data-flx="threads.threads-button.channel-header-icon"
			/>
		</Popout>
	);
});

const ThreadNotificationDropdown = observer(({thread, onClose}: {thread: Channel; onClose: () => void}) => {
	const {i18n} = useLingui();
	const muted = ThreadMemberships.isMuted(thread.id);
	return (
		<DataMenuRenderer
			groups={[
				{
					items: [
						{
							label: muted ? i18n._(D.UNMUTE_THREAD_DESCRIPTOR) : i18n._(D.MUTE_THREAD_DESCRIPTOR),
							onClick: () => {
								onClose();
								void ThreadCommands.updateThreadMemberSettings(
									thread,
									muted ? {muted: false} : {muted: true, mute_config: null},
								).catch((error) => reportThreadActionError(i18n, error));
							},
						},
					],
				},
				{items: buildThreadNotificationItems(i18n, thread)},
			]}
			data-flx="threads.thread-notification-dropdown.data-menu-renderer"
		/>
	);
});

export const ThreadNotificationSettingsButton = observer(({thread}: {thread: Channel}) => {
	const {i18n} = useLingui();
	const {isOpen, withTracking} = useContextMenuTrigger();
	const muted = ThreadMemberships.isMuted(thread.id);
	const handleClick = useCallback(
		(event: React.MouseEvent<HTMLButtonElement>) => {
			event.preventDefault();
			event.stopPropagation();
			ContextMenuCommands.openFromElementBottomRight(
				event,
				({onClose}) => (
					<ThreadNotificationDropdown
						thread={thread}
						onClose={onClose}
						data-flx="threads.thread-notification-settings-button.thread-notification-dropdown"
					/>
				),
				withTracking(),
			);
		},
		[thread, withTracking],
	);
	if (!ThreadMemberships.isMember(thread.id)) return null;
	return (
		<ChannelHeaderIcon
			icon={muted ? BellSlashIcon : BellIcon}
			label={i18n._(NOTIFICATION_SETTINGS_DESCRIPTOR)}
			isSelected={isOpen || muted}
			aria-haspopup="menu"
			aria-expanded={isOpen}
			onClick={handleClick}
			data-flx="threads.thread-notification-settings-button.channel-header-icon"
		/>
	);
});
