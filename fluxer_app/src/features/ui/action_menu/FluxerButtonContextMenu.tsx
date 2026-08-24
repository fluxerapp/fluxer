// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import Guilds from '@app/features/guild/state/Guilds';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import ReadStates from '@app/features/read_state/state/ReadStates';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {HideIcon, MarkAsReadIcon, ViewDetailsIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {
	BulkDMSettingsMenuItems,
	BulkGuildSettingsMenuItems,
} from '@app/features/ui/action_menu/items/BulkSettingsMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import SidebarPreferences from '@app/features/ui/state/SidebarPreferences';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const MARK_ALL_DMS_AS_READ_DESCRIPTOR = msg({
	message: 'Mark all DMs as read',
	comment: 'Action that marks every direct message conversation as read.',
});
const SHOW_INLINE_DM_LIST_DESCRIPTOR = msg({
	message: 'Show inline DM list',
	comment: 'Setting label that shows the inline DM list in the sidebar.',
});
const COLLAPSE_INLINE_DM_LIST_DESCRIPTOR = msg({
	message: 'Collapse inline DM list',
	comment: 'Setting label that collapses the inline DM list.',
});
const SHOW_UNREAD_DM_COUNT_WHEN_COLLAPSED_DESCRIPTOR = msg({
	message: 'Show unread DM count when collapsed',
	comment: 'Setting label that shows an unread badge while the inline DM list is collapsed.',
});
const SHOW_INCOMING_FRIEND_REQUEST_COUNT_DESCRIPTOR = msg({
	message: 'Show incoming friend request count',
	comment: 'Setting label that shows a badge for incoming friend requests.',
});
const MARK_ALL_COMMUNITIES_AS_READ_DESCRIPTOR = msg({
	message: 'Mark all communities as read',
	comment: 'Action that marks every community as read.',
});

interface FluxerButtonContextMenuProps {
	onClose: () => void;
}

export const FluxerButtonContextMenu: React.FC<FluxerButtonContextMenuProps> = observer(({onClose}) => {
	const {i18n} = useLingui();
	const inlineDmsCollapsed = SidebarPreferences.inlineDmsCollapsed;
	const showCollapsedUnreadDmsBadge = SidebarPreferences.showCollapsedUnreadDmsBadge;
	const showIncomingFriendRequestBadge = SidebarPreferences.showIncomingFriendRequestBadge;
	const guilds = Guilds.getGuilds();
	const dmChannels = Channels.dmChannels;
	const unreadDmIds = dmChannels.filter((channel) => ReadStates.hasUnread(channel.id)).map((channel) => channel.id);
	const unreadGuildChannelIds: Array<string> = [];
	for (const guild of guilds) {
		for (const channel of Channels.getGuildChannels(guild.id)) {
			if (ReadStates.isUnreadOrMentioned(channel.id)) {
				unreadGuildChannelIds.push(channel.id);
			}
		}
	}
	const handleMarkAllDmsRead = useCallback(() => {
		if (unreadDmIds.length > 0) {
			void ReadStateCommands.bulkAckChannels(unreadDmIds);
		}
		onClose();
	}, [unreadDmIds, onClose]);
	const handleToggleInlineDms = useCallback(() => {
		SidebarPreferences.toggleInlineDmsCollapsed();
		onClose();
	}, [onClose]);
	const handleToggleCollapsedUnreadDmsBadge = useCallback((checked: boolean) => {
		SidebarPreferences.setShowCollapsedUnreadDmsBadge(checked);
	}, []);
	const handleToggleIncomingFriendRequestBadge = useCallback((checked: boolean) => {
		SidebarPreferences.setShowIncomingFriendRequestBadge(checked);
	}, []);
	const handleMarkAllCommunitiesRead = useCallback(() => {
		if (unreadGuildChannelIds.length > 0) {
			void ReadStateCommands.bulkAckChannels(unreadGuildChannelIds);
		}
		onClose();
	}, [unreadGuildChannelIds, onClose]);
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.fluxer-button-context-menu.menu-group">
				<MenuItem
					icon={<MarkAsReadIcon data-flx="ui.action-menu.fluxer-button-context-menu.mark-as-read-icon" />}
					onClick={handleMarkAllDmsRead}
					disabled={unreadDmIds.length === 0}
					data-flx="ui.action-menu.fluxer-button-context-menu.menu-item.mark-all-dms-read"
				>
					{i18n._(MARK_ALL_DMS_AS_READ_DESCRIPTOR)}
				</MenuItem>
				<MenuItem
					icon={
						inlineDmsCollapsed ? (
							<ViewDetailsIcon data-flx="ui.action-menu.fluxer-button-context-menu.view-details-icon" />
						) : (
							<HideIcon data-flx="ui.action-menu.fluxer-button-context-menu.hide-icon" />
						)
					}
					onClick={handleToggleInlineDms}
					data-flx="ui.action-menu.fluxer-button-context-menu.menu-item.toggle-inline-dms"
				>
					{inlineDmsCollapsed ? i18n._(SHOW_INLINE_DM_LIST_DESCRIPTOR) : i18n._(COLLAPSE_INLINE_DM_LIST_DESCRIPTOR)}
				</MenuItem>
				<CheckboxItem
					checked={showCollapsedUnreadDmsBadge}
					onCheckedChange={handleToggleCollapsedUnreadDmsBadge}
					data-flx="ui.action-menu.fluxer-button-context-menu.checkbox-item"
				>
					{i18n._(SHOW_UNREAD_DM_COUNT_WHEN_COLLAPSED_DESCRIPTOR)}
				</CheckboxItem>
				<CheckboxItem
					checked={showIncomingFriendRequestBadge}
					onCheckedChange={handleToggleIncomingFriendRequestBadge}
					data-flx="ui.action-menu.fluxer-button-context-menu.checkbox-item--2"
				>
					{i18n._(SHOW_INCOMING_FRIEND_REQUEST_COUNT_DESCRIPTOR)}
				</CheckboxItem>
				<MenuItem
					icon={<MarkAsReadIcon data-flx="ui.action-menu.fluxer-button-context-menu.mark-as-read-icon--2" />}
					onClick={handleMarkAllCommunitiesRead}
					disabled={unreadGuildChannelIds.length === 0}
					data-flx="ui.action-menu.fluxer-button-context-menu.menu-item.mark-all-communities-read"
				>
					{i18n._(MARK_ALL_COMMUNITIES_AS_READ_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="ui.action-menu.fluxer-button-context-menu.menu-group--2">
				<BulkGuildSettingsMenuItems
					guilds={guilds}
					onClose={onClose}
					data-flx="ui.action-menu.fluxer-button-context-menu.bulk-guild-settings-menu-items"
				/>
			</MenuGroup>
			<MenuGroup data-flx="ui.action-menu.fluxer-button-context-menu.menu-group--3">
				<BulkDMSettingsMenuItems
					channels={dmChannels}
					onClose={onClose}
					data-flx="ui.action-menu.fluxer-button-context-menu.bulk-dm-settings-menu-items"
				/>
			</MenuGroup>
		</>
	);
});
