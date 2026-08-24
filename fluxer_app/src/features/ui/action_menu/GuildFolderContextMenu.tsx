// SPDX-License-Identifier: AGPL-3.0-or-later

import Channels from '@app/features/channel/state/Channels';
import {openGuildFolderSettingsModal} from '@app/features/guild/components/modals/GuildFolderSettingsModal';
import type {Guild} from '@app/features/guild/models/Guild';
import GuildReadState from '@app/features/guild/state/GuildReadState';
import {FOLDER_SETTINGS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as ReadStateCommands from '@app/features/read_state/commands/ReadStateCommands';
import {MarkAsReadIcon, SettingsIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {BulkGuildSettingsMenuItems} from '@app/features/ui/action_menu/items/BulkSettingsMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback} from 'react';

const MARK_FOLDER_AS_READ_DESCRIPTOR = msg({
	message: 'Mark folder as read',
	comment: 'Action that marks every channel inside the folder as read.',
});

interface GuildFolder {
	id: number | null;
	name: string | null;
	color: number | null;
	guildIds: Array<string>;
}

interface GuildFolderContextMenuProps {
	folder: GuildFolder;
	guilds: Array<Guild>;
	onClose: () => void;
}

function guildHasUnreadState(guildId: string): boolean {
	return GuildReadState.guildIsUnread(guildId) || GuildReadState.guildUnreadIgnoringMute(guildId);
}

export const GuildFolderContextMenu: React.FC<GuildFolderContextMenuProps> = observer(({folder, guilds, onClose}) => {
	const {i18n} = useLingui();
	const hasUnreads = guilds.some((guild) => guildHasUnreadState(guild.id));
	const handleMarkFolderAsRead = useCallback(() => {
		const channelIds: Array<string> = [];
		for (const guild of guilds) {
			if (!guildHasUnreadState(guild.id)) {
				continue;
			}
			const channels = Channels.getGuildChannels(guild.id);
			for (const channel of channels) {
				channelIds.push(channel.id);
			}
		}
		if (channelIds.length > 0) {
			void ReadStateCommands.bulkAckChannels(channelIds);
		}
		onClose();
	}, [guilds, onClose]);
	const handleFolderSettings = useCallback(() => {
		if (folder.id != null) {
			openGuildFolderSettingsModal(folder.id);
		}
		onClose();
	}, [folder.id, onClose]);
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.guild-folder-context-menu.menu-group">
				<MenuItem
					icon={<MarkAsReadIcon data-flx="ui.action-menu.guild-folder-context-menu.mark-as-read-icon" />}
					onClick={handleMarkFolderAsRead}
					disabled={!hasUnreads}
					data-flx="ui.action-menu.guild-folder-context-menu.menu-item.mark-folder-as-read"
				>
					{i18n._(MARK_FOLDER_AS_READ_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
			<MenuGroup data-flx="ui.action-menu.guild-folder-context-menu.menu-group--2">
				<BulkGuildSettingsMenuItems
					guilds={guilds}
					onClose={onClose}
					data-flx="ui.action-menu.guild-folder-context-menu.bulk-guild-settings-menu-items"
				/>
			</MenuGroup>
			<MenuGroup data-flx="ui.action-menu.guild-folder-context-menu.menu-group--3">
				<MenuItem
					icon={<SettingsIcon data-flx="ui.action-menu.guild-folder-context-menu.settings-icon" />}
					onClick={handleFolderSettings}
					data-flx="ui.action-menu.guild-folder-context-menu.menu-item.folder-settings"
				>
					{i18n._(FOLDER_SETTINGS_DESCRIPTOR)}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
