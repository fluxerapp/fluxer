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

import {observer} from 'mobx-react-lite';
import type React from 'react';
import {Permissions} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import PermissionStore from '~/stores/PermissionStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import {
	ChannelNotificationSettingsMenuItem,
	CopyChannelIdMenuItem,
	CopyChannelLinkMenuItem,
	DeleteChannelMenuItem,
	EditChannelMenuItem,
	FavoriteChannelMenuItem,
	InvitePeopleToChannelMenuItem,
	MarkChannelAsReadMenuItem,
	MuteChannelMenuItem,
} from './items/ChannelMenuItems';
import {DebugChannelMenuItem} from './items/DebugMenuItems';
import {MenuGroup} from './MenuGroup';

interface ChannelContextMenuProps {
	channel: ChannelRecord;
	onClose: () => void;
}

export const ChannelContextMenu: React.FC<ChannelContextMenuProps> = observer(({channel, onClose}) => {
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		channelId: channel.id,
		guildId: channel.guildId,
	});
	const developerMode = UserSettingsStore.developerMode;

	return (
		<>
			<MenuGroup>
				<MarkChannelAsReadMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<FavoriteChannelMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<InvitePeopleToChannelMenuItem channel={channel} onClose={onClose} />
				<CopyChannelLinkMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<MuteChannelMenuItem channel={channel} onClose={onClose} />
				<ChannelNotificationSettingsMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			{canManageChannels && (
				<MenuGroup>
					<EditChannelMenuItem channel={channel} onClose={onClose} />
					<DeleteChannelMenuItem channel={channel} onClose={onClose} />
				</MenuGroup>
			)}

			{developerMode && (
				<MenuGroup>
					<DebugChannelMenuItem channel={channel} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<CopyChannelIdMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>
		</>
	);
});
