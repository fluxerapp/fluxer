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
import type {GuildRecord} from '~/records/GuildRecord';
import AuthenticationStore from '~/stores/AuthenticationStore';
import PermissionStore from '~/stores/PermissionStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import * as InviteUtils from '~/utils/InviteUtils';
import {DebugGuildMenuItem} from './items/DebugMenuItems';
import {
	CommunitySettingsMenuItem,
	CopyGuildIdMenuItem,
	CreateCategoryMenuItem,
	CreateChannelMenuItem,
	EditCommunityProfileMenuItem,
	HideMutedChannelsMenuItem,
	InvitePeopleMenuItem,
	LeaveCommunityMenuItem,
	MarkAsReadMenuItem,
	MuteCommunityMenuItem,
	NotificationSettingsMenuItem,
	PrivacySettingsMenuItem,
} from './items/GuildMenuItems';
import {ReportGuildMenuItem} from './items/ReportGuildMenuItem';
import {MenuGroup} from './MenuGroup';

interface GuildContextMenuProps {
	guild: GuildRecord;
	onClose: () => void;
}

export const GuildContextMenu: React.FC<GuildContextMenuProps> = observer(({guild, onClose}) => {
	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {guildId: guild.id});
	const developerMode = UserSettingsStore.developerMode;

	return (
		<>
			<MenuGroup>
				<MarkAsReadMenuItem guild={guild} onClose={onClose} />
			</MenuGroup>

			{canInvite && (
				<MenuGroup>
					<InvitePeopleMenuItem guild={guild} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<MuteCommunityMenuItem guild={guild} onClose={onClose} />
				<NotificationSettingsMenuItem guild={guild} onClose={onClose} />
				<HideMutedChannelsMenuItem guild={guild} onClose={onClose} />
			</MenuGroup>

			<MenuGroup>
				<CommunitySettingsMenuItem guild={guild} onClose={onClose} />
				<PrivacySettingsMenuItem guild={guild} onClose={onClose} />
				<EditCommunityProfileMenuItem guild={guild} onClose={onClose} />
			</MenuGroup>

			{canManageChannels && (
				<MenuGroup>
					<CreateChannelMenuItem guild={guild} onClose={onClose} />
					<CreateCategoryMenuItem guild={guild} onClose={onClose} />
				</MenuGroup>
			)}

			{!guild.isOwner(AuthenticationStore.currentUserId) && (
				<MenuGroup>
					<LeaveCommunityMenuItem guild={guild} onClose={onClose} />
					<ReportGuildMenuItem guild={guild} onClose={onClose} />
				</MenuGroup>
			)}

			{developerMode && (
				<MenuGroup>
					<DebugGuildMenuItem guild={guild} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				<CopyGuildIdMenuItem guild={guild} onClose={onClose} />
			</MenuGroup>
		</>
	);
});
