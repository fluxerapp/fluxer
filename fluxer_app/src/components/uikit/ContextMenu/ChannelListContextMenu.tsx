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

import {useLingui} from '@lingui/react/macro';
import {FolderPlusIcon, PlusIcon, UserPlusIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {Permissions} from '~/Constants';
import {CategoryCreateModal} from '~/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '~/components/modals/ChannelCreateModal';
import {InviteModal} from '~/components/modals/InviteModal';
import type {GuildRecord} from '~/records/GuildRecord';
import PermissionStore from '~/stores/PermissionStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import * as InviteUtils from '~/utils/InviteUtils';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';
import {MenuItemCheckbox} from './MenuItemCheckbox';

interface ChannelListContextMenuProps {
	guild: GuildRecord;
	onClose: () => void;
}

export const ChannelListContextMenu: React.FC<ChannelListContextMenuProps> = observer(({guild, onClose}) => {
	const {t} = useLingui();
	const canManageChannels = PermissionStore.can(Permissions.MANAGE_CHANNELS, {
		guildId: guild.id,
	});

	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);

	const hideMutedChannels = UserGuildSettingsStore.getSettings(guild.id)?.hide_muted_channels ?? false;

	const handleToggleHideMutedChannels = React.useCallback(() => {
		UserGuildSettingsActionCreators.toggleHideMutedChannels(guild.id);
	}, [guild.id]);

	const handleCreateChannel = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <ChannelCreateModal guildId={guild.id} />));
	}, [guild.id, onClose]);

	const handleCreateCategory = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <CategoryCreateModal guildId={guild.id} />));
	}, [guild.id, onClose]);

	const handleInvitePeople = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <InviteModal channelId={invitableChannelId ?? ''} />));
	}, [invitableChannelId, onClose]);

	return (
		<>
			<MenuGroup>
				<MenuItemCheckbox checked={hideMutedChannels} onChange={handleToggleHideMutedChannels}>
					{t`Hide Muted Channels`}
				</MenuItemCheckbox>
			</MenuGroup>

			{canManageChannels && (
				<MenuGroup>
					<MenuItem icon={<PlusIcon style={{width: 16, height: 16}} />} onClick={handleCreateChannel}>
						{t`Create Channel`}
					</MenuItem>
					<MenuItem icon={<FolderPlusIcon style={{width: 16, height: 16}} />} onClick={handleCreateCategory}>
						{t`Create Category`}
					</MenuItem>
				</MenuGroup>
			)}

			{canInvite && (
				<MenuGroup>
					<MenuItem icon={<UserPlusIcon style={{width: 16, height: 16}} />} onClick={handleInvitePeople}>
						{t`Invite People`}
					</MenuItem>
				</MenuGroup>
			)}
		</>
	);
});
