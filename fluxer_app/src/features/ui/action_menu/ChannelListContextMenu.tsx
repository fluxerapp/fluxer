// SPDX-License-Identifier: AGPL-3.0-or-later

import {CategoryCreateModal} from '@app/features/channel/components/modals/CategoryCreateModal';
import {ChannelCreateModal} from '@app/features/channel/components/modals/ChannelCreateModal';
import type {Guild} from '@app/features/guild/models/Guild';
import {
	CREATE_CATEGORY_DESCRIPTOR,
	CREATE_CHANNEL_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	INVITE_PEOPLE_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Permission from '@app/features/permissions/state/Permission';
import {CheckboxItem} from '@app/features/ui/action_menu/ContextMenu';
import {CreateCategoryIcon, CreateIcon, InviteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

interface ChannelListContextMenuProps {
	guild: Guild;
	onClose: () => void;
}

export const ChannelListContextMenu: React.FC<ChannelListContextMenuProps> = observer(({guild, onClose}) => {
	const {i18n} = useLingui();
	const canManageChannels = Permission.can(Permissions.MANAGE_CHANNELS, {
		guildId: guild.id,
	});
	const invitableChannelId = InviteUtils.getInvitableChannelId(guild.id);
	const canInvite = InviteUtils.canInviteToChannel(invitableChannelId, guild.id);
	const hideMutedChannels = UserGuildSettings.getSettingsForScope(guild.id)?.hide_muted_channels ?? false;
	const handleToggleHideMutedChannels = useCallback(() => {
		UserGuildSettingsCommands.toggleHideMutedChannels(guild.id);
	}, [guild.id]);
	const handleCreateChannel = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<ChannelCreateModal
					guildId={guild.id}
					data-flx="ui.action-menu.channel-list-context-menu.handle-create-channel.channel-create-modal"
				/>
			)),
		);
	}, [guild.id, onClose]);
	const handleCreateCategory = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<CategoryCreateModal
					guildId={guild.id}
					data-flx="ui.action-menu.channel-list-context-menu.handle-create-category.category-create-modal"
				/>
			)),
		);
	}, [guild.id, onClose]);
	const handleInvitePeople = useCallback(() => {
		ModalCommands.pushAfterBottomSheetClose(
			onClose,
			modal(() => (
				<InviteModal
					channelId={invitableChannelId ?? ''}
					data-flx="ui.action-menu.channel-list-context-menu.handle-invite-people.invite-modal"
				/>
			)),
		);
	}, [invitableChannelId, onClose]);
	return (
		<>
			<MenuGroup data-flx="ui.action-menu.channel-list-context-menu.menu-group">
				<CheckboxItem
					checked={hideMutedChannels}
					onCheckedChange={handleToggleHideMutedChannels}
					data-flx="ui.action-menu.channel-list-context-menu.checkbox-item"
				>
					{i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR)}
				</CheckboxItem>
			</MenuGroup>
			{canManageChannels && (
				<MenuGroup data-flx="ui.action-menu.channel-list-context-menu.menu-group--2">
					<MenuItem
						icon={<CreateIcon data-flx="ui.action-menu.channel-list-context-menu.create-icon" />}
						onClick={handleCreateChannel}
						data-flx="ui.action-menu.channel-list-context-menu.menu-item.create-channel"
					>
						{i18n._(CREATE_CHANNEL_DESCRIPTOR)}
					</MenuItem>
					<MenuItem
						icon={<CreateCategoryIcon data-flx="ui.action-menu.channel-list-context-menu.create-category-icon" />}
						onClick={handleCreateCategory}
						data-flx="ui.action-menu.channel-list-context-menu.menu-item.create-category"
					>
						{i18n._(CREATE_CATEGORY_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			)}
			{canInvite && (
				<MenuGroup data-flx="ui.action-menu.channel-list-context-menu.menu-group--3">
					<MenuItem
						icon={<InviteIcon data-flx="ui.action-menu.channel-list-context-menu.invite-icon" />}
						onClick={handleInvitePeople}
						data-flx="ui.action-menu.channel-list-context-menu.menu-item.invite-people"
					>
						{i18n._(INVITE_PEOPLE_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			)}
		</>
	);
});
