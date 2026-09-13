// SPDX-License-Identifier: AGPL-3.0-or-later

import * as LinkChannelCommands from '@app/features/channel/commands/LinkChannelCommands';
import {RenameChannelModal} from '@app/features/channel/components/modals/RenameChannelModal';
import {useDeleteMyMessagesInChannel} from '@app/features/channel/hooks/useDeleteMyMessagesInChannel';
import type {Channel} from '@app/features/channel/models/Channel';
import {DELETE_MY_MESSAGES_DESCRIPTOR} from '@app/features/channel/utils/ChannelMessageDescriptors';
import type {Guild} from '@app/features/guild/models/Guild';
import {
	CHANGE_NICKNAME_DESCRIPTOR,
	CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR,
	OPEN_LINK_DESCRIPTOR,
	REMOVE_FROM_FAVORITES_DESCRIPTOR,
	UNCATEGORIZED_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import Favorites, {type FavoriteChannel} from '@app/features/messaging/state/Favorites';
import {focusChannelTextareaAfterNavigation} from '@app/features/messaging/utils/ChannelTextareaFocusUtils';
import * as NavigationCommands from '@app/features/navigation/commands/NavigationCommands';
import Permission from '@app/features/permissions/state/Permission';
import {
	ChangeNicknameIcon,
	DeleteIcon,
	OpenInCommunityIcon,
	RemoveFromFavoritesIcon,
} from '@app/features/ui/action_menu/ContextMenuIcons';
import {
	ChannelNotificationSettingsMenuItem,
	CopyChannelIdMenuItem,
	DeleteChannelMenuItem,
	EditChannelMenuItem,
	InvitePeopleToChannelMenuItem,
	MarkChannelAsReadMenuItem,
	MuteChannelMenuItem,
} from '@app/features/ui/action_menu/items/ChannelMenuItems';
import {DebugChannelMenuItem} from '@app/features/ui/action_menu/items/DebugMenuItems';
import {MenuGroup} from '@app/features/ui/action_menu/MenuGroup';
import {MenuItem} from '@app/features/ui/action_menu/MenuItem';
import {MenuItemSubmenu} from '@app/features/ui/action_menu/MenuItemSubmenu';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import UserSettings from '@app/features/user/state/UserSettings';
import {ChannelTypes, Permissions} from '@fluxer/constants/src/ChannelConstants';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const OPEN_IN_COMMUNITY_DESCRIPTOR = msg({
	message: 'Open in community',
	comment: 'Favorites menu action that opens the favorited channel in the community it belongs to.',
});
const MOVE_TO_DESCRIPTOR = msg({
	message: 'Move to',
	comment: 'Favorites menu submenu label that moves the favorited channel into another favorites category.',
});

interface FavoritesChannelContextMenuProps {
	favoriteChannel: FavoriteChannel;
	channel: Channel | null;
	guild: Guild | null;
	onClose: () => void;
}

export const FavoritesChannelContextMenu: React.FC<FavoritesChannelContextMenuProps> = observer(
	({favoriteChannel, channel, guild: _guild, onClose}) => {
		const {i18n} = useLingui();
		const deleteMyMessagesInChannel = useDeleteMyMessagesInChannel();
		const handleSetNickname = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<RenameChannelModal
						currentName={favoriteChannel.nickname || channel?.name || ''}
						onSave={(nickname) => {
							Favorites.setChannelNickname(favoriteChannel.channelId, nickname || null);
						}}
						data-flx="ui.action-menu.favorites-channel-context-menu.handle-set-nickname.rename-channel-modal"
					/>
				)),
			);
		};
		const handleRemoveFromFavorites = () => {
			Favorites.removeChannel(favoriteChannel.channelId);
			ToastCommands.createToast({type: 'success', children: i18n._(CHANNEL_REMOVED_FROM_FAVORITES_DESCRIPTOR)});
			onClose();
		};
		const handleMoveTo = (categoryId: string | null) => {
			const currentChannel = Favorites.getChannel(favoriteChannel.channelId);
			if (!currentChannel) return;
			const channelsInTarget = Favorites.getChannelsInCategory(categoryId);
			const newPosition = channelsInTarget.length;
			Favorites.moveChannel(favoriteChannel.channelId, categoryId, newPosition);
			onClose();
		};
		const handleOpenInGuild = () => {
			if (!channel?.guildId) return;
			if (LinkChannelCommands.openLinkChannel(channel)) {
				onClose();
				return;
			}
			NavigationCommands.selectChannel(channel.guildId, channel.id);
			onClose();
			focusChannelTextareaAfterNavigation(channel.id);
		};
		const handleDeleteMyMessages = () => {
			if (!channel) return;
			onClose();
			deleteMyMessagesInChannel(channel.id);
		};
		if (!channel) {
			return (
				<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group">
					<MenuItem
						icon={<DeleteIcon data-flx="ui.action-menu.favorites-channel-context-menu.delete-icon" />}
						onClick={handleRemoveFromFavorites}
						danger
						data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.remove-from-favorites"
					>
						{i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			);
		}
		const canManageChannel =
			Boolean(channel.guildId) &&
			Permission.can(Permissions.MANAGE_CHANNELS, {channelId: channel.id, guildId: channel.guildId});
		const canUpdateRtcRegion =
			Boolean(channel.guildId) &&
			channel.type === ChannelTypes.GUILD_VOICE &&
			Permission.can(Permissions.UPDATE_RTC_REGION, {channelId: channel.id, guildId: channel.guildId});
		const canEditChannel = canManageChannel || canUpdateRtcRegion;
		const developerMode = UserSettings.developerMode;
		return (
			<>
				{channel.guildId && (
					<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--2">
						<MarkChannelAsReadMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.mark-channel-as-read-menu-item"
						/>
						<InvitePeopleToChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.invite-people-to-channel-menu-item"
						/>
					</MenuGroup>
				)}
				<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--3">
					<MenuItem
						icon={<ChangeNicknameIcon data-flx="ui.action-menu.favorites-channel-context-menu.change-nickname-icon" />}
						onClick={handleSetNickname}
						data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.set-nickname"
					>
						{i18n._(CHANGE_NICKNAME_DESCRIPTOR)}
					</MenuItem>
					{channel.guildId && (
						<MenuItem
							icon={
								<OpenInCommunityIcon data-flx="ui.action-menu.favorites-channel-context-menu.open-in-community-icon" />
							}
							onClick={handleOpenInGuild}
							data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.open-in-guild"
						>
							{channel.type === ChannelTypes.GUILD_LINK
								? i18n._(OPEN_LINK_DESCRIPTOR)
								: i18n._(OPEN_IN_COMMUNITY_DESCRIPTOR)}
						</MenuItem>
					)}
					{(favoriteChannel.parentId !== null ||
						Favorites.sortedCategories.some((category) => category.id !== favoriteChannel.parentId)) && (
						<MenuItemSubmenu
							label={i18n._(MOVE_TO_DESCRIPTOR)}
							render={() => (
								<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--4">
									{favoriteChannel.parentId !== null && (
										<MenuItem
											onClick={() => handleMoveTo(null)}
											data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.move-to"
										>
											{i18n._(UNCATEGORIZED_DESCRIPTOR)}
										</MenuItem>
									)}
									{Favorites.sortedCategories
										.filter((category) => category.id !== favoriteChannel.parentId)
										.map((category) => (
											<MenuItem
												key={category.id}
												onClick={() => handleMoveTo(category.id)}
												data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.move-to--2"
											>
												{category.name}
											</MenuItem>
										))}
								</MenuGroup>
							)}
							data-flx="ui.action-menu.favorites-channel-context-menu.menu-item-submenu"
						/>
					)}
				</MenuGroup>
				{channel.guildId && (
					<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--5">
						<MuteChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.mute-channel-menu-item"
						/>
						<ChannelNotificationSettingsMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.channel-notification-settings-menu-item"
						/>
					</MenuGroup>
				)}
				{canEditChannel && (
					<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--6">
						<EditChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.edit-channel-menu-item"
						/>
						{canManageChannel && (
							<DeleteChannelMenuItem
								channel={channel}
								onClose={onClose}
								data-flx="ui.action-menu.favorites-channel-context-menu.delete-channel-menu-item"
							/>
						)}
					</MenuGroup>
				)}
				{(channel.type === ChannelTypes.DM || channel.type === ChannelTypes.GROUP_DM) && (
					<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--7">
						<MenuItem
							icon={<DeleteIcon data-flx="ui.action-menu.favorites-channel-context-menu.delete-icon--2" />}
							onClick={handleDeleteMyMessages}
							danger
							data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.delete-my-messages"
						>
							{i18n._(DELETE_MY_MESSAGES_DESCRIPTOR)}
						</MenuItem>
					</MenuGroup>
				)}
				<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--8">
					{developerMode && (
						<DebugChannelMenuItem
							channel={channel}
							onClose={onClose}
							data-flx="ui.action-menu.favorites-channel-context-menu.debug-channel-menu-item"
						/>
					)}
					<CopyChannelIdMenuItem
						channel={channel}
						onClose={onClose}
						data-flx="ui.action-menu.favorites-channel-context-menu.copy-channel-id-menu-item"
					/>
				</MenuGroup>
				<MenuGroup data-flx="ui.action-menu.favorites-channel-context-menu.menu-group--9">
					<MenuItem
						icon={
							<RemoveFromFavoritesIcon data-flx="ui.action-menu.favorites-channel-context-menu.remove-from-favorites-icon" />
						}
						onClick={handleRemoveFromFavorites}
						danger
						data-flx="ui.action-menu.favorites-channel-context-menu.menu-item.remove-from-favorites--2"
					>
						{i18n._(REMOVE_FROM_FAVORITES_DESCRIPTOR)}
					</MenuItem>
				</MenuGroup>
			</>
		);
	},
);
