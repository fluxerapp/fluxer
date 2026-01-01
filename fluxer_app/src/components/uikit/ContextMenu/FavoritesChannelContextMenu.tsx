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
import {ArrowSquareOutIcon, ArrowsOutCardinalIcon, PencilSimpleIcon, StarIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {Permissions} from '~/Constants';
import {RenameChannelModal} from '~/components/modals/RenameChannelModal';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {GuildRecord} from '~/records/GuildRecord';
import FavoritesStore, {type FavoriteChannel} from '~/stores/FavoritesStore';
import PermissionStore from '~/stores/PermissionStore';
import * as RouterUtils from '~/utils/RouterUtils';
import {
	ChannelNotificationSettingsMenuItem,
	EditChannelMenuItem,
	InvitePeopleToChannelMenuItem,
	MarkChannelAsReadMenuItem,
	MuteChannelMenuItem,
} from './items/ChannelMenuItems';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';
import {MenuItemSubmenu} from './MenuItemSubmenu';

interface FavoritesChannelContextMenuProps {
	favoriteChannel: FavoriteChannel;
	channel: ChannelRecord | null;
	guild: GuildRecord | null;
	onClose: () => void;
}

export const FavoritesChannelContextMenu: React.FC<FavoritesChannelContextMenuProps> = observer(
	({favoriteChannel, channel, guild: _guild, onClose}) => {
		const {t} = useLingui();

		const handleSetNickname = () => {
			onClose();
			ModalActionCreators.push(
				modal(() => (
					<RenameChannelModal
						currentName={favoriteChannel.nickname || channel?.name || ''}
						onSave={(nickname) => {
							FavoritesStore.setChannelNickname(favoriteChannel.channelId, nickname || null);
						}}
					/>
				)),
			);
		};

		const handleRemoveFromFavorites = () => {
			FavoritesStore.removeChannel(favoriteChannel.channelId);
			ToastActionCreators.createToast({type: 'success', children: t`Channel removed from favorites`});
			onClose();
		};

		const handleMoveTo = (categoryId: string | null) => {
			const currentChannel = FavoritesStore.getChannel(favoriteChannel.channelId);
			if (!currentChannel) return;

			const channelsInTarget = FavoritesStore.getChannelsInCategory(categoryId);
			const newPosition = channelsInTarget.length;

			FavoritesStore.moveChannel(favoriteChannel.channelId, categoryId, newPosition);
			onClose();
		};

		const handleOpenInGuild = () => {
			if (!channel?.guildId) return;
			RouterUtils.transitionTo(Routes.guildChannel(channel.guildId, channel.id));
			onClose();
		};

		if (!channel) {
			return (
				<MenuGroup>
					<MenuItem icon={<TrashIcon />} onClick={handleRemoveFromFavorites} danger>
						{t`Remove from Favorites`}
					</MenuItem>
				</MenuGroup>
			);
		}

		const canManageChannel =
			channel.guildId &&
			PermissionStore.can(Permissions.MANAGE_CHANNELS, {channelId: channel.id, guildId: channel.guildId});

		return (
			<>
				{channel.guildId && (
					<MenuGroup>
						<MarkChannelAsReadMenuItem channel={channel} onClose={onClose} />
						<InvitePeopleToChannelMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>
				)}

				<MenuGroup>
					<MenuItem icon={<PencilSimpleIcon />} onClick={handleSetNickname}>
						{t`Change Nickname`}
					</MenuItem>
					{channel.guildId && (
						<MenuItem icon={<ArrowSquareOutIcon />} onClick={handleOpenInGuild}>
							{t`Open in Community`}
						</MenuItem>
					)}
					{(favoriteChannel.parentId !== null ||
						FavoritesStore.sortedCategories.some((category) => category.id !== favoriteChannel.parentId)) && (
						<MenuItemSubmenu
							label={t`Move to`}
							icon={<ArrowsOutCardinalIcon />}
							render={() => (
								<MenuGroup>
									{favoriteChannel.parentId !== null && (
										<MenuItem onClick={() => handleMoveTo(null)}>{t`Uncategorized`}</MenuItem>
									)}
									{FavoritesStore.sortedCategories
										.filter((category) => category.id !== favoriteChannel.parentId)
										.map((category) => (
											<MenuItem key={category.id} onClick={() => handleMoveTo(category.id)}>
												{category.name}
											</MenuItem>
										))}
								</MenuGroup>
							)}
						/>
					)}
				</MenuGroup>

				{channel.guildId && (
					<MenuGroup>
						<MuteChannelMenuItem channel={channel} onClose={onClose} />
						<ChannelNotificationSettingsMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>
				)}

				{canManageChannel && (
					<MenuGroup>
						<EditChannelMenuItem channel={channel} onClose={onClose} />
					</MenuGroup>
				)}

				<MenuGroup>
					<MenuItem icon={<StarIcon weight="fill" />} onClick={handleRemoveFromFavorites} danger>
						{t`Remove from Favorites`}
					</MenuItem>
				</MenuGroup>
			</>
		);
	},
);
