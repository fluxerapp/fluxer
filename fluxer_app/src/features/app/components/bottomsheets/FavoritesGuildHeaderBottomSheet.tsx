// SPDX-License-Identifier: AGPL-3.0-or-later

import sharedStyles from '@app/features/app/components/bottomsheets/shared.module.css';
import {AddFavoriteChannelModal} from '@app/features/expressions/components/modals/AddFavoriteChannelModal';
import {CreateFavoriteCategoryModal} from '@app/features/expressions/components/modals/CreateFavoriteCategoryModal';
import {
	ADD_CHANNEL_DESCRIPTOR,
	CREATE_CATEGORY_DESCRIPTOR,
	HIDE_FAVORITES_DESCRIPTOR,
	HIDE_MUTED_CHANNELS_DESCRIPTOR,
	MUTE_FAVORITES_DESCRIPTOR,
	UNMUTE_FAVORITES_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as FavoritesCommands from '@app/features/messaging/commands/FavoritesCommands';
import Favorites from '@app/features/messaging/state/Favorites';
import {CreateCategoryIcon, CreateChannelIcon, HideIcon, MuteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import * as UserGuildSettingsCommands from '@app/features/user/commands/UserGuildSettingsCommands';
import UserGuildSettings from '@app/features/user/state/UserGuildSettings';
import {FAVORITES_GUILD_ID} from '@fluxer/constants/src/AppConstants';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

interface FavoritesGuildHeaderBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const FavoritesGuildHeaderBottomSheet: React.FC<FavoritesGuildHeaderBottomSheetProps> = observer(
	({isOpen, onClose}) => {
		const {i18n} = useLingui();
		const hideMutedChannels = Favorites.hideMutedChannels;
		const settings = UserGuildSettings.getSettingsForScope(FAVORITES_GUILD_ID);
		const isMuted = settings?.muted ?? false;
		const handleAddChannel = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<AddFavoriteChannelModal data-flx="app.favorites-guild-header-bottom-sheet.handle-add-channel.add-favorite-channel-modal" />
				)),
			);
		};
		const handleCreateCategory = () => {
			ModalCommands.pushAfterBottomSheetClose(
				onClose,
				modal(() => (
					<CreateFavoriteCategoryModal data-flx="app.favorites-guild-header-bottom-sheet.handle-create-category.create-favorite-category-modal" />
				)),
			);
		};
		const handleToggleHideMutedChannels = (checked: boolean) => {
			Favorites.setHideMutedChannels(checked);
		};
		const handleToggleMuteFavorites = () => {
			UserGuildSettingsCommands.updateGuildSettings(FAVORITES_GUILD_ID, {muted: !isMuted});
			onClose();
		};
		const handleHideFavorites = () => {
			ModalCommands.runAfterBottomSheetClose(onClose, () => FavoritesCommands.confirmHideFavorites(undefined, i18n));
		};
		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: (
							<CreateChannelIcon
								className={sharedStyles.icon}
								data-flx="app.favorites-guild-header-bottom-sheet.create-channel-icon"
							/>
						),
						label: i18n._(ADD_CHANNEL_DESCRIPTOR),
						onClick: handleAddChannel,
					},
					{
						icon: (
							<CreateCategoryIcon
								className={sharedStyles.icon}
								data-flx="app.favorites-guild-header-bottom-sheet.create-category-icon"
							/>
						),
						label: i18n._(CREATE_CATEGORY_DESCRIPTOR),
						onClick: handleCreateCategory,
					},
				],
			},
			{
				items: [
					{
						icon: (
							<MuteIcon className={sharedStyles.icon} data-flx="app.favorites-guild-header-bottom-sheet.mute-icon" />
						),
						label: isMuted ? i18n._(UNMUTE_FAVORITES_DESCRIPTOR) : i18n._(MUTE_FAVORITES_DESCRIPTOR),
						onClick: handleToggleMuteFavorites,
					},
					{
						label: i18n._(HIDE_MUTED_CHANNELS_DESCRIPTOR),
						checked: hideMutedChannels,
						onChange: handleToggleHideMutedChannels,
					},
				],
			},
			{
				items: [
					{
						icon: (
							<HideIcon className={sharedStyles.icon} data-flx="app.favorites-guild-header-bottom-sheet.hide-icon" />
						),
						label: i18n._(HIDE_FAVORITES_DESCRIPTOR),
						onClick: handleHideFavorites,
						danger: true,
					},
				],
			},
		];
		return (
			<MenuBottomSheet
				isOpen={isOpen}
				onClose={onClose}
				groups={menuGroups}
				data-flx="app.favorites-guild-header-bottom-sheet.menu-bottom-sheet"
			/>
		);
	},
);
