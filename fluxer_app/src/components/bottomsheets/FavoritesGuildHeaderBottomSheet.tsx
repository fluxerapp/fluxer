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
import {BellIcon, BellSlashIcon, EyeSlashIcon, FolderPlusIcon, PlusCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as FavoritesActionCreators from '~/actions/FavoritesActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserGuildSettingsActionCreators from '~/actions/UserGuildSettingsActionCreators';
import {FAVORITES_GUILD_ID} from '~/Constants';
import {AddFavoriteChannelModal} from '~/components/modals/AddFavoriteChannelModal';
import {CreateFavoriteCategoryModal} from '~/components/modals/CreateFavoriteCategoryModal';
import type {MenuGroupType} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import {MenuBottomSheet} from '~/components/uikit/MenuBottomSheet/MenuBottomSheet';
import FavoritesStore from '~/stores/FavoritesStore';
import UserGuildSettingsStore from '~/stores/UserGuildSettingsStore';
import sharedStyles from './shared.module.css';

interface FavoritesGuildHeaderBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
}

export const FavoritesGuildHeaderBottomSheet: React.FC<FavoritesGuildHeaderBottomSheetProps> = observer(
	({isOpen, onClose}) => {
		const {t, i18n} = useLingui();
		const hideMutedChannels = FavoritesStore.hideMutedChannels;
		const settings = UserGuildSettingsStore.getSettings(FAVORITES_GUILD_ID);
		const isMuted = settings?.muted ?? false;

		const handleAddChannel = () => {
			onClose();
			ModalActionCreators.push(modal(() => <AddFavoriteChannelModal />));
		};

		const handleCreateCategory = () => {
			onClose();
			ModalActionCreators.push(modal(() => <CreateFavoriteCategoryModal />));
		};

		const handleToggleHideMutedChannels = (checked: boolean) => {
			FavoritesStore.setHideMutedChannels(checked);
		};

		const handleToggleMuteFavorites = () => {
			UserGuildSettingsActionCreators.updateGuildSettings(FAVORITES_GUILD_ID, {muted: !isMuted});
			onClose();
		};

		const handleHideFavorites = () => {
			onClose();
			FavoritesActionCreators.confirmHideFavorites(undefined, i18n);
		};

		const menuGroups: Array<MenuGroupType> = [
			{
				items: [
					{
						icon: <PlusCircleIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Add Channel`,
						onClick: handleAddChannel,
					},
					{
						icon: <FolderPlusIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Create Category`,
						onClick: handleCreateCategory,
					},
				],
			},
			{
				items: [
					{
						icon: isMuted ? (
							<BellIcon weight="fill" className={sharedStyles.icon} />
						) : (
							<BellSlashIcon weight="fill" className={sharedStyles.icon} />
						),
						label: isMuted ? t`Unmute Favorites` : t`Mute Favorites`,
						onClick: handleToggleMuteFavorites,
					},
					{
						label: t`Hide Muted Channels`,
						checked: hideMutedChannels,
						onChange: handleToggleHideMutedChannels,
					},
				],
			},
			{
				items: [
					{
						icon: <EyeSlashIcon weight="fill" className={sharedStyles.icon} />,
						label: t`Hide Favorites`,
						onClick: handleHideFavorites,
						danger: true,
					},
				],
			},
		];

		return <MenuBottomSheet isOpen={isOpen} onClose={onClose} groups={menuGroups} />;
	},
);
