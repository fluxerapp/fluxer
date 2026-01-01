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
import {FolderPlusIcon, PlusCircleIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import {AddFavoriteChannelModal} from '~/components/modals/AddFavoriteChannelModal';
import {CreateFavoriteCategoryModal} from '~/components/modals/CreateFavoriteCategoryModal';
import FavoritesStore from '~/stores/FavoritesStore';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';
import {MenuItemCheckbox} from './MenuItemCheckbox';

interface FavoritesChannelListContextMenuProps {
	onClose: () => void;
}

export const FavoritesChannelListContextMenu: React.FC<FavoritesChannelListContextMenuProps> = observer(({onClose}) => {
	const {t} = useLingui();
	const hideMutedChannels = FavoritesStore.hideMutedChannels;

	const handleToggleHideMutedChannels = React.useCallback((checked: boolean) => {
		FavoritesStore.setHideMutedChannels(checked);
	}, []);

	const handleAddChannel = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <AddFavoriteChannelModal />));
	}, [onClose]);

	const handleCreateCategory = React.useCallback(() => {
		onClose();
		ModalActionCreators.push(modal(() => <CreateFavoriteCategoryModal />));
	}, [onClose]);

	return (
		<>
			<MenuGroup>
				<MenuItemCheckbox checked={hideMutedChannels} onChange={handleToggleHideMutedChannels}>
					{t`Hide Muted Channels`}
				</MenuItemCheckbox>
			</MenuGroup>

			<MenuGroup>
				<MenuItem icon={<PlusCircleIcon style={{width: 16, height: 16}} />} onClick={handleAddChannel}>
					{t`Add Channel`}
				</MenuItem>
				<MenuItem icon={<FolderPlusIcon style={{width: 16, height: 16}} />} onClick={handleCreateCategory}>
					{t`Create Category`}
				</MenuItem>
			</MenuGroup>
		</>
	);
});
